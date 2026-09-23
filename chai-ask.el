;;; chai-ask.el --- A standalone answer from the Chai Library -*- lexical-binding: t; -*-

;; Author: Yibie <yibie@outlook.com>
;; Keywords: outlines, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Answers a question from Library passages, with citations that jump back
;; into the books they came from.
;;
;; This is the path for someone who has no chat client in Emacs: it talks to
;; one local server over curl and renders the answer in a buffer of its own.
;; If you already run a chat client, prefer wiring Chai into it — `chai-superchat.el'
;; does that for superchat — because a chat client already has model switching,
;; several providers, sessions and tools, and this file will never have any of
;; them.
;;
;; Everything that makes an answer trustworthy — numbering the passages,
;; budgeting them, validating the citations, jumping to the source — lives in
;; `chai-context.el' and is shared by both paths.  What remains here is only
;; the request and the buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chai-context)

;;; Customization

(defgroup chai-ask nil
  "A standalone answer from the Chai Library."
  :group 'chai)

(defcustom chai-ask-endpoint "http://localhost:11434"
  "Base URL of the OpenAI-compatible server that generates answers."
  :type 'string
  :group 'chai-ask)

(defcustom chai-ask-model "qwen3.5:9b"
  "Name of the model used to write answers."
  :type 'string
  :group 'chai-ask)

(defcustom chai-ask-think nil
  "Whether the model may reason at length before answering.

Off by default.  A reasoning model spends the answer budget thinking before it
emits a single word, so the buffer sits empty, and the reasoning gains nothing
here: the passages are already in front of it."
  :type 'boolean
  :group 'chai-ask)

(defcustom chai-ask-max-tokens 800
  "Greatest length of a generated answer, in tokens."
  :type 'integer
  :group 'chai-ask)

(defcustom chai-ask-temperature 0.2
  "Sampling temperature.
Low, because the task is to report what the passages say rather than to
compose freely."
  :type 'number
  :group 'chai-ask)

(defcustom chai-ask-system-prompt
  "你是用户阅读库的助手。只依据给出的段落回答，不要使用段落之外的知识。\n\
每一个论断后面都要用 [1]、[2] 这样的编号标注它出自哪一段。\n\
如果给出的段落不足以回答，就直接说明不足，不要推测。\n\
用提问所用的语言作答，简洁、具体。"
  "Instructions given to the model before the passages and the question."
  :type 'string
  :group 'chai-ask)

;;; The request

(defvar chai-ask--process nil
  "Generation currently in flight, or nil.")

(defvar-local chai-ask--partial ""
  "Incomplete response line received so far.")

(defvar-local chai-ask--passages nil
  "Passages given to the model for the answer in this buffer.")

(defvar-local chai-ask--answer-start nil
  "Where the generated answer begins in this buffer.")

(defvar-local chai-ask--thinking-noted nil
  "Whether this buffer has already said the model is thinking.")

(defun chai-ask--prompt (query passages)
  "Return the user message asking QUERY over PASSAGES."
  (concat (chai-context-render passages)
          "\n以上是全部材料。请只依据它们回答下面的问题，并用 [编号] 标注出处。\n\n"
          "问题：" query))

(defun chai-ask--request-body (prompt)
  "Return the JSON request asking the model to answer PROMPT."
  (json-serialize
   (list :model chai-ask-model
         :stream t
         :think (if chai-ask-think t :false)
         :messages (vector (list :role "system" :content chai-ask-system-prompt)
                           (list :role "user" :content prompt))
         :options (list :temperature chai-ask-temperature
                        :num_predict chai-ask-max-tokens))))

(defun chai-ask--insert (buffer text &optional face)
  "Append TEXT to BUFFER, propertized with FACE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert (if face (propertize text 'face face) text))))))

(defun chai-ask--handle-chunk (buffer line)
  "Render the fragment carried by LINE into BUFFER."
  (when (buffer-live-p buffer)
    (if (not (string-prefix-p "{" line))
        ;; Curl writes its failures on stderr, which arrives here.
        (unless (string-blank-p line)
          (chai-ask--insert buffer (concat line "\n") 'error))
      (when-let* ((object (ignore-errors (json-parse-string line :object-type 'plist))))
        (cond
         ((plist-get object :error)
          (chai-ask--insert buffer (format "\n%s\n" (plist-get object :error)) 'error))
         (t
          (let* ((message-part (plist-get object :message))
                 (content (and message-part (plist-get message-part :content)))
                 (thinking (and message-part (plist-get message-part :thinking))))
            ;; A model that ignores `think: false' would otherwise leave the
            ;; buffer empty for as long as it reasons; say so instead.
            (when (and thinking (not (string-empty-p thinking)))
              (with-current-buffer buffer
                (unless chai-ask--thinking-noted
                  (setq chai-ask--thinking-noted t)
                  (chai-ask--insert buffer "（模型正在思考…）\n" 'chai-search-meta))))
            (when (and content (not (string-empty-p content)))
              (chai-ask--insert buffer content)))))))))

(defun chai-ask--filter (process string)
  "Split STRING from PROCESS into lines and render each."
  (let ((buffer (process-get process 'chai-buffer)))
    (with-current-buffer buffer
      (setq chai-ask--partial (concat chai-ask--partial string)))
    (let* ((pending (with-current-buffer buffer chai-ask--partial))
           (lines (split-string pending "\n")))
      (with-current-buffer buffer
        (setq chai-ask--partial (car (last lines))))
      (dolist (line (butlast lines))
        (chai-ask--handle-chunk buffer (string-trim line))))))

(defun chai-ask--sentinel (process _event)
  "Finish the answer in PROCESS's buffer once generation ends."
  (unless (process-live-p process)
    (setq chai-ask--process nil)
    (let ((buffer (process-get process 'chai-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (chai-ask--finish))))))

;;; The buffer

(defvar-keymap chai-ask-mode-map
  :doc "Keymap for `chai-ask-mode'."
  "RET" #'push-button
  "n"   #'forward-button
  "p"   #'backward-button
  "g"   #'chai-ask-again)

(define-derived-mode chai-ask-mode special-mode "Chai Answer"
  "Major mode for the Chai answer buffer."
  (setq-local truncate-lines nil))

(defconst chai-ask-buffer-name "*Chai Answer*")

(defvar chai-ask--last-query nil
  "Question that produced the current answer buffer.")

(defun chai-ask--insert-sources ()
  "List the passages the answer actually cited."
  (let* ((answer (buffer-substring-no-properties
                  (or chai-ask--answer-start (point-min)) (point-max)))
         (sources (chai-context-sources chai-ask--passages answer))
         (inhibit-read-only t))
    (goto-char (point-max))
    (insert "\n\n" (propertize "── 引用来源 ──\n" 'face 'chai-search-meta))
    (if (null sources)
        (insert (propertize "回答没有标注任何来源，请谨慎对待。\n" 'face 'warning))
      (pcase-dolist (`(,number ,hit ,name) sources)
        (insert (propertize (format "[%d] " number) 'face 'chai-search-meta))
        (insert-button name :type 'chai-context-citation 'chai-hit hit)
        (insert "\n")))))

(defun chai-ask--finish ()
  "Wrap up the answer: link its citations and list its sources."
  (chai-context-linkify (or chai-ask--answer-start (point-min)) (point-max)
                        chai-ask--passages)
  (chai-ask--insert-sources)
  (goto-char (point-min)))

(defun chai-ask--start (query passages)
  "Ask the model QUERY over PASSAGES and stream the answer into a buffer."
  (let ((buffer (get-buffer-create chai-ask-buffer-name))
        (prompt (chai-ask--prompt query passages)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (chai-ask-mode)
        (setq chai-ask--passages passages
              chai-ask--partial ""
              chai-ask--thinking-noted nil)
        (insert (propertize (concat "问：" query "\n\n") 'face 'chai-search-title))
        (insert (propertize (format "依据 %d 段材料，%s 正在作答…\n\n"
                                    (length passages) chai-ask-model)
                            'face 'chai-search-meta))
        (setq chai-ask--answer-start (point-marker))))
    (pop-to-buffer buffer)
    (setq chai-ask--process
          (make-process
           :name "chai-ask"
           :buffer nil
           :noquery t
           :connection-type 'pipe
           :command (list chai-curl-program "-sS" "--no-buffer"
                          ;; localhost must not be routed through a system proxy
                          "--noproxy" "*"
                          "-H" "Content-Type: application/json"
                          ;; The body arrives on stdin: a prompt carrying several
                          ;; passages outgrows the limit on a command line.
                          "--data-binary" "@-"
                          (concat chai-ask-endpoint "/api/chat"))
           :filter #'chai-ask--filter
           :sentinel #'chai-ask--sentinel))
    (process-put chai-ask--process 'chai-buffer buffer)
    (process-send-string chai-ask--process (chai-ask--request-body prompt))
    (process-send-eof chai-ask--process)
    buffer))

;;;###autoload
(defun chai-ask (query &optional filters)
  "Answer QUERY from the Chai Library, citing the passages used.
FILTERS is the optional plist documented in `chai-search-query'."
  (interactive "sChai ask: ")
  (when (process-live-p chai-ask--process)
    (user-error "An answer is already being generated"))
  (setq chai-ask--last-query query)
  (if-let* ((passages (chai-context-for query filters)))
      (chai-ask--start query passages)
    ;; No material means no answer: the alternative is a model answering from
    ;; its own memory, which is not what a Library search is for.
    (message "Chai: 没有检索到相关段落，无法作答。试试换个说法，或先运行 `chai-index-rebuild'")))

;;;###autoload
(defun chai-ask-again ()
  "Ask the last question again."
  (interactive)
  (unless chai-ask--last-query (user-error "No previous question"))
  (chai-ask chai-ask--last-query))

(provide 'chai-ask)

;;; chai-ask.el ends here
