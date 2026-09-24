;;; chai-superchat.el --- Chai as superchat's reading library -*- lexical-binding: t; -*-

;; Author: Yibie <yibie@outlook.com>
;; Keywords: outlines, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Lets superchat answer from your Chai Library: every turn is searched
;; against your books, the passages found are attached to the prompt, and the
;; citations the model writes come back as jumps into the source.
;;
;; The division is by what each side knows.  Choosing a model, streaming,
;; sessions, memory and tools are superchat's, and it does all of them better
;; than a reading tool ever would.  Knowing that `[3]' means a particular
;; character position in a particular Org file, under a particular chapter, is
;; Chai's, and superchat has no way to know it.  So this file is thin on
;; purpose: it moves passages one way and citations the other.
;;
;; Loading it does nothing until superchat is present, so it is safe to
;; require unconditionally.
;;
;; Turn it on with `chai-superchat-mode'.

;;; Code:

(require 'cl-lib)
;; `cl-struct-slot-value' lives here and is not autoloaded by `cl-lib'.
(require 'cl-macs)
(require 'subr-x)
(require 'chai-context)

(declare-function superchat-turn-clean-input "superchat-core" (turn))
(declare-function superchat-turn-llm-result "superchat-core" (turn))

;; The turn is a `cl-defstruct', and superchat is not loaded when this file is
;; compiled.  The slot is therefore reached through its offset, looked up at
;; run time: `setf' on the generated accessor compiles into a call to a setter
;; function that does not exist, and the `cl-struct-slot-value' setter wants to
;; know the type while compiling.  Plain `aref' and `aset' want neither.

(defun chai-superchat--slot (turn slot)
  "Return SLOT of TURN."
  (aref turn (cl-struct-slot-offset 'superchat-turn slot)))

(defun chai-superchat--set-slot (turn slot value)
  "Set SLOT of TURN to VALUE."
  (aset turn (cl-struct-slot-offset 'superchat-turn slot) value))

(defun chai-superchat--prompt (turn)
  "Return the prompt TURN currently carries."
  (chai-superchat--slot turn 'prompt))


(defun chai-superchat--set-prompt (turn value)
  "Set TURN's prompt to VALUE."
  (chai-superchat--set-slot turn 'prompt value))

(defgroup chai-superchat nil
  "Chai as superchat's reading library."
  :group 'chai)

(defcustom chai-superchat-instruction
  "Answer only from the passages above, citing each claim with its [number]; \
if they are not enough to answer, say so plainly instead of guessing."
  "Line appended after the passages, telling the model how to use them."
  :type 'string
  :group 'chai-superchat)

(defcustom chai-superchat-scope 'session
  "Which superchat turns are answered from the Chai Library.

`session' — only conversations you have marked with `chai-superchat-cowork'.
This is the default because superchat is a general chat client: attaching
chapters of your books to a question about a git diff helps nobody.

`always' — every turn.  Reasonable only if you use superchat for reading and
little else."
  :type '(choice (const :tag "Only sessions marked for reading" session)
                 (const :tag "Every turn" always))
  :group 'chai-superchat)

(defcustom chai-superchat-command "chai"
  "Slash command that answers one question from the Library.

Typing =/chai what is a consensus algorithm= consults the books for that turn only, without
marking the conversation.  The answer stays in the conversation, so it can be
followed up on like any other."
  :type 'string
  :group 'chai-superchat)

(defcustom chai-superchat-minimum-length 4
  "Shortest input worth searching the Library for.
Below this a turn is almost always an aside — \"thanks\", \"go on\" — and
attaching a chapter of a book to it only crowds the conversation."
  :type 'integer
  :group 'chai-superchat)

(defvar superchat-build-prompt-functions nil)
(defvar superchat-post-turn-functions nil)
(defvar superchat-command-hooks nil)

(defvar chai-superchat--sessions (make-hash-table :test 'equal)
  "Session ids whose turns are answered from the Library.")

(defvar chai-superchat--passages nil
  "Passages attached to the turn now in flight.
Held here rather than only on the turn because the buffer that renders the
answer is reached separately from the turn that produced it.")
(defun chai-superchat--reading-p (turn)
  "Return non-nil when TURN belongs to a conversation about the Library."
  (or (eq chai-superchat-scope 'always)
      (when-let* ((session (ignore-errors (chai-superchat--slot turn 'session-id))))
        (gethash session chai-superchat--sessions))))

;;; Sending passages in

(defun chai-superchat-attach (turn)
  "Attach Library passages relevant to TURN's input to its prompt.

Only for a conversation marked with `chai-superchat-cowork' — see
`chai-superchat-scope'.  Returns TURN unchanged otherwise, and also when the
Library has nothing to offer: a turn that is not about the books should reach
the model exactly as the user wrote it."
  (let ((query (string-trim (or (superchat-turn-clean-input turn) ""))))
    (setq chai-superchat--passages nil)
    (when (and (chai-superchat--reading-p turn)
               (>= (length query) chai-superchat-minimum-length))
      (when-let* ((passages (ignore-errors (chai-context-for query))))
        (setq chai-superchat--passages passages)
        (chai-superchat--set-prompt
         turn
         (concat (chai-context-render passages)
                 "\n" chai-superchat-instruction "\n\n"
                 (chai-superchat--prompt turn)))))
    turn))

;;; Asking once, without marking the conversation

(defun chai-superchat-command (command args input _lang _target-model)
  "Answer one question from the Library when COMMAND is the Chai command.

ARGS is the rest of the line, INPUT the whole of it.  Returns a request for
superchat to run, so the answer arrives in the conversation with everything
superchat gives it — streaming, history, follow-up questions.  Returns nil for
any other command, leaving the chain to whoever handles it."
  (when (equal command chai-superchat-command)
    (let ((question (string-trim (if (string-blank-p (or args "")) (or input "") args))))
      (setq chai-superchat--passages nil)
      (cond
       ((string-empty-p question)
        `(:type :echo :content ,(format "Usage: /%s your question" chai-superchat-command)))
       ((not (setq chai-superchat--passages (ignore-errors (chai-context-for question))))
        ;; Saying nothing was found is the honest answer; sending the question
        ;; on without material would only get the model's own recollection.
        `(:type :echo :content "Chai: no passages found"))
       (t
        `(:type :llm-query
          :prompt ,(concat (chai-context-render chai-superchat--passages)
                           "\n" chai-superchat-instruction "\n\n"
                           question)
          :user-message ,question))))))

;;; Bringing citations back

(defun chai-superchat--answer-buffer ()
  "Return the buffer superchat renders answers into, or nil."
  (when (boundp 'superchat-buffer-name)
    (get-buffer (symbol-value 'superchat-buffer-name))))

(defun chai-superchat-linkify (turn)
  "Turn the citations in TURN's answer into jumps into the Library."
  (when-let* ((passages chai-superchat--passages)
              (buffer (chai-superchat--answer-buffer))
              (answer (superchat-turn-llm-result turn)))
    (with-current-buffer buffer
      (save-excursion
        ;; The answer is the last thing written, so search back only as far as
        ;; it could reach rather than over the whole conversation.
        (let ((from (max (point-min) (- (point-max) (* 2 (length answer))))))
          (chai-context-linkify from (point-max) passages)))))
  turn)

;;; Turning it on

;;;###autoload
(define-minor-mode chai-superchat-mode
  "Wire Chai into superchat.

Adds two ways to reach the Library and one way back.  `/chai QUESTION' answers
a single question from your books; `chai-superchat-cowork' marks a whole
conversation as being about them.  Either way the citations in the reply become
jumps into the source.

Turning this off leaves superchat exactly as it was."
  :global t
  :group 'chai-superchat
  (if chai-superchat-mode
      (progn
        (add-hook 'superchat-build-prompt-functions #'chai-superchat-attach)
        (add-hook 'superchat-post-turn-functions #'chai-superchat-linkify)
        (add-hook 'superchat-command-hooks #'chai-superchat-command))
    (remove-hook 'superchat-build-prompt-functions #'chai-superchat-attach)
    (remove-hook 'superchat-post-turn-functions #'chai-superchat-linkify)
    (remove-hook 'superchat-command-hooks #'chai-superchat-command)))

;;; Marking a conversation as being about the Library

;;;###autoload
(defun chai-superchat-cowork (&optional off)
  "Answer this superchat conversation from the Chai Library.

Every turn of this conversation is then searched against your books, and what
is found is attached to the question.  Other conversations are untouched, so
superchat stays a general chat client.  With a prefix argument, or when OFF is
non-nil, stop reading the Library here.

For a single question without changing the conversation, use `chai-ask'."
  (interactive "P")
  (let ((session (and (boundp 'superchat--session-id)
                      (symbol-value 'superchat--session-id))))
    (unless (and (stringp session) (not (string-empty-p session)))
      (user-error "Not in a superchat conversation"))
    (unless chai-superchat-mode (chai-superchat-mode 1))
    (if off
        (progn (remhash session chai-superchat--sessions)
               (message "Chai: this conversation no longer reads the Library"))
      (puthash session t chai-superchat--sessions)
      (message "Chai: this conversation now reads the Library (%s to stop)"
               (substitute-command-keys "\\[universal-argument] \\[chai-superchat-cowork]")))))


;; Enabled on load.  `chai.el' arranges for this file to be loaded when
;; superchat is, so reaching here means superchat is present and the hooks
;; belong in place; `chai-superchat-mode' turns them off again.
(unless (bound-and-true-p chai-superchat-mode)
  (chai-superchat-mode 1))

(provide 'chai-superchat)

;;; chai-superchat.el ends here
