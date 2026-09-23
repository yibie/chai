;;; chai-vector.el --- Semantic recall for the Chai Library -*- lexical-binding: t; -*-

;; Author: Yibie <yibie@outlook.com>
;; Keywords: outlines, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Adds a semantic recall channel: passages found by meaning rather than by
;; the words they contain.  It is entirely optional — without it Chai searches
;; exactly as before, and `chai-search--fuse' needs no branch for its absence
;; because an empty channel contributes nothing.
;;
;; Three measurements shaped this file, taken on a real 2788-book library:
;;
;; - Embedding every passage costs 13 hours and 0.6s per query, because
;;   sqlite-vec scans linearly.  Embedding every *section* costs about three
;;   hours and 0.15s, and loses nothing: the passages a semantic search wins
;;   on are chapter-topic matches, which is what a section already is.
;;
;; - `int8' quantisation keeps 98-100% of the float32 ranking at a quarter of
;;   the size.  There is no reason to store raw floats.  Binary quantisation
;;   keeps only 55-61%, which is not a ranking at all.
;;
;; - A vector search always returns k results, however irrelevant.  Asked for
;;   a term the library has never seen, it answers with noise while BM25
;;   honestly returns nothing.  So this channel may only add candidates; the
;;   lexical channel remains the authority on whether anything matched.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chai-index)

;;; Customization

(defgroup chai-vector nil
  "Semantic recall for the Chai Library."
  :group 'chai)

(defcustom chai-vector-extension nil
  "Path to the sqlite-vec loadable extension, or nil to look for it.

The file must be named `vec0' with the platform's library suffix: Emacs only
loads SQLite extensions from a fixed allowlist of names, and a differently
named copy of the same library is refused."
  :type '(choice (const :tag "Search the usual places" nil) file)
  :group 'chai-vector)

(defcustom chai-vector-endpoint "http://localhost:11434"
  "Base URL of the server that turns text into vectors."
  :type 'string
  :group 'chai-vector)

(defcustom chai-vector-model "bge-m3"
  "Embedding model.

The default reads a whole section without truncating it, which a model with a
short window cannot do: a section runs to several thousand characters, and the
tail of an over-long input is silently dropped rather than reported."
  :type 'string
  :group 'chai-vector)

(defcustom chai-vector-request-tokens 7000
  "Token budget for one embedding request.

The server applies its context window to the whole request rather than to each
input, so a batch is filled by estimated size, not by count: sixteen long
sections in one call is tens of thousands of tokens and is simply refused."
  :type 'integer
  :group 'chai-vector)

(defcustom chai-vector-section-chars 1500
  "How much of a section is embedded.

Only the opening, which is where a section says what it is about.  A single
vector standing for many thousands of characters averages its way into
vagueness, and long inputs also shrink the batch the server will accept."
  :type 'integer
  :group 'chai-vector)

(defcustom chai-vector-recall 30
  "How many sections the semantic channel recalls for one query."
  :type 'integer
  :group 'chai-vector)

(defcustom chai-vector-chunks-per-section 3
  "How many passages of a recalled section enter the fused ranking.
Without a bound one long section would fill the results by itself."
  :type 'integer
  :group 'chai-vector)

;;; Extension

(defconst chai-vector--extension-name
  (concat "vec0" (or (and (eq system-type 'darwin) ".dylib")
                     (and (memq system-type '(windows-nt cygwin)) ".dll")
                     ".so"))
  "File name Emacs will accept for the sqlite-vec extension.")

(defvar chai-vector--extension-cache 'unset
  "Resolved path of the sqlite-vec extension, nil when absent.")

(defun chai-vector--locate-extension ()
  "Return the path of a readable sqlite-vec extension, or nil.

A configured path that is not there returns nil rather than itself: reporting
the extension as missing is clearer than handing SQLite a name it will refuse
with an error about modules."
  (or (and chai-vector-extension
           (file-readable-p chai-vector-extension)
           chai-vector-extension)
      (seq-find #'file-readable-p
                (mapcar (lambda (dir) (expand-file-name chai-vector--extension-name dir))
                        (list (expand-file-name "chai/" user-emacs-directory)
                              (file-name-directory (or load-file-name buffer-file-name "./"))
                              "/usr/local/lib/" "/opt/homebrew/lib/")))))

(defun chai-vector-extension-path ()
  "Return the sqlite-vec extension path, resolving it once."
  (when (eq chai-vector--extension-cache 'unset)
    (setq chai-vector--extension-cache (chai-vector--locate-extension)))
  chai-vector--extension-cache)

(defun chai-vector--load-extension (db)
  "Load sqlite-vec into DB.  Return non-nil on success."
  (when-let* ((path (chai-vector-extension-path)))
    (condition-case nil
        (progn (sqlite-load-extension db path) t)
      (error nil))))

(defvar chai-vector--enabled nil
  "Whether the current index connection has sqlite-vec loaded.")

(defun chai-vector-enable (db)
  "Make DB able to answer vector queries.  Return non-nil on success."
  (setq chai-vector--enabled (chai-vector--load-extension db)))

(defun chai-vector-available-p (&optional db)
  "Return non-nil when semantic recall can actually answer a query on DB.
That means the extension loads, the table exists, and something is in it."
  (when-let* ((db (or db (ignore-errors (chai-index--db)))))
    (and (or chai-vector--enabled (chai-vector-enable db))
         (let ((count (ignore-errors
                        (caar (sqlite-select db "SELECT count(*) FROM vec_sections")))))
           (and count (> count 0))))))

;;; Embedding

(defun chai-vector--curl-embed (texts)
  "Return the vectors of TEXTS, or nil when the endpoint cannot be reached.

The request goes in on standard input rather than as an argument: a batch of
sections runs to hundreds of kilobytes, well past the limit on the size of a
command line, and the call would fail before curl ever started."
  (let ((request (json-serialize (list :model chai-vector-model
                                       :input (vconcat texts))))
        (response (generate-new-buffer " *chai-embed*")))
    (unwind-protect
        (with-temp-buffer
          (insert request)
          (let ((status (call-process-region
                         (point-min) (point-max) chai-curl-program
                         nil response nil
                         "-sS" "--noproxy" "*"
                         "-H" "Content-Type: application/json"
                         "--data-binary" "@-"
                         (concat chai-vector-endpoint "/api/embed"))))
            (when (eq status 0)
              (with-current-buffer response
                (goto-char (point-min))
                (when-let* ((object (ignore-errors
                                      (json-parse-buffer :object-type 'plist
                                                         :array-type 'list))))
                  (plist-get object :embeddings))))))
      (kill-buffer response))))

(defun chai-vector-embed (texts)
  "Return one vector per element of TEXTS."
  (chai-vector--curl-embed texts))

(defun chai-vector--json (vector)
  "Return VECTOR as the JSON array sqlite-vec accepts."
  (concat "[" (mapconcat (lambda (x) (format "%.6g" x)) vector ",") "]"))

;;; Storage

(defun chai-vector--ensure-table (db dimension)
  "Make sure DB holds a vector table of DIMENSION, rebuilding a stale one.

The dimension is fixed when the table is created, so an index built with one
model cannot be queried with another; rebuilding is the only correct response
to that change, and is far better than silently comparing incompatible
vectors."
  (let* ((existing (caar (sqlite-select db "SELECT sql FROM sqlite_master
                                            WHERE name = 'vec_sections'")))
         (expected (format "int8[%d]" dimension)))
    (when (and existing (not (string-search expected existing)))
      (sqlite-execute db "DROP TABLE vec_sections")
      (setq existing nil))
    (unless existing
      (sqlite-execute db (format "CREATE VIRTUAL TABLE vec_sections USING vec0(
                                    section_id INTEGER PRIMARY KEY,
                                    embedding int8[%d] distance_metric=cosine)"
                                 dimension)))))

(defun chai-vector--section-text (db section)
  "Return the text embedded for SECTION: its heading path and its opening."
  (pcase-let ((`(,path ,beg ,end ,outline)
               (car (sqlite-select db "SELECT d.path, s.beg_pos, s.end_pos, s.outline
                                       FROM sections s JOIN documents d ON d.id = s.doc
                                       WHERE s.id = ?" (list section)))))
    (when (and path (file-readable-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (let* ((from (min (max (point-min) beg) (point-max)))
               (to (min (max from end) (point-max)))
               (body (chai-index--clean-text (buffer-substring-no-properties from to)))
               (body (if (> (length body) chai-vector-section-chars)
                         (substring body 0 chai-vector-section-chars)
                       body)))
          ;; The heading path is part of what a section is about, and costs
          ;; almost nothing to include.
          (string-trim (concat (or outline "") "\n" body)))))))

(defun chai-vector--batches (db ids)
  "Split IDS into (IDS . TEXTS) batches that fit one request."
  (let ((batches nil) (batch nil) (texts nil) (budget chai-vector-request-tokens))
    (dolist (id ids)
      (let* ((text (or (chai-vector--section-text db id) ""))
             (cost (max 1 (chai-estimate-tokens text))))
        (when (and batch (> cost budget))
          (push (cons (nreverse batch) (nreverse texts)) batches)
          (setq batch nil texts nil budget chai-vector-request-tokens))
        (push id batch)
        (push text texts)
        (setq budget (- budget cost))))
    (when batch (push (cons (nreverse batch) (nreverse texts)) batches))
    (nreverse batches)))

(defun chai-vector--pending (db limit skip)
  "Return up to LIMIT ids of sections with no vector yet, ignoring SKIP.
SKIP holds the sections this run already tried and could not embed; they are
left without a vector so that a later run retries them."
  (seq-take
   (seq-remove (lambda (id) (gethash id skip))
               (mapcar #'car
                       (sqlite-select db "SELECT s.id FROM sections s
                                          WHERE s.id NOT IN (SELECT section_id FROM vec_sections)
                                          LIMIT ?" (list (* 4 limit)))))
   limit))

;;; Building

(defconst chai-vector--progress-prefix "CHAI-VECTOR-PROGRESS ")
(defconst chai-vector--result-prefix "CHAI-VECTOR-RESULT ")

;;;###autoload
(defun chai-vector-batch-run (settings)
  "Embed the Library's sections, reporting progress on stderr.
Entry point of the batch Emacs started by `chai-vector-build'."
  (setq chai-index-file (plist-get settings :index))
  (when-let* ((value (plist-get settings :model))) (setq chai-vector-model value))
  (when-let* ((value (plist-get settings :extension))) (setq chai-vector-extension value))
  (let* ((db (chai-index--db))
         (started (float-time))
         (done 0)
         (failed 0))
    (unless (chai-vector-enable db)
      (princ (format "%s(:error \"sqlite-vec extension not found\")\n" chai-vector--result-prefix)
             #'external-debugging-output)
      (error "sqlite-vec extension not found"))
    (let* ((probe (car (chai-vector-embed '("测试"))))
           (dimension (length probe))
           (total (caar (sqlite-select db "SELECT count(*) FROM sections"))))
      (unless (> dimension 0) (error "The embedding endpoint returned nothing"))
      (chai-vector--ensure-table db dimension)
      (cl-flet ((report ()
                  (princ (format "%s%d %d\n" chai-vector--progress-prefix done total)
                         #'external-debugging-output)))
        (report)
        (let ((skip (make-hash-table :test 'eql))
              (pending t))
          (while pending
            (setq pending (chai-vector--pending db 64 skip))
            (dolist (batch (chai-vector--batches db pending))
              (let* ((ids (car batch))
                     (vectors (chai-vector-embed (cdr batch))))
                (if (/= (length vectors) (length ids))
                    ;; No vector is better than a wrong one: a placeholder
                    ;; would sit in the index looking like a real answer.
                    ;; Skipping leaves these for the next run to retry.
                    (progn
                      (setq failed (+ failed (length ids)))
                      (dolist (id ids) (puthash id t skip)))
                  (with-sqlite-transaction db
                    (cl-loop for id in ids
                             for vector in vectors
                             do (sqlite-execute db "INSERT INTO vec_sections (section_id, embedding)
                                                    VALUES (?, vec_quantize_int8(?, 'unit'))"
                                                (list id (chai-vector--json vector)))))
                  (setq done (+ done (length ids)))))
              (report)
              (chai-index--checkpoint))))
        (chai-index--checkpoint 'truncate)
        (princ (format "%s(:embedded %d :failed %d :dimension %d :seconds %.1f)\n"
                       chai-vector--result-prefix done failed dimension
                       (- (float-time) started))
               #'external-debugging-output)))))

(defvar chai-vector--process nil)
(defvar chai-vector--progress nil)
(defvar chai-vector--partial "")

(defun chai-vector--handle-line (line)
  "Act on one output LINE from the embedding process."
  (cond
   ((string-prefix-p chai-vector--progress-prefix line)
    (pcase-let ((`(,done ,total)
                 (mapcar #'string-to-number
                         (split-string (substring line (length chai-vector--progress-prefix))))))
      (setq chai-vector--progress (format " Chai embedding %d/%d" done total))
      (force-mode-line-update t)
      (let ((message-log-max nil))
        (message "Embedding the Chai Library... %d/%d section(s) — %s to stop"
                 done total (substitute-command-keys "\\[chai-vector-stop]")))))
   ((string-prefix-p chai-vector--result-prefix line)
    (let ((result (car (read-from-string (substring line (length chai-vector--result-prefix))))))
      (if (plist-get result :error)
          (message "Chai embedding failed: %s" (plist-get result :error))
        (message "Chai embedding: %d section(s) in %.0f min%s"
                 (plist-get result :embedded) (/ (plist-get result :seconds) 60)
                 (if (> (or (plist-get result :failed) 0) 0)
                     (format " (%d failed)" (plist-get result :failed)) "")))))
   ((not (string-blank-p line))
    (message "Chai embedding: %s" line))))

(defun chai-vector--filter (_process string)
  "Turn STRING into whole lines and act on each."
  (setq chai-vector--partial (concat chai-vector--partial string))
  (let ((lines (split-string chai-vector--partial "\n")))
    (setq chai-vector--partial (car (last lines)))
    (dolist (line (butlast lines))
      (chai-vector--handle-line (string-trim-right line)))))

(defun chai-vector--sentinel (process _event)
  "Clear the mode line once PROCESS ends."
  (unless (process-live-p process)
    (setq chai-vector--process nil chai-vector--progress nil chai-vector--partial "")
    (setq global-mode-string (delq 'chai-vector--progress global-mode-string))
    (force-mode-line-update t)))

;;;###autoload
(defun chai-vector-stop ()
  "Stop an embedding run."
  (interactive)
  (if (not (process-live-p chai-vector--process))
      (message "Chai: no embedding is running")
    (delete-process chai-vector--process)
    (message "Chai embedding stopped — run `chai-vector-build' to resume")))

;;;###autoload
(defun chai-vector-build ()
  "Give the Library semantic recall by embedding its sections.

Runs in a separate Emacs, one batch at a time, and can be stopped and resumed:
sections already embedded are skipped.  This is a long job — measured at about
three hours for a library of a few thousand books — so it is deliberately a
command of its own rather than part of `chai-index-rebuild'."
  (interactive)
  (when (process-live-p chai-vector--process)
    (user-error "An embedding run is already going; stop it with `chai-vector-stop'"))
  (unless (chai-vector-extension-path)
    (user-error "sqlite-vec not found%s: put `%s' in %s, or set `chai-vector-extension'"
                (if chai-vector-extension
                    (format " at %s" chai-vector-extension)
                  "")
                chai-vector--extension-name
                (expand-file-name "chai/" user-emacs-directory)))
  (chai-index-close)
  (let ((directory (file-name-directory (locate-library "chai-vector")))
        (settings (list :index chai-index-file
                        :model chai-vector-model
                        :extension (chai-vector-extension-path))))
    (setq chai-vector--partial ""
          chai-vector--progress " Chai embedding…")
    (unless (memq 'chai-vector--progress global-mode-string)
      (setq global-mode-string (append global-mode-string '(chai-vector--progress))))
    (setq chai-vector--process
          (make-process
           :name "chai-vector"
           :buffer nil :noquery t :connection-type 'pipe
           :command (list (chai-index--emacs-program) "-Q" "--batch"
                          "-L" directory "-l" "chai-vector"
                          "--eval" (format "(chai-vector-batch-run '%S)" settings))
           :filter #'chai-vector--filter
           :sentinel #'chai-vector--sentinel))
    (message "Chai: embedding the Library in a separate Emacs; %s to stop"
             (substitute-command-keys "\\[chai-vector-stop]"))
    nil))

;;; Recall

(defun chai-vector-recall-chunks (query)
  "Return chunk ids for QUERY, best first, or nil when unavailable.

The ranking is by section: the passages of the closest section come first,
capped so that one long section cannot crowd out the rest."
  (when-let* ((db (ignore-errors (chai-index--db))))
    (when (chai-vector-available-p db)
      (when-let* ((vector (car (chai-vector-embed (list query)))))
        (let ((sections (sqlite-select
                         db "SELECT section_id FROM vec_sections
                             WHERE embedding MATCH vec_quantize_int8(?, 'unit') AND k = ?"
                         (list (chai-vector--json vector) chai-vector-recall)))
              (ids nil))
          (dolist (row sections)
            (dolist (chunk (sqlite-select db "SELECT id FROM chunks WHERE section = ? LIMIT ?"
                                          (list (car row) chai-vector-chunks-per-section)))
              (push (car chunk) ids)))
          (nreverse ids))))))

;;;###autoload
(defun chai-vector-status ()
  "Report how much of the Library has semantic recall."
  (interactive)
  (let* ((db (chai-index--db))
         (sections (caar (sqlite-select db "SELECT count(*) FROM sections")))
         (embedded (if (chai-vector-available-p db)
                       (caar (sqlite-select db "SELECT count(*) FROM vec_sections"))
                     0)))
    (message "Chai semantic recall: %d/%d section(s) embedded%s"
             embedded (or sections 0)
             (if (chai-vector-extension-path) ""
                 (format " — sqlite-vec (%s) not found" chai-vector--extension-name)))
    (list :sections (or sections 0) :embedded embedded)))

(provide 'chai-vector)

;;; chai-vector.el ends here
