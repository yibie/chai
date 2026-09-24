;;; chai-context.el --- Library passages as material for a model -*- lexical-binding: t; -*-

;; Author: Yibie <yibie@outlook.com>
;; Keywords: outlines, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Turns a question into numbered passages from the Library, and turns the
;; citations a model writes back into places in your books.
;;
;; This is deliberately not an answering engine.  Choosing a model, streaming
;; its output, keeping a conversation — those belong to whatever chat client
;; you already use, and a reading tool has no business reimplementing them.
;; What a chat client cannot do is know that `[3]' means character 12847 of a
;; particular Org file, under a particular chapter.  That is what lives here.
;;
;; Two rules travel with the passages, and callers should keep them:
;;
;; - Numbering is a contract.  A model handed passages [1]..[N] may cite only
;;   those; `chai-context-citations' drops anything else.  A marker outside the
;;   range names a source that was never supplied, and so does not exist.
;;
;; - The material is budgeted before it is sent.  Overflowing a model's window
;;   does not fail loudly — the runtime drops tokens from the front, where the
;;   instructions live — so passages are added while they fit and the surplus
;;   is left out of the end.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chai-search)

;;; Customization

(defgroup chai-context nil
  "Library passages offered to a language model."
  :group 'chai)

(defcustom chai-context-passages 8
  "Greatest number of passages gathered for one question."
  :type 'integer
  :group 'chai-context)

(defcustom chai-context-tokens 4000
  "Token budget for the passages gathered for one question."
  :type 'integer
  :group 'chai-context)

(defcustom chai-context-title-width 28
  "Greatest width of a book title where a passage names its source.
Library titles carry a book's whole subtitle, which crowds out the part of a
citation a reader actually reads."
  :type 'integer
  :group 'chai-context)

;;; Naming a passage

(defun chai-context-source-name (hit)
  "Return a short name for the book and heading HIT came from."
  (concat "“"
          (truncate-string-to-width
           (or (plist-get hit :title) (file-name-base (plist-get hit :file)))
           chai-context-title-width nil nil t)
          "”"
          (if-let* ((outline (plist-get hit :outline)))
              (concat chai-index-outline-separator outline)
            "")))

(defun chai-context-label (hit index)
  "Return the heading line introducing HIT as passage INDEX."
  (format "[%d] %s" index (chai-context-source-name hit)))

;;; Gathering

(defun chai-context-select (hits)
  "Return the prefix of HITS that fits in `chai-context-tokens'.
Each element is (INDEX HIT TEXT) with TEXT the passage in full, read from its
source file so that it reflects the book as it now reads."
  (let ((budget chai-context-tokens)
        (index 0)
        (selected nil))
    (catch 'full
      (dolist (hit (seq-take hits chai-context-passages))
        (let* ((text (or (chai-search-passage-text hit) (plist-get hit :text) ""))
               (cost (+ (chai-estimate-tokens text)
                        (chai-estimate-tokens (chai-context-label hit 1)))))
          (when (and selected (> cost budget))
            ;; Dropping from the end keeps the best-ranked passages.
            (throw 'full nil))
          (setq index (1+ index)
                budget (- budget cost))
          (push (list index hit text) selected))))
    (nreverse selected)))

;;;###autoload
(defun chai-context-for (query &optional filters)
  "Return numbered Library passages relevant to QUERY, or nil when none are.

FILTERS is the optional plist documented in `chai-search-query'.  Nil means
the Library has nothing to say about QUERY — a caller must not fall back on
the model's own memory, which is the one thing a Library answer must never be."
  (let ((chai-search-semantic (or chai-search-semantic
                                  (and (fboundp 'chai-vector-available-p)
                                       (chai-vector-available-p)))))
    (when-let* ((hits (chai-search-query query chai-context-passages filters)))
      (chai-context-select hits))))

;;;###autoload
(defun chai-context-render (passages &optional heading)
  "Return PASSAGES as a numbered block of material, under HEADING."
  (concat (or heading "Passages retrieved from the reading library:") "\n\n"
          (mapconcat (lambda (entry)
                       (pcase-let ((`(,index ,hit ,text) entry))
                         (format "%s\n%s\n" (chai-context-label hit index) text)))
                     passages "\n")))

;;; Citations

;;;###autoload
(defun chai-context-citations (answer count)
  "Return the citation numbers ANSWER uses, in order of first appearance.

Only numbers between 1 and COUNT are kept: a marker outside the range of
passages actually supplied refers to a source that was never there."
  (let ((seen nil)
        (start 0))
    (while (string-match "\\[\\([0-9]+\\)\\]" answer start)
      (let ((number (string-to-number (match-string 1 answer))))
        (setq start (match-end 0))
        (when (and (>= number 1) (<= number count) (not (memq number seen)))
          (push number seen))))
    (nreverse seen)))

(defun chai-context-hit (passages number)
  "Return the hit PASSAGES numbers NUMBER, or nil."
  (nth 1 (nth (1- number) passages)))

(define-button-type 'chai-context-citation
  'action #'chai-context--follow
  'follow-link t
  'help-echo "Jump to this passage")

(defun chai-context--follow (button)
  "Open the passage BUTTON cites."
  (chai-search--goto (button-get button 'chai-hit) t))

;;;###autoload
(defun chai-context-linkify (beg end passages)
  "Turn every valid citation between BEG and END into a jump to its passage."
  (let ((count (length passages))
        (inhibit-read-only t))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\[\\([0-9]+\\)\\]" end t)
        (let ((number (string-to-number (match-string 1))))
          (when (and (>= number 1) (<= number count))
            (make-button (match-beginning 0) (match-end 0)
                         :type 'chai-context-citation
                         'chai-hit (chai-context-hit passages number))))))))

;;;###autoload
(defun chai-context-sources (passages answer)
  "Return the sources ANSWER actually cited among PASSAGES.
Each element is (NUMBER HIT NAME)."
  (mapcar (lambda (number)
            (let ((hit (chai-context-hit passages number)))
              (list number hit (chai-context-source-name hit))))
          (chai-context-citations answer (length passages))))

(provide 'chai-context)

;;; chai-context.el ends here
