;;; chai-search.el --- Search across the Chai Library -*- lexical-binding: t; -*-

;; Author: Yibie <yibie@outlook.com>
;; Keywords: outlines, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Searches the index built by `chai-index.el' and shows the ranked passages
;; in a buffer whose entries jump back to the exact place in the source book.
;;
;; Recall runs as independent ranked lists that are combined with Reciprocal
;; Rank Fusion.  Today there are two: BM25 over every chunk, and BM25 over the
;; chunks the reader has annotated.  Because RRF consumes positions rather than
;; scores, the channels need no common scale, an empty channel contributes
;; nothing without a special case, and a third channel (dense vectors) can be
;; added later by passing one more list to `chai-search--fuse'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chai-index)
(require 'chai-vector)

;; Defined in `chai-library-table', which is not required here: searching does
;; not depend on the table, only the two commands the table binds do.
(declare-function chai-library-get-book-at-point "chai-library-table" ())

;;; Customization

(defgroup chai-search nil
  "Search across the Chai Library."
  :group 'chai)

(defcustom chai-search-recall-limit 100
  "How many chunks each recall channel considers before fusion."
  :type 'integer
  :group 'chai-search)

(defcustom chai-search-default-limit 20
  "How many hits `chai-search' shows by default."
  :type 'integer
  :group 'chai-search)

(defcustom chai-search-rrf-k 60
  "Smoothing constant of the Reciprocal Rank Fusion.
Larger values flatten the advantage of the very top ranks, so a single
channel cannot dominate the fused order."
  :type 'integer
  :group 'chai-search)

(defcustom chai-search-semantic nil
  "Whether an ordinary search also recalls passages by meaning.

Off by default: the lexical channel answers in about a millisecond, while the
semantic one scans every vector and takes a fraction of a second.  `chai-ask'
binds this on, where that cost disappears next to the model's own latency."
  :type 'boolean
  :group 'chai-search)

(defcustom chai-search-preview-lines 4
  "How many lines of a passage the results buffer shows."
  :type 'integer
  :group 'chai-search)

;;; Fusion

(defun chai-search--fuse (rankings k)
  "Fuse RANKINGS into one ordered alist of (ID . SCORE).
Each element of RANKINGS is a list of chunk ids in descending relevance.
Only positions count, so channels with incomparable scores fuse directly and
an empty channel simply adds nothing."
  (let ((scores (make-hash-table :test 'eql))
        (fused nil))
    (dolist (ranking rankings)
      (let ((rank 0))
        (dolist (id ranking)
          (setq rank (1+ rank))
          (puthash id (+ (gethash id scores 0.0) (/ 1.0 (+ k rank))) scores))))
    (maphash (lambda (id score) (push (cons id score) fused)) scores)
    (sort fused (lambda (a b) (> (cdr a) (cdr b))))))

;;; Recall

(defun chai-search--where-clauses (filters)
  "Return (SQL . VALUES) for FILTERS, a plist of optional restrictions.
Supported keys: :file, :status, :keyword, :type, and :highlighted."
  (let ((sql nil)
        (values nil))
    (when-let* ((file (plist-get filters :file)))
      (push "d.path = ?" sql)
      (push (expand-file-name file) values))
    (when-let* ((status (plist-get filters :status)))
      (push "d.status = ?" sql)
      (push (format "%s" status) values))
    (when-let* ((keyword (plist-get filters :keyword)))
      (push "instr(' ' || d.keywords || ' ', ?) > 0" sql)
      (push (format " %s " keyword) values))
    (when-let* ((type (plist-get filters :type)))
      (push "instr(' ' || c.hl_types || ' ', ?) > 0" sql)
      (push (format " %s " type) values))
    (when (plist-get filters :highlighted)
      (push "c.hl_types <> ''" sql))
    (cons (if sql (concat " AND " (string-join (nreverse sql) " AND ")) "")
          (nreverse values))))

(defun chai-search--recall (query filters)
  "Return the raw recall rows for QUERY under FILTERS, best first.
Each row is (ID PATH HEADLINE OUTLINE BEG END PREVIEW TYPES BM25 TITLE)."
  (when-let* ((expression (chai-index-query-expression query)))
    (let* ((where (chai-search--where-clauses filters))
           (db (chai-index--db)))
      (sqlite-select
       db
       (concat "SELECT c.id, d.path, c.headline, c.outline, c.beg_pos, c.end_pos,
                       c.preview, c.hl_types, bm25(fts_chunks), d.title
                FROM fts_chunks
                JOIN chunks c ON c.id = fts_chunks.rowid
                JOIN documents d ON d.id = c.doc
                WHERE fts_chunks MATCH ?"
               (car where)
               " ORDER BY bm25(fts_chunks) LIMIT ?")
       (append (list expression) (cdr where) (list chai-search-recall-limit))))))

(defun chai-search--rows-for (ids filters)
  "Return recall rows for IDS, the passages only the semantic channel found.
They never went through the lexical query, so their columns are fetched here."
  (when ids
    (let* ((where (chai-search--where-clauses filters))
           (placeholders (mapconcat (lambda (_) "?") ids ","))
           (db (chai-index--db)))
      (sqlite-select
       db
       (concat "SELECT c.id, d.path, c.headline, c.outline, c.beg_pos, c.end_pos,
                       c.preview, c.hl_types, 0.0, d.title
                FROM chunks c JOIN documents d ON d.id = c.doc
                WHERE c.id IN (" placeholders ")"
               (car where))
       (append ids (cdr where))))))

(defun chai-search--row-to-hit (row score bm25 highlighted)
  "Build a hit plist from recall ROW with its fused SCORE."
  (cl-flet ((present (value) (unless (chai-index-blank-p value) (chai-index-trim value))))
    (list :id (nth 0 row)
          :file (nth 1 row)
          :headline (present (nth 2 row))
          :outline (present (nth 3 row))
          :beg (nth 4 row)
          :end (nth 5 row)
          :text (nth 6 row)
          :types (split-string (or (nth 7 row) "") " " t)
          :title (nth 9 row)
          :score score
          :explain (list :bm25 bm25 :highlight highlighted :rrf score))))

;;;###autoload
(defun chai-search-query (query &optional limit filters)
  "Return up to LIMIT hits for QUERY across the indexed Chai Library.

FILTERS is an optional plist restricting the search: :file limits to one book,
:status to books in a reading state, :keyword to books carrying a keyword,
:type to passages annotated with a highlight type, and a non-nil :highlighted
to annotated passages only.

Each hit is a plist with :file, :title, :headline, :beg, :end, :text, :types,
:score and an :explain breakdown.  :headline is the heading directly above the
passage and :outline the full path of headings enclosing it.  The positions
address the source file, so a caller can jump straight to the passage.

:text is the stored preview, bounded by `chai-index-preview-chars'.  Call
`chai-search-passage-text' for the passage in full."
  (let* ((usable (and query (not (string-blank-p query))))
         (rows (and usable (chai-search--recall query filters)))
         ;; Only ever adds candidates.  A vector search returns its k nearest
         ;; neighbours whatever the question, so it cannot be trusted to say
         ;; that nothing matched — that remains the lexical channel's answer.
         (semantic (and usable rows chai-search-semantic
                        (ignore-errors (chai-vector-recall-chunks query)))))
    (when rows
      (let* ((lexical (mapcar #'car rows))
             ;; Not a second query: the annotated subset of the same recall,
             ;; ranked the same way.  Passages the reader marked therefore earn
             ;; a second RRF contribution and float up.
             (annotated (mapcar #'car
                                (seq-filter (lambda (row)
                                              (not (string-empty-p (or (nth 7 row) ""))))
                                            rows)))
             ;; Passages only the semantic channel found still need their
             ;; columns, but they belong to that channel alone — counting them
             ;; as lexical hits too would credit them for a match they never
             ;; made.
             (extra (seq-difference semantic lexical))
             (fused (chai-search--fuse (list lexical annotated semantic) chai-search-rrf-k))
             (by-id (make-hash-table :test 'eql)))
        (dolist (row (append rows (chai-search--rows-for extra filters)))
          (puthash (car row) row by-id))
        (seq-take
         (delq nil
               (mapcar (lambda (entry)
                         (when-let* ((row (gethash (car entry) by-id)))
                           (let ((types (or (nth 7 row) "")))
                             (chai-search--row-to-hit row (cdr entry)
                                                      (- (nth 8 row))
                                                      (not (string-empty-p types))))))
                       fused))
         (or limit chai-search-default-limit))))))

;;; Results buffer

(defvar chai-search--last-query nil
  "Query that produced the current results buffer.")

(defvar chai-search--last-filters nil
  "Filters that produced the current results buffer.")

(defconst chai-search-buffer-name "*Chai Search*")

(defvar-keymap chai-search-mode-map
  :doc "Keymap for `chai-search-mode'."
  "RET" #'chai-search-visit
  "o"   #'chai-search-visit-other-window
  "n"   #'chai-search-next
  "p"   #'chai-search-previous
  "g"   #'chai-search-refresh
  "<mouse-1>" #'chai-search-visit)

(define-derived-mode chai-search-mode special-mode "Chai Search"
  "Major mode for the Chai search results buffer."
  (setq-local truncate-lines nil))

(defface chai-search-title '((t :inherit font-lock-function-name-face :weight bold))
  "Face for a book title in the Chai search results."
  :group 'chai-search)

(defface chai-search-headline '((t :inherit font-lock-doc-face))
  "Face for the headline a passage sits under."
  :group 'chai-search)

(defface chai-search-meta '((t :inherit shadow))
  "Face for scores and other metadata in the Chai search results."
  :group 'chai-search)

(defun chai-search-passage-text (hit)
  "Return the full text of HIT's passage, read from its source file.

The index stores only a preview, so anything that needs the whole passage —
a wider excerpt, or context for a question — comes back through here.  Reading
from the file rather than the index also means the text is current: if the book
was edited since it was indexed, this returns what it says now."
  (let ((file (plist-get hit :file)))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((beg (min (max (point-min) (plist-get hit :beg)) (point-max)))
              (end (min (max (point-min) (plist-get hit :end)) (point-max))))
          (chai-index--clean-text (buffer-substring-no-properties beg end)))))))

(defun chai-search--preview (text)
  "Return at most `chai-search-preview-lines' lines of TEXT."
  (let ((lines (split-string text "\n" t "[ \t]+")))
    (string-join (seq-take lines chai-search-preview-lines) "\n")))

;;;###autoload
(defun chai-library-search (query)
  "Search the whole Chai Library from the Library table."
  (interactive "sSearch the Library: ")
  (chai-search query))

;;;###autoload
(defun chai-library-search-book (query)
  "Search only the book at point in the Library table."
  (interactive
   (list (read-string (format "Search 《%s》: "
                              (or (chai-book-title (chai-library-get-book-at-point))
                                  "?")))))
  (let ((book (chai-library-get-book-at-point)))
    (unless book (user-error "No book at point"))
    (chai-search query (list :file (chai-book-file-path book)))))

(defun chai-search-semantic (query)
  "Search the Chai Library by meaning as well as by wording."
  (interactive "sChai search (semantic): ")
  (let ((chai-search-semantic t))
    (chai-search query)))

(defun chai-search--insert-hit (index hit)
  "Insert HIT into the current buffer as entry number INDEX."
  (let ((start (point))
        (types (plist-get hit :types)))
    (insert (propertize (format "%2d. " index) 'face 'chai-search-meta))
    (insert (propertize (or (plist-get hit :title)
                            (file-name-base (plist-get hit :file)))
                        'face 'chai-search-title))
    (when-let* ((outline (or (plist-get hit :outline) (plist-get hit :headline))))
      (insert (propertize (concat chai-index-outline-separator outline)
                          'face 'chai-search-headline)))
    (insert (propertize (format "   %.4f%s\n"
                                (plist-get hit :score)
                                (if types (concat "  ★ " (string-join types " ")) ""))
                        'face 'chai-search-meta))
    (insert (replace-regexp-in-string "^" "    " (chai-search--preview (plist-get hit :text))))
    (insert "\n\n")
    (put-text-property start (point) 'chai-search-hit hit)))

(defun chai-search--render (query hits)
  "Render HITS for QUERY into the Chai search buffer and display it."
  (let ((buffer (get-buffer-create chai-search-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (chai-search-mode)
        (insert (propertize (format "Chai search: %s — %d hit(s)\n\n" query (length hits))
                            'face 'chai-search-meta))
        (if (null hits)
            (insert "No passage matched.  Try fewer words, or run `chai-index-rebuild'.\n")
          (let ((index 0))
            (dolist (hit hits)
              (setq index (1+ index))
              (chai-search--insert-hit index hit))))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun chai-search--hit-at-point ()
  "Return the hit at point, or signal a user error."
  (or (get-text-property (point) 'chai-search-hit)
      (user-error "No Chai search hit at point")))

(defun chai-search--goto (hit other-window)
  "Open the source of HIT, in OTHER-WINDOW when non-nil."
  (let* ((buffer (find-file-noselect (plist-get hit :file)))
         (position (plist-get hit :beg)))
    (if other-window (pop-to-buffer buffer) (pop-to-buffer-same-window buffer))
    (widen)
    ;; The index can lag an edit; clamp instead of signalling.
    (goto-char (min (max (point-min) position) (point-max)))
    (when (derived-mode-p 'org-mode) (org-fold-show-context 'link-search))
    (recenter)))

(defun chai-search-visit ()
  "Open the passage at point in this window."
  (interactive)
  (chai-search--goto (chai-search--hit-at-point) nil))

(defun chai-search-visit-other-window ()
  "Open the passage at point in another window."
  (interactive)
  (chai-search--goto (chai-search--hit-at-point) t))

(defun chai-search-next ()
  "Move to the next hit."
  (interactive)
  (let ((next (next-single-property-change (point) 'chai-search-hit)))
    (when next (goto-char next))))

(defun chai-search-previous ()
  "Move to the previous hit."
  (interactive)
  (let ((previous (previous-single-property-change (point) 'chai-search-hit)))
    (when previous (goto-char previous))))

(defun chai-search-refresh ()
  "Run the last query again."
  (interactive)
  (unless chai-search--last-query (user-error "No previous Chai search"))
  (chai-search--render chai-search--last-query
                       (chai-search-query chai-search--last-query
                                          chai-search-default-limit
                                          chai-search--last-filters)))

;;;###autoload
(defun chai-search (query &optional filters)
  "Search the Chai Library for QUERY and show the ranked passages.
FILTERS is the optional plist documented in `chai-search-query'."
  (interactive "sChai search: ")
  (setq chai-search--last-query query
        chai-search--last-filters filters)
  (chai-search--render query (chai-search-query query chai-search-default-limit filters)))

;;;###autoload
(defun chai-search-highlights (query)
  "Search only the passages carrying a Chai highlight or comment."
  (interactive "sChai search (annotated only): ")
  (chai-search query '(:highlighted t)))

(provide 'chai-search)

;;; chai-search.el ends here
