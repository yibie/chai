;;; chai-library-table.el --- Table component for Chai Library -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Yibie
;; Author: Yibie <yibie@outlook.com>
;; Keywords: library, reading, chai
;; Package-Requires: ((emacs "29.1") (chai-library "0.1.0"))

;;; Commentary:
;; This module provides the table UI component for Chai Library using tabulated-list-mode.

(require 'chai-library)
(require 'tabulated-list)
(require 'cl-lib)
(require 'seq)
(require 'tp)

(defgroup chai-library-table nil
  "Table component for Chai Library."
  :group 'chai-library)

;;; tp.el Incremental Update Infrastructure

(defun chai-library--tp-cell-var (book-id field)
  "Generate a variable symbol for BOOK-ID and FIELD combination."
  (intern (format "chai--cell-%s-%s" book-id field)))

(defun chai-library--tp-layer-name (book-id field)
  "Generate a layer name for BOOK-ID and FIELD combination."
  (intern (format "chai-layer-%s-%s" book-id field)))

(defun chai-library--tp-define-cell-layer (book-id field)
  "Define a tp layer for BOOK-ID and FIELD with reactive variable.
The layer uses `display' property to show the cell value."
  (let* ((var-sym (chai-library--tp-cell-var book-id field))
         (layer-name (chai-library--tp-layer-name book-id field)))
    ;; Ensure variable exists
    (unless (boundp var-sym)
      (set var-sym ""))
    ;; Define the layer with reactive display property
    ;; Use tp--define-layer-internal with :props keyword
    (tp--define-layer-internal layer-name
      :props `(display ,(intern (concat "$" (symbol-name var-sym)))))))


(defun chai-library--tp-update-cell (book-id field value)
  "Update cell VALUE for BOOK-ID and FIELD.
Uses tp.el to incrementally update the display property."
  (let ((var-sym (chai-library--tp-cell-var book-id field)))
    ;; Set the reactive variable, triggering tp.el auto-update
    (set var-sym value)))

;;; Utility Functions

(defun chai-library--truncate-text (text max-width)
  "Truncate TEXT to MAX-WIDTH characters.
If text is longer than max-width, add '...' at the end."
  (if (<= (string-width text) max-width)
      text
    (concat (substring text 0 (- max-width 3)) "...")))

(defun chai-library--align-text (text width alignment)
  "Align TEXT to WIDTH according to ALIGNMENT (:left, :right, :center)."
  (let ((text-width (string-width text)))
    (cond
     ((>= text-width width) (substring text 0 width))
     ((eq alignment :right)
      (concat (make-string (- width text-width) ? ) text))
     ((eq alignment :center)
      (let ((left (/ (- width text-width) 2)))
        (concat (make-string left ? ) text
                (make-string (- (- width text-width) left) ? ))))
     (t ;; :left or default
      (concat text (make-string (- width text-width) ? ))))))

(defun chai-library--get-cell-value (book field)
  "Get cell value from BOOK for FIELD.
FIELD can be: status, rating, title, author, keywords."
  (pcase field
    ('status (chai-library--format-status-value (chai-book-status book)))
    ('rating (chai-library--format-rating-value (chai-book-rating book)))
    ('title (or (chai-book-title book) "Untitled"))
    ('author (or (chai-book-author book) ""))
    ('keywords (string-join (or (chai-book-keywords book) nil) ", "))
    (_ "")))

;;; Cell Formatting Functions

(defun chai-library--format-status-value (status)
  "Format STATUS value as a propertized string."
  (propertize (pcase status
                ('reading "📖")
                ('done "✅")
                ('archived "📦")
                (_ "⚪️"))
              'face (pcase status
                      ('reading 'chai-library-status-reading)
                      ('done 'chai-library-status-done)
                      ('archived 'chai-library-status-archived)
                      (_ 'chai-library-status-unread))))

(defun chai-library--format-rating-value (rating)
  "Format RATING value as a propertized string."
  (propertize (make-string (or rating 0) ?★)
              'face 'chai-library-rating))

;;; Reactive Update Functions

(defun chai-library--update-book-status (book-id new-status)
  "Update the status display for BOOK-ID to NEW-STATUS.
Uses tp.el reactive variable to trigger automatic UI update."
  (chai-library--tp-update-cell book-id :status
                                 (chai-library--format-status-value new-status)))

(defun chai-library--update-book-rating (book-id new-rating)
  "Update the rating display for BOOK-ID to NEW-RATING.
Uses tp.el reactive variable to trigger automatic UI update."
  (chai-library--tp-update-cell book-id :rating
                                 (chai-library--format-rating-value new-rating)))

(defun chai-library--tp-init-book (book-id status rating)
  "Initialize tp layers for BOOK-ID with STATUS and RATING."
  ;; Define layers for status and rating
  (chai-library--tp-define-cell-layer book-id :status)
  (chai-library--tp-define-cell-layer book-id :rating)
  ;; Set initial values
  (chai-library--tp-update-cell book-id :status
                                 (chai-library--format-status-value status))
  (chai-library--tp-update-cell book-id :rating
                                 (chai-library--format-rating-value rating)))


;;; Faces


(defface chai-library-status-reading
  '((t :inherit success))
  "Face for books with 'reading status.")

(defface chai-library-status-done
  '((t :inherit info))
  "Face for books with 'done status.")

(defface chai-library-status-unread
  '((t :inherit shadow))
  "Face for books with 'unread status.")

(defface chai-library-status-archived
  '((t :inherit shadow))
  "Face for books with 'archived status.")

(defface chai-library-title
  '((t :inherit shadow))
  "Face for title column.")

(defface chai-library-author
  '((t :inherit shadow))
  "Face for author column.")

(defface chai-library-time
  '((t :inherit shadow))
  "Face for time column.")

(defface chai-library-rating
  '((t :inherit shadow :foreground "#cf9f30"))
  "Face for rating column.")

(defface chai-library-keywords
  '((t :inherit shadow))
  "Face for keywords column.")

;;; Table Format

(defvar chai-library-tabulated-format
  `[("Status" 4 t)
    ("Rating" 11 ,(lambda (a b)
                    (let ((ra (length (aref (cadr a) 1)))
                          (rb (length (aref (cadr b) 1))))
                      (< ra rb)))
               :right-align nil)
    ("Author" 20 t)
    ("Title" 45 t)
    ("Keywords" 25 nil)
    ("Time" 12 t)]
  "Format specification for `tabulated-list-mode`.")

(defvar-local chai-library--filter nil
  "Current filter pattern.")

;;; Major Mode

(define-derived-mode chai-library-mode tabulated-list-mode "Chai Library"
  "Major mode for viewing Chai library."
  (setq tabulated-list-format chai-library-tabulated-format)
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("Time" . t))
  (add-hook 'tabulated-list-revert-hook #'chai-library-refresh nil t))

;;; Data Conversion

(defun chai-library--book-to-entry (book)
  "Convert a chai-book struct to a tabulated-list entry.
Returns (ID VECTOR).
For managed books (with IDs), applies tp layers for reactive updates."
  (let* ((book-id (chai-book-id book))
         (status (chai-book-status book))
         (rating (chai-book-rating book))
         ;; Create status and rating cells
         (status-cell (chai-library--format-status-value status))
         (rating-cell (chai-library--format-rating-value rating)))
    ;; For managed books, initialize tp layers and apply layer to cells
    (when book-id
      (chai-library--tp-init-book book-id status rating)
      ;; Apply tp layer to status cell (placeholder for tp to update via display property)
      (setq status-cell (tp-set " " (chai-library--tp-layer-name book-id :status)))
      ;; Apply tp layer to rating cell
      (setq rating-cell (tp-set " " (chai-library--tp-layer-name book-id :rating))))
    (list book  ; Use the book struct itself as the ID for easy retrieval
          (vector
           status-cell
           rating-cell
           ;; Author
           (propertize (or (chai-book-author book) "")
                       'face 'chai-library-author)
           ;; Title
           (propertize (or (chai-book-title book) "Untitled")
                       'face 'chai-library-title)
           ;; Keywords
           (propertize (string-join (or (chai-book-keywords book) nil) ",")
                       'face 'chai-library-keywords)
           ;; Time
           (propertize (format-time-string "%Y-%m-%d" (chai-book-modified book))
                       'face 'chai-library-time)))))


(defun chai-library--filter-books (books pattern)
  "Filter BOOKS by PATTERN."
  (if (or (not pattern) (string-empty-p pattern))
      books
    (seq-filter (lambda (book)
                  (let ((title (chai-book-title book))
                        (author (or (chai-book-author book) ""))
                        (keywords (string-join (or (chai-book-keywords book) nil) " ")))
                    (or (string-match-p pattern title)
                        (string-match-p pattern author)
                        (string-match-p pattern keywords))))
                books)))

;;; Commands

(defun chai-library-refresh ()
  "Refresh the library display."
  (interactive)
  (let* ((all-books (chai-library-scan))
         (books (if chai-library--filter
                    (chai-library--filter-books all-books chai-library--filter)
                  all-books)))
    (setq tabulated-list-entries
          (mapcar #'chai-library--book-to-entry books))
    (tabulated-list-print t)))


;;;###autoload
(defun chai-library-open ()
  "Display Chai library table in a full-frame window."
  (interactive)
  (let ((buf (get-buffer-create "*Chai Library*")))
    (with-current-buffer buf
      (chai-library-mode)
      (chai-library-refresh))
    (pop-to-buffer buf)
    (delete-other-windows)))


(defun chai-library-get-book-at-point ()
  "Get the book struct at point, supporting both backend modes."
  (or (tabulated-list-get-id)
      (get-text-property (point) 'chai-book)))

(defun chai-library-open-book-at-point ()
  "Open the book file at current point."
  (interactive)
  (let ((book (chai-library-get-book-at-point)))
    (if book
        (let ((path (chai-book-file-path book)))
          (unless (and (stringp path) (file-exists-p path))
            (user-error "File not found (stale entry?). Try `g` to refresh: %s" path))
          (find-file path))
      (user-error "No book at this line"))))

(defun chai-library--update-book-path-after-rename (book old-path new-path)
  "Update BOOK path in-place after renaming OLD-PATH -> NEW-PATH.

This keeps the tabulated-list entry stable (it stores BOOK as its ID), and
prevents opening a stale path (which would create an empty buffer).
Also updates any live buffer visiting OLD-PATH and the scan cache."
  (when (and (stringp old-path) (stringp new-path) (not (string= old-path new-path)))
    ;; If the old file is currently visited, update that buffer too.
    (when-let ((buf (get-file-buffer old-path)))
      (with-current-buffer buf
        ;; File is already renamed on disk; just update buffer bookkeeping.
        (set-visited-file-name new-path t))))
  (when (and book (stringp new-path))
    (setf (chai-book-file-path book) new-path))
  ;; Keep scan cache consistent: filename -> chai-book
  (when (and (boundp 'chai-library--cache-hash)
             (hash-table-p chai-library--cache-hash)
             (stringp old-path)
             (stringp new-path))
    (let ((old-name (file-name-nondirectory old-path))
          (new-name (file-name-nondirectory new-path)))
      (unless (string= old-name new-name)
        (remhash old-name chai-library--cache-hash)
        (puthash new-name book chai-library--cache-hash)))))

(defun chai-library-set-filter (pattern)
  "Set filter pattern."
  (interactive "sFilter pattern: ")
  (setq chai-library--filter (if (string-empty-p pattern) nil pattern))
  (chai-library-refresh))

(defun chai-library-clear-filter ()
  "Clear current filter."
  (interactive)
  (setq chai-library--filter nil)
  (chai-library-refresh))

;;; Keybindings

(define-key chai-library-mode-map (kbd "RET") #'chai-library-open-book-at-point)
;;; Commands

(defun chai-library-add ()
  "Import/Rename the current file to match Chai naming convention.
Prompts for a new filename starting with an ID.
If the current file already has an ID, it is preserved.
Format: ID__Author__Title.org"
  (interactive)
  (let* ((book (or (chai-library-get-book-at-point) (user-error "No book selected")))
         (current-path (chai-book-file-path book))
         (current-id (chai-book-id book))
         (current-base (file-name-base current-path))
         (id (or current-id (chai-library--generate-id)))
         ;; Initial input:
         ;; If file starts with ID, use base as is (user edits suffix).
         ;; If not, prepend generated ID.
         (initial-input (if current-id
                           (concat current-base)
                         (concat id "__" current-base)))
         (new-base (read-string "New Book (Format: ID__Author__Title): " initial-input))
         (safe-base (replace-regexp-in-string "[/\\:*?\"<>|[:space:]]+" "-" new-base))
         (new-filename (concat safe-base ".org"))
         (new-path (expand-file-name new-filename (file-name-directory current-path))))
    
    (when (equal current-path new-path)
      (user-error "Filename unchanged"))
    
    (when (file-exists-p new-path)
      (user-error "Target file already exists: %s" new-path))
    
    (rename-file current-path new-path)
    (chai-library--ensure-file-id new-path)
    (chai-library--update-book-path-after-rename book current-path new-path)
    (message "Renamed to: %s" new-filename)
    (chai-library-refresh)))

(defun chai-library-set-status ()
  "Set status for the book at point."
  (interactive)
  (let* ((book (or (chai-library-get-book-at-point) (user-error "No book selected")))
         (current-path (chai-book-file-path book))
         (current-rating (chai-book-rating book))
         (book-id (chai-book-id book))
         (status-str (completing-read "Status: "
                                      (mapcar #'symbol-name chai-library-status-choices)
                                      nil t))
         (status (intern status-str)))
    (if (not book-id)
      (user-error "Cannot set status for unmanaged file. Use 'a' to import first")
      (let ((new-path (chai-library--rename-managed-file-state current-path status current-rating)))
        (chai-library--update-book-path-after-rename book current-path new-path)
        (setf (chai-book-status book) status)
        (setf (chai-book-rating book) current-rating))
      ;; Update reactive cell directly - tp.el will handle the UI update
      (chai-library--update-book-status book-id status))))

(defun chai-library--set-rating-internal (rating)
  "Internal helper to set RATING for book at point."
  (let* ((book (or (chai-library-get-book-at-point) (user-error "No book selected")))
         (current-path (chai-book-file-path book))
         (current-status (chai-book-status book))
         (book-id (chai-book-id book)))
    (if (not book-id)
      (user-error "Cannot set rating for unmanaged file. Use 'a' to import first")
      (let ((new-path (chai-library--rename-managed-file-state current-path current-status rating)))
        (chai-library--update-book-path-after-rename book current-path new-path)
        (setf (chai-book-status book) current-status)
        (setf (chai-book-rating book) rating))
      ;; Update reactive cell directly - tp.el will handle the UI update
      (chai-library--update-book-rating book-id rating))))

(defun chai-library-set-rating (rating)
  "Set RATING for the book at point."
  (interactive "nRating (0-5): ")
  (unless (and (>= rating 0) (<= rating 5))
    (user-error "Rating must be between 0 and 5"))
  (chai-library--set-rating-internal (if (= rating 0) nil rating)))

(defun chai-library-set-rating-0 () (interactive) (chai-library--set-rating-internal nil))
(defun chai-library-set-rating-1 () (interactive) (chai-library--set-rating-internal 1))
(defun chai-library-set-rating-2 () (interactive) (chai-library--set-rating-internal 2))
(defun chai-library-set-rating-3 () (interactive) (chai-library--set-rating-internal 3))
(defun chai-library-set-rating-4 () (interactive) (chai-library--set-rating-internal 4))
(defun chai-library-set-rating-5 () (interactive) (chai-library--set-rating-internal 5))

(defun chai-library-cycle-sort ()
  "Cycle sorting through Author, Title, Time, Rating, Status."
  (interactive)
  (let* ((sort-cols '("Author" "Title" "Time" "Rating" "Status"))
         (current-col (car tabulated-list-sort-key))
         (next-col (or (cadr (member current-col sort-cols)) (car sort-cols))))
    (setq tabulated-list-sort-key (cons next-col (not (cdr tabulated-list-sort-key))))
    (message "Sorting by %s" next-col)
    (tabulated-list-revert)))

(define-key chai-library-mode-map (kbd "/") #'chai-library-set-filter)
(define-key chai-library-mode-map (kbd "c") #'chai-library-clear-filter)
(define-key chai-library-mode-map (kbd "g") #'chai-library-refresh)
(define-key chai-library-mode-map (kbd "a") #'chai-library-add)
(define-key chai-library-mode-map (kbd "s") #'chai-library-set-status)
(define-key chai-library-mode-map (kbd "r") #'chai-library-set-rating)
(define-key chai-library-mode-map (kbd "S") #'chai-library-cycle-sort)
(define-key chai-library-mode-map (kbd "k") #'chai-library-set-keywords)
(define-key chai-library-mode-map (kbd "d") #'chai-library-delete)
(define-key chai-library-mode-map (kbd "0") #'chai-library-set-rating-0)
(define-key chai-library-mode-map (kbd "1") #'chai-library-set-rating-1)
(define-key chai-library-mode-map (kbd "2") #'chai-library-set-rating-2)
(define-key chai-library-mode-map (kbd "3") #'chai-library-set-rating-3)
(define-key chai-library-mode-map (kbd "4") #'chai-library-set-rating-4)
(define-key chai-library-mode-map (kbd "5") #'chai-library-set-rating-5)

(defun chai-library-set-keywords ()
  "Set keywords for the book at point.
Keywords are stored in the filename as ==keyword1_keyword2.
Input is a comma-separated list; spaces within each keyword become hyphens."
  (interactive)
  (let* ((book (or (chai-library-get-book-at-point) (user-error "No book selected")))
         (current-path (chai-book-file-path book))
         (book-id (chai-book-id book))
         (current-keywords (chai-book-keywords book))
         (current-kw-str (string-join (or current-keywords nil) ", "))
         (new-kw-str (read-string "Keywords (comma-separated): " current-kw-str))
         (new-keywords (when (not (string-empty-p (string-trim new-kw-str)))
                         (seq-remove #'string-empty-p
                                     (mapcar (lambda (s)
                                               (replace-regexp-in-string "[[:space:]]+" "-" (string-trim s)))
                                             (split-string new-kw-str "[,，]" t))))))
    (if (not book-id)
        (user-error "Cannot set keywords for unmanaged file. Use 'a' to import first")
      (setf (chai-book-keywords book) new-keywords)
      (let* ((new-filename (chai-library--generate-filename book))
             (dir (file-name-directory current-path))
             (new-path (expand-file-name new-filename dir)))
        (unless (string= (file-truename current-path) (file-truename new-path))
          (when (file-exists-p new-path)
            (user-error "Target file already exists: %s" new-path))
          (rename-file current-path new-path)
          (chai-library--update-book-path-after-rename book current-path new-path))
        (message "Keywords: %s" (if new-keywords (string-join new-keywords ", ") "(none)"))
        (chai-library-refresh)))))

(defun chai-library-delete ()
  "Delete the book file at point.
Prompts for confirmation before deleting."
  (interactive)
  (let* ((book (or (chai-library-get-book-at-point) (user-error "No book selected")))
         (file-path (chai-book-file-path book))
         (title (or (chai-book-title book) (file-name-base file-path))))
    (when (yes-or-no-p (format "Delete \"%s\"? " title))
      (delete-file file-path)
      (message "Deleted: %s" file-path)
      (chai-library-refresh))))


(provide 'chai-library-table)
;;; chai-library-table.el ends here
