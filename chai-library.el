;;; chai-library.el --- Library management for Chai using TP -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Keywords: library, reading, chai
;; Package-Requires: ((emacs "29.1") (tp "0.1.0"))

;;; Commentary:

;; This module provides the library management interface for Chai.
;; It uses the "Filename as Database" philosophy:
;; Format: ID__Author__Title==Keywords--Status-Rating.org
;;
;; UI is built using the `tp.el` library for rich, reactive text properties.

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (when dir
    (dolist (rel '("../tp" "../twidget"))
      (let ((path (expand-file-name rel dir)))
        (when (file-directory-p path)
          (add-to-list 'load-path path))))))

;; Try to load optional dependencies
(condition-case err
    (progn
      ;; Note: 'chai is removed - we only need chai-library itself
      (require 'tp)
      (require 'tp-palette)
      (require 'twidget)
      (message "Chai Library: tp.el and twidget.el loaded"))
  (error
   (message "Chai Library: Warning - tp.el or twidget.el not available: %s" (error-message-string err))
   (message "Chai Library: Running in basic mode without advanced UI features")))

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
;;; Customization

(defgroup chai-library nil
  "Library management for Chai."
  :group 'chai)

(defcustom chai-library-directory (expand-file-name "library/" (expand-file-name "chai/" user-emacs-directory))
  "Directory where book files are stored.
Defaults to ~/.emacs.d/chai/library/."
  :type 'directory
  :group 'chai-library)

(defcustom chai-library-import-inbox (expand-file-name "inbox/" (expand-file-name "chai/" user-emacs-directory))
  "Directory to watch for external files (PDF, MD, etc.) to be converted and imported."
  :type 'directory
  :group 'chai-library)

(defcustom chai-library-import-archive (expand-file-name "archive/" (expand-file-name "chai/" user-emacs-directory))
  "Directory where original files are moved after successful conversion."
  :type 'directory
  :group 'chai-library)

(defvar chai-library--python-script (expand-file-name "convert-to-org.py" (file-name-directory (or load-file-name (buffer-file-name))))
  "Path to the Python conversion script.")

(defvar chai-library--cache-hash (make-hash-table :test 'equal)
  "Incremental cache for books: filename -> chai-book struct.")

(defvar chai-library--cache-books nil
  "Cached sorted list of books for quick access when nothing changed.")

(defvar chai-library--cache-mtime nil
  "Modification time of the library directory during the last scan.")

;;; Constants & Regex

;; Format: ID__Author__Title==Keywords--Status-Rating.org
;; ID: 14 digits (YYYYMMDDHHMMSS) or Denote-like (YYYYMMDDTHHMMSS)
;; Separators: __ (Metadata), == (Keywords), -- (State)

(defcustom chai-library-id-format "%Y%m%dT%H%M%S"
  "Time format used to generate IDs for imported files.

The default matches Denote's timestamp style: YYYYMMDDTHHMMSS."
  :type 'string
  :group 'chai-library)

(defconst chai-library-id-regexp
  "\\`\\([0-9]\\{14\\}\\|[0-9]\\{8\\}T[0-9]\\{6\\}\\)\\'"
  "Regexp matching a managed ID (14 digits or Denote-like).")

(defconst chai-library-filename-regexp
  "\\`\\([0-9]\\{14\\}\\|[0-9]\\{8\\}T[0-9]\\{6\\}\\)__\\(.+\\)\\.org\\'"
  "Loose regexp matching a managed filename.

This regexp is intentionally permissive; use `chai-library--parse-managed-filename' 
to extract fields safely.

Groups:
1: ID
2: Rest after ID__ (including author/title/keywords/state)")

;;; Data Structure

(cl-defstruct (chai-book (:constructor chai-book-create)
	                         (:copier nil))
  id          ; String: 14-digit timestamp (or nil for unmanaged)
  author      ; String
  title       ; String
  keywords    ; List of strings
  status      ; Symbol: 'unread, 'reading, 'done, 'archived, 'unmanaged
  rating      ; Integer: 0-5
  file-path   ; Absolute path
  modified    ; Time object
  )

;;; Filename Parsing & Formatting

(defconst chai-library-managed-filename-format
  "ID__Title[==Keywords][--Status[-Rating]].org  or  ID__Author__Title[...]")

(defconst chai-library-managed-filename-separators-help
  "Separators: __ metadata, == keywords, -- status-rating"
  "Help text for separators used in managed filenames.")

(defcustom chai-library-status-choices '(unread reading done archived)
  "Statuses offered by interactive commands in Chai Library."
  :type '(repeat symbol)
  :group 'chai-library)

(defcustom chai-library-open-status-icons nil
  "Whether to show status icons in the list."
  :type 'boolean
  :group 'chai-library)

(defcustom chai-library-open-unread-status nil
  "Whether to show the \"unread\" status label in the list.

When nil, and the entry has no rating, unread is treated as the implicit
default and is not shown."
  :type 'boolean
  :group 'chai-library)

;; NOTE: The following variables are reserved for future UI enhancements:
;; - chai-library-use-fixed-pitch: for font control
;; - chai-library-default-sort-key/sort-desc: for customizable default sorting
;; - chai-library-table-column-widths: for configurable column widths
;; These are not currently used but kept for backward compatibility.

(defun chai-library--generate-id ()
  "Generate a new ID string using `chai-library-id-format'."
  (format-time-string chai-library-id-format))

(defun chai-library--normalize-metadata-string (s)
  "Normalize metadata string S.

Returns nil when S is empty or a placeholder (e.g. \"-\")."
  (let ((s (string-trim (or s ""))))
    (cond
     ((string-empty-p s) nil)
     ((string-match-p "\\`[-_]+\\'" s) nil)
     (t s))))

(defun chai-library--sanitize-filename-component (s fallback)
  "Sanitize S for use in managed filenames.

Keeps Unicode characters, but removes path separators and Chai's own
separators. If the result is empty, return FALLBACK."
  (let ((s (string-trim (or s ""))))
    (setq s (replace-regexp-in-string "[[:space:]]+" "-" s))
    (setq s (replace-regexp-in-string "[/\\:*?\"<>|]+" "-" s))
    (setq s (replace-regexp-in-string "__+" "-" s))
    (setq s (replace-regexp-in-string "==+" "-" s))
    (setq s (replace-regexp-in-string "--+" "-" s))
    (setq s (replace-regexp-in-string "-+" "-" s))
    (setq s (replace-regexp-in-string "\\`-+\\|-+\\'" "" s))
    (if (string-empty-p s) fallback s)))

(defun chai-library--read-file-metadata (file-path)
  "Read file-level metadata from FILE-PATH.

Returns a plist: (:id :title :author :keywords :status :rating).
ID is read from the first headline's PROPERTIES drawer :ID: (preferred)
or from legacy #+PROPERTY: ID format (backward compatibility)."
  (condition-case _
      (with-temp-buffer
        (insert-file-contents file-path nil 0 4096)
        (org-mode)
        (let ((case-fold-search t)
              id title author filetags status rating)
          ;; Read #+TITLE, #+AUTHOR, #+FILETAGS from file keywords
          (goto-char (point-min))
          (when (re-search-forward "^#\\+TITLE:[ \t]*\\(.*\\)$" nil t)
            (setq title (string-trim (match-string 1))))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+AUTHOR:[ \t]*\\(.*\\)$" nil t)
            (setq author (string-trim (match-string 1))))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+FILETAGS:[ \t]*\\(.*\\)$" nil t)
            (setq filetags (string-trim (match-string 1))))
          ;; Read ID: prefer drawer format, fall back to legacy #+PROPERTY: ID
          (setq id (or (chai-library--read-drawer-property "ID")
                       (progn
                         (goto-char (point-min))
                         (when (re-search-forward "^#\\+PROPERTY:[ \t]*\\(CHAI_ID\\|ID\\)[ \t]+\\(.*\\)$" nil t)
                           (string-trim (match-string 2))))))
          ;; Read STATUS and RATING from drawer or legacy #+PROPERTY
          (setq status (or (chai-library--read-drawer-property "STATUS")
                           (progn
                             (goto-char (point-min))
                             (when (re-search-forward "^#\\+PROPERTY:[ \t]*STATUS[ \t]+\\(.*\\)$" nil t)
                               (string-trim (match-string 1))))))
          (setq rating (or (chai-library--read-drawer-property "RATING")
                           (progn
                             (goto-char (point-min))
                             (when (re-search-forward "^#\\+PROPERTY:[ \t]*RATING[ \t]+\\([0-5]\\)\\b" nil t)
                               (match-string 1)))))
          (list
           :id (when (and (stringp id) (string-match-p chai-library-id-regexp id)) id)
           :title (when (and (stringp title) (not (string-empty-p title))) title)
           :author (when (and (stringp author) (not (string-empty-p author))) author)
           :keywords (when (and (stringp filetags) (not (string-empty-p filetags)))
                       (seq-remove
                        #'string-empty-p
                        (split-string (replace-regexp-in-string "[ \t]" "" filetags) ":" t)))
           :status (when (and (stringp status) (not (string-empty-p status)))
                     (intern status))
           :rating (when (and (stringp rating) (not (string-empty-p rating)))
                     (string-to-number rating)))))
    (error (list :id nil :title nil :author nil :keywords nil :status nil :rating nil))))


(defun chai-library--read-drawer-property (property)
  "Read PROPERTY from first headline's PROPERTIES drawer.
PROPERTY should be a string like \"ID\" or \"CUSTOM_ID\".
Returns the property value or nil if not found."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\*+ " nil t)
      (let ((case-fold-search t))
        (when (re-search-forward "^[ 	]*:PROPERTIES:" nil t)
          (let ((end (save-excursion
                       (re-search-forward "^[ 	]*:END:" nil t)
                       (point))))
            (goto-char (line-beginning-position 0))
            (when (re-search-forward (format "^[ 	]*:%s:[ 	]*\\(.*\\)$" property) end t)
              (string-trim (match-string 1)))))))))

(defun chai-library--upsert-org-keyword (keyword value)
  "Upsert #+KEYWORD: VALUE at file top."
  (let ((case-fold-search t)
        (keyword (upcase keyword)))
    (goto-char (point-min))
    (if (re-search-forward (format "^#\\+%s:[ 	]*\\(.*\\)$" (regexp-quote keyword)) nil t)
        (replace-match (format "#+%s: %s" keyword value) t t)
      (goto-char (point-min))
      (insert (format "#+%s: %s\n" keyword value)))))

(defun chai-library--upsert-drawer-property (name value)
  "Upsert property NAME with VALUE in first headline's PROPERTIES drawer.
Creates the drawer if it doesn't exist. Uses standard Org-mode drawer format."
  (let ((case-fold-search t))
    (goto-char (point-min))
    ;; Find first headline
    (if (not (re-search-forward "^\\*+ " nil t))
        (error "No headline found in file")
      ;; Move to after headline but before content
      (let ((headline-end (line-end-position)))
        ;; Check if there's already a PROPERTIES drawer
        (forward-line 1)
        (if (looking-at "^[ 	]*:PROPERTIES:")
            ;; Update existing property
            (let ((drawer-end (save-excursion
                                (re-search-forward "^[ 	]*:END:" nil t)
                                (line-beginning-position))))
              (if (re-search-forward (format "^[ 	]*:%s:[ 	]*\\(.*\\)$" name) drawer-end t)
                  (replace-match (format ":%s: %s" name value) t t)
                ;; Insert new property before :END:
                (goto-char drawer-end)
                (insert (format ":%s: %s\n" name value))))
          ;; Create new PROPERTIES drawer
          (goto-char headline-end)
          (insert (format "\n:PROPERTIES:\n:%s: %s\n:END:" name value)))))))

(defun chai-library--ensure-book-structure (file-path id)
  "Ensure FILE-PATH has proper book structure with ID.
If the file has no headline, creates one from the filename.
Returns the buffer."
  (let ((buf (find-file-noselect file-path))
        (file-title (file-name-base file-path)))
    (with-current-buffer buf
      (org-mode)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (if (re-search-forward "^\\*+ " nil t)
            ;; Has headline - add/update ID in drawer
            (progn
              (org-back-to-heading t)
              (chai-library--upsert-drawer-property "ID" id))
          ;; No headline - create structure
          (let ((title file-title))
            ;; Try to extract title from #+TITLE if exists
            (goto-char (point-min))
            (when (re-search-forward "^#\\+TITLE:[ 	]*\\(.*\\)$" nil t)
              (setq title (match-string 1)))
            ;; Find insertion point (after any #+ lines or at start)
            (goto-char (point-min))
            (while (and (not (eobp)) (looking-at "^#\\+"))
              (forward-line 1))
            ;; Insert headline with drawer
            (insert (format "* %s\n:PROPERTIES:\n:ID: %s\n:END:\n\n" 
                            (string-trim title) id))))
        (save-buffer)
        buf))))

(defun chai-library--upsert-org-property (name value)
  "Upsert #+PROPERTY: NAME VALUE at file top.
Deprecated: Use `chai-library--upsert-drawer-property' for standard Org drawer format."
  (let ((case-fold-search t)
        (name (upcase name)))
    (goto-char (point-min))
    (if (re-search-forward (format "^#\\+PROPERTY:[ 	]*%s[ 	]*\\(.*\\)$" (regexp-quote name)) nil t)
        (replace-match (format "#+PROPERTY: %s %s" name value) t t)
      (goto-char (point-min))
      (insert (format "#+PROPERTY: %s %s\n" name value)))))

(defun chai-library--ensure-file-id (file-path)
  "Ensure FILE-PATH has a valid ID property in first headline's drawer.
If the file has no headline, creates one with the ID in a PROPERTIES drawer."
  (let* ((base (file-name-nondirectory file-path))
         (parsed (chai-library--parse-managed-filename base))
         (id-from-name (and parsed (nth 0 parsed)))
         (meta (chai-library--read-file-metadata file-path))
         (id-from-meta (plist-get meta :id))
         (id (cond
              ((and (stringp id-from-name) (string-match-p chai-library-id-regexp id-from-name)) id-from-name)
              ((and (stringp id-from-meta) (string-match-p chai-library-id-regexp id-from-meta)) id-from-meta)
              (t (chai-library--generate-id)))))
    ;; Use new function that handles files without headlines
    (chai-library--ensure-book-structure file-path id)
    id))

;;; Link Integration

(defun chai-library-find-by-id (id)
  "Find a book in the library by ID.
Returns the `chai-book' struct or nil."
  (let ((books (chai-library-scan))) ;; scan uses cache efficiently
    (seq-find (lambda (b) (equal (chai-book-id b) id)) books)))

(defun chai-library-open-book-by-id (id)
  "Open the book corresponding to ID.
Used by `chai:' links."
  (let ((book (chai-library-find-by-id id)))
    (if book
        (find-file (chai-book-file-path book))
      (message "Chai Library: Book with ID %s not found." id))))

(defun chai-library--parse-status-rating (str)
  "Parse 'status-rating' string (e.g. 'reading-4') into (status . rating)."
  (let ((s (string-trim (or str ""))))
    (cond
     ((string-empty-p s) (cons nil nil))
     ((string-match "\\`\\([a-z]+\\)\\(?:-\\([0-5]\\)\\)?\\'" s)
      (let* ((status (intern (match-string 1 s)))
             (rating-str (match-string 2 s))
             (rating (and rating-str (string-to-number rating-str))))
        (when (and (numberp rating) (= rating 0))
          (setq rating nil))
        (cons status rating)))
     (t (cons nil nil)))))

(defun chai-library--format-status-rating (status rating)
  "Format status and rating."
  (let* ((status (or status 'unread))
         (rating (and (numberp rating) (> rating 0) rating)))
    (if rating
        (format "%s-%d" (symbol-name status) rating)
      (symbol-name status))))

(defun chai-library--format-state-suffix (status rating)
  "Return a filename suffix."
  (let* ((rating (and (numberp rating) (> rating 0) rating))
         (status (or status (and rating 'unread))))
    (cond
     ((and (null status) (null rating)) nil)
     ((and (eq status 'unread) (null rating) (not chai-library-open-unread-status)) nil)
     (t (concat "--" (chai-library--format-status-rating status rating))))))

(defun chai-library--managed-filename-current-state (filename)
  "Return (STATUS . RATING) from managed FILENAME."
  (let ((stem (file-name-base filename)))
    (if (string-match "--\\([a-z]+\\)\\(?:-\\([0-5]\\)\\)?\\'" stem)
        (let* ((status (intern (match-string 1 stem)))
               (rating-str (match-string 2 stem))
               (rating (and rating-str (string-to-number rating-str))))
          (when (and (numberp rating) (= rating 0))
            (setq rating nil))
          (cons status rating))
      (cons nil nil))))

(defun chai-library--managed-filename-set-state (filename status rating)
  "Return a new base filename with STATUS/RATING applied."
  (let* ((ext (file-name-extension filename t))
         (stem (file-name-base filename))
         (prefix (if (string-match "\\`\\(.*\\)--[a-z]+\\(?:-[0-5]\\)?\\'" stem)
                     (match-string 1 stem)
                   stem))
         (suffix (chai-library--format-state-suffix status rating))
         (new-stem (concat prefix (or suffix ""))))
    (concat new-stem (or ext ".org"))))

(defun chai-library--rename-managed-file-state (file-path status rating)
  "Rename FILE-PATH (managed)."
  (let* ((dir (file-name-directory file-path))
         (base (file-name-nondirectory file-path))
         (new-base (chai-library--managed-filename-set-state base status rating))
         (new-path (expand-file-name new-base dir)))
    (if (string= (file-truename file-path) (file-truename new-path))
        file-path
      (progn
        (when (file-exists-p new-path)
          (user-error "Target file already exists: %s" new-path))
        (rename-file file-path new-path)
        new-path))))

(defun chai-library--parse-managed-filename (filename)
  "Parse managed FILENAME into (ID AUTHOR TITLE KW-STR STATE-STR)."
  (when (and (stringp filename)
             (string-suffix-p ".org" filename t))
    (let* ((stem (file-name-base filename))
           (parts (split-string stem "__" t)))
      (when (>= (length parts) 2)
        (let* ((id (nth 0 parts))
               (author (when (>= (length parts) 3) (nth 1 parts)))
               (title-and-rest (if (>= (length parts) 3)
                                   (mapconcat #'identity (nthcdr 2 parts) "__")
                                 (nth 1 parts))))
          (when (string-match-p chai-library-id-regexp id)
            (let (main title kw-str state-str)
              (setq main title-and-rest)
              (when (string-match "\\`\\(.*\\)--\\([a-z]+\\(?:-[0-5]\\)?\\)\\'" main)
                (let ((before (match-string 1 main))
                      (state (match-string 2 main)))
                  (setq main before)
                  (setq state-str state)))
              (when (string-match "\\`\\(.*\\)==\\(.*\\)\\'" main)
                (setq title (match-string 1 main))
                (setq kw-str (match-string 2 main)))
              (unless title
                (setq title main))
              (list id author title kw-str state-str))))))))

(defun chai-library--parse-filename (filename dir &optional attrs)
  "Parse FILENAME in DIR into a `chai-book' struct."
  (let* ((file-path (expand-file-name filename dir))
         (attrs (or attrs (file-attributes file-path)))
         (mod-time (file-attribute-modification-time attrs)))
    (if-let ((parts (chai-library--parse-managed-filename filename)))
        (let* ((id (nth 0 parts))
               (author (or (nth 1 parts) ""))
               (title (nth 2 parts))
	               (kw-str (nth 3 parts))
	               (state-str (nth 4 parts))
	               (keywords (when (and kw-str (> (length kw-str) 0))
	                           (split-string kw-str "_" t)))
	               (state-rating (chai-library--parse-status-rating state-str)))
          (let* ((author (or (chai-library--normalize-metadata-string
                              (replace-regexp-in-string "-" " " author))
                             ""))
                 (title (or (chai-library--normalize-metadata-string
                             (replace-regexp-in-string "-" " " title))
                            "Untitled"))
                 (status (car state-rating))
                 (rating (cdr state-rating)))
            (when (and (eq status 'unread) (null rating) (not chai-library-open-unread-status))
              (setq status nil))
            (chai-book-create
             :id id
             :author author
             :title title
             :keywords keywords
             :status status
             :rating rating
             :file-path file-path
             :modified mod-time)))
      (chai-book-create
       :id nil
       :author ""
       :title (file-name-base filename)
       :keywords nil
       :status 'unmanaged
       :rating nil
       :file-path file-path
       :modified mod-time))))

(defun chai-library--generate-filename (book)
  "Generate standard filename from BOOK struct."
  (let* ((safe-author (chai-library--sanitize-filename-component
                       (chai-book-author book) ""))
         (safe-title (chai-library--sanitize-filename-component
                      (chai-book-title book) "Untitled"))
         (kw-part (if (chai-book-keywords book)
                      (concat "==" (mapconcat #'identity (chai-book-keywords book) "_"))
                    ""))
         (state-part (or (chai-library--format-state-suffix
                          (chai-book-status book)
                          (chai-book-rating book))
                         "")))
    (if (string-empty-p safe-author)
        (format "%s__%s%s%s.org" (chai-book-id book) safe-title kw-part state-part)
      (format "%s__%s__%s%s%s.org" (chai-book-id book) safe-author safe-title kw-part state-part))))

(defun chai-library-scan (&optional force)
  "Scan library directory and return books."
  (unless (file-exists-p chai-library-directory)
    (make-directory chai-library-directory t))
  (let* ((dir-attrs (file-attributes chai-library-directory))
         (dir-mtime (file-attribute-modification-time dir-attrs)))
    (if (and (not force) chai-library--cache-books (equal dir-mtime chai-library--cache-mtime))
        chai-library--cache-books
      (let* ((files (directory-files-and-attributes chai-library-directory nil "\\.org\\'"))
             (filenames (make-hash-table :test 'equal))
             (books nil))
        (dolist (item files)
          (let* ((filename (car item))
                 (attrs (cdr item))
                 (f-mtime (file-attribute-modification-time attrs))
                 (cached (gethash filename chai-library--cache-hash)))
            (puthash filename t filenames)
            (if (and (not force) cached (equal f-mtime (chai-book-modified cached)))
                (push cached books)
              (when-let ((new-book (chai-library--parse-filename filename chai-library-directory attrs)))
                (puthash filename new-book chai-library--cache-hash)
                (push new-book books)))))
        (maphash (lambda (k _v) (unless (gethash k filenames) (remhash k chai-library--cache-hash)))
                 chai-library--cache-hash)
        (let ((sorted (sort books (lambda (a b) (time-less-p (chai-book-modified b) (chai-book-modified a))))))
          (setq chai-library--cache-mtime dir-mtime
                chai-library--cache-books sorted)
          sorted)))))

;;; Import Command

(defcustom chai-library-python-executable "python"
  "Python executable to use for running the conversion script."
  :type 'string
  :group 'chai-library)

(defun chai-library--import-sentinel (process event)
  "Sentinel for import PROCESS, handling EVENT."
  (when (string-match-p "finished\\|exited" event)
    (let ((buf (process-buffer process)))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format "\n--- %s ---\n" 
                       (if (= 0 (process-exit-status process))
                           "Import ✓" "Import ✗"))))
      (message "Chai: Import %s" 
               (if (= 0 (process-exit-status process)) "Finished" "Failed"))
      ;; Refresh library if open
      (when-let ((lib-buf (get-buffer "*Chai Library*")))
        (with-current-buffer lib-buf
          (when (fboundp 'chai-library-refresh)
            (chai-library-refresh)))))))

;;;###autoload
(defun chai-library-import (&optional single-file)
  "Import external files into Chai Library.
Without prefix arg, import all files from `chai-library-import-inbox'.
With prefix arg SINGLE-FILE, prompt to select a single file to import."
  (interactive "P")
  (unless (file-exists-p chai-library--python-script)
    (user-error "Convert script doesn't exist: %s" chai-library--python-script))
  
  ;; Ensure directories exist
  (dolist (dir (list chai-library-directory chai-library-import-inbox chai-library-import-archive))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  
  (let* ((file-to-import
          (when single-file
            (read-file-name "Choose File: " nil nil t nil
                           (lambda (f) (or (file-directory-p f)
                                          (member (downcase (file-name-extension f t))
                                                  '(".pdf" ".md" ".epub" ".html")))))))
         (args (if file-to-import
                   (list "--file" (expand-file-name file-to-import)
                         "--reference" (expand-file-name chai-library-directory))
                 (list "--temp" (expand-file-name chai-library-import-inbox)
                       "--reference" (expand-file-name chai-library-directory)
                       "--archive" (expand-file-name chai-library-import-archive))))
         (buf (get-buffer-create "*Chai Import*")))
    
    ;; Check inbox has files (for batch mode)
    (unless file-to-import
      (let ((files (seq-filter (lambda (f) (not (string-prefix-p "." f)))
                               (directory-files chai-library-import-inbox nil nil t))))
        (when (null files)
          (user-error "Inbox 为空: %s" chai-library-import-inbox))))
    
    ;; Setup buffer
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (special-mode)
      (setq-local buffer-read-only nil)
      (insert (format "[%s] Import: %s\n\n"
                      (format-time-string "%H:%M:%S")
                      (abbreviate-file-name (or file-to-import chai-library-import-inbox)))))
    
    ;; Display buffer
    (display-buffer buf '(display-buffer-at-bottom . ((window-height . 12))))
    
    ;; Start process
    (let ((proc (apply #'start-process "chai-import" buf
                       chai-library-python-executable
                       chai-library--python-script
                       args)))
      (set-process-sentinel proc #'chai-library--import-sentinel)
      (message "Chai: Import Start..."))))

(provide 'chai-library)

;;; chai-library.el ends here
