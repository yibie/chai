;;; chai.el --- Semantic highlights and export for Chai -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Keywords: outlines, hypermedia, knowledge-management, reading
;; URL: https://github.com/yibie/chai

;;; Commentary:

;; This package provides a lightweight highlighting and export layer for
;; reading and digesting knowledge in Org mode.
;;
;; While reading a source document, mark important fragments with semantic
;; chai links such as [[chai:key][text]] or [[chai:idea:note][text]].  The
;; package can then export the collected highlights as plain text or clean
;; Org markup to the kill ring, ready to be pasted into your own notes
;; (Org-roam, Denote, etc.).
;;
;; Highlights are ordinary Org links, so they survive in any Org buffer
;; without requiring a dedicated minor mode.

;;; Code:

(require 'org)
(require 'org-element)
(require 'ob-core)
(require 'cl-lib)
(require 'subr-x)

(declare-function chirp-entry-at-point "chirp-core")
(defvar eww-current-title)
(defvar eww-current-url)

;;; Customization

(defgroup chai nil
  "Customization group for Chai (拆) package."
  :group 'org)

(defcustom chai-export-preview-directory
  (expand-file-name "chai/exports/" user-emacs-directory)
  "Directory where `chai-export-preview' suggests saving preview files.
Each preview file is named `<source-file-base>_chai.org'."
  :type 'directory
  :group 'chai)

(defcustom chai-export-heading-file nil
  "Optional file associated with the temporary `chai-export-heading' buffer.
When nil, the buffer remains unsaved and can be copied into another note.
When set, normal Emacs saving writes the edited heading export to this file."
  :type '(choice (const :tag "Keep as temporary buffer" nil) file)
  :group 'chai)

;;; Faces

(defgroup chai-faces nil
  "Faces for Chai highlighting."
  :group 'chai)

(defface chai-highlight-important
  '((((background dark))  :background "#5a2d2d" :foreground "#F8F8F2" :extend nil)
    (((background light)) :background "#FFE0B2" :foreground "#000000" :extend nil))
  "Face for \\='important\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-idea
  '((((background dark))  :background "#1e4040" :foreground "#F8F8F2" :extend nil)
    (((background light)) :background "#B2DFDB" :foreground "#000000" :extend nil))
  "Face for \\='idea\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-question
  '((((background dark))  :background "#1e3050" :foreground "#F8F8F2" :extend nil)
    (((background light)) :background "#BBDEFB" :foreground "#000000" :extend nil))
  "Face for \\='question\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-critical
  '((((background dark))  :background "#4a3010" :foreground "#F8F8F2" :extend nil)
    (((background light)) :background "#FFB74D" :foreground "#000000" :extend nil))
  "Face for \\='critical\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-key
  '((((background dark))  :background "#4a4010" :foreground "#F8F8F2" :extend nil)
    (((background light)) :background "#FFE082" :foreground "#000000" :extend nil))
  "Face for \\='key\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-core
  '((((background dark))  :background "#5a2020" :foreground "#F8F8F2" :extend nil)
    (((background light)) :background "#FFCCBC" :foreground "#000000" :extend nil))
  "Face for \\='core\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-detail
  '((((background dark))  :background "#1a3a28" :foreground "#F8F8F2" :extend nil)
    (((background light)) :background "#DCEDC8" :foreground "#000000" :extend nil))
  "Face for \\='detail\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-example
  '((((background dark))  :background "#1a2e3a" :foreground "#F8F8F2" :extend nil)
    (((background light)) :background "#E3F2FD" :foreground "#000000" :extend nil))
  "Face for \\='example\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-hard
  '((((background dark))  :background "#2e1e3a" :foreground "#F8F8F2" :extend nil)
    (((background light)) :background "#EDE7F6" :foreground "#000000" :extend nil))
  "Face for \\='hard\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-block
  '((((background dark))  :background "#1e1e2a" :foreground "#F8F8F2" :extend nil)
    (((background light)) :background "#ECEFF1" :foreground "#000000" :extend nil))
  "Face for \\='block\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-view
  '((((background dark))  :background "#2a1e3a" :foreground "#F8F8F2" :extend nil)
    (((background light)) :background "#F3E5F5" :foreground "#000000" :extend nil))
  "Face for \\='view\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-outdated
  '((((background dark))  :foreground "#B0B0B0" :strike-through t :extend nil)
    (((background light)) :foreground "#9E9E9E" :strike-through t :extend nil))
  "Face for \\='outdated\\=' highlights."
  :group 'chai-faces)

(defcustom chai-highlight-types
  '(;; Original types
    ("important" . chai-highlight-important)
    ("idea"      . chai-highlight-idea)
    ("question"  . chai-highlight-question)
    ("critical"  . chai-highlight-critical)
    ;; Extended types
    ("key"       . chai-highlight-key)
    ("core"      . chai-highlight-core)
    ("detail"    . chai-highlight-detail)
    ("example"   . chai-highlight-example)
    ("hard"      . chai-highlight-hard)
    ("block"     . chai-highlight-block)
    ("view"      . chai-highlight-view)
    ("outdated"  . chai-highlight-outdated))
  "Alist mapping highlight types to faces.
Each element is a cons cell (TYPE . FACE), where TYPE is a string
identifier for the highlight (used in the link) and FACE is the
face symbol to use for display."
  :type '(alist :key-type (string :tag "Type Name")
                :value-type (face :tag "Face"))
  :group 'chai)

;;; Link Protocol

(defun chai-link-face (path)
  "Return the face for the given chai link PATH."
  (let ((type (car (split-string path ":"))))
    (or (cdr (assoc type chai-highlight-types))
        'org-link)))

(defun chai-link-follow (path)
  "Follow a chai link.
If PATH is an ID (optionally followed by ::LINE), open the corresponding book.
Otherwise, treated as a highlight tag (no action)."
  (if (string-match
       "\\`\\([0-9]\\{14\\}\\|[0-9]\\{8\\}T[0-9]\\{6\\}\\)\\(?:::\\([1-9][0-9]*\\)\\)?\\'"
       path)
      (let ((id (match-string 1 path))
            (line (match-string 2 path)))
        (if (fboundp 'chai-library-open-book-by-id)
            (let ((buffer (chai-library-open-book-by-id id)))
              (when (and line (bufferp buffer))
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (forward-line (1- (string-to-number line)))))
              buffer)
          (user-error "Chai Library not loaded. Cannot open book with ID: %s" id)))
    (message "Chai highlight: %s" path)))

(defun chai-link-export (path description backend)
  "Export a chai link.
PATH is the link path, DESCRIPTION is the link text,
BACKEND is the export backend."
  (cond
   ((eq backend 'html)
    (format "<span class=\"chai-highlight chai-%s\">%s</span>"
            (car (split-string path ":")) description))
   ((eq backend 'latex)
    (format "\\textbf{%s}" description))
   (t description)))

(defun chai-link-help-echo-at-point ()
  "Return help echo text for the chai link at point, or nil."
  (let ((elem (ignore-errors (org-element-context))))
    (when (and elem
               (eq (org-element-type elem) 'link)
               (string= (org-element-property :type elem) "chai"))
      (let* ((path (org-element-property :path elem))
             (parsed (chai-parse-link-path path))
             (type (car parsed))
             (note (cdr parsed)))
        (if note
            (format "Chai note (%s): %s" type note)
          (format "Chai: %s" type))))))

(defun chai-link-help-echo (window _object pos)
  "Return help echo text for the chai link at POS in WINDOW."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char pos)
      (chai-link-help-echo-at-point))))

(org-link-set-parameters "chai"
                         :face 'chai-link-face
                         :mouse-face 'highlight
                         :help-echo #'chai-link-help-echo
                         :follow #'chai-link-follow
                         :export #'chai-link-export)

;;; Mouse / Context Menu Actions

(defun chai--link-at-point-p (&optional pos)
  "Return non-nil if point (or POS) is on a chai link."
  (save-excursion
    (when pos (goto-char pos))
    (let ((elem (ignore-errors (org-element-context))))
      (and elem
           (eq (org-element-type elem) 'link)
           (string= (org-element-property :type elem) "chai")))))

(defun chai--link-text-at-point (&optional pos)
  "Return the description text of the chai link at point (or POS), or nil."
  (when (chai--link-at-point-p pos)
    (save-excursion
      (when pos (goto-char pos))
      (let* ((elem (org-element-context))
             (cbeg (org-element-property :contents-begin elem))
             (cend (org-element-property :contents-end elem)))
        (when (and cbeg cend)
          (buffer-substring-no-properties cbeg cend))))))

(defun chai--link-end (elem)
  "Return the end position of link ELEM, excluding trailing whitespace."
  (- (org-element-property :end elem)
     (or (org-element-property :post-blank elem) 0)))

(defun chai-mouse-change-type (&optional pos)
  "Change the type of the chai highlight at point (or POS)."
  (interactive)
  (unless (chai--highlight-at-point-p pos)
    (user-error "No chai highlight at point"))
  (save-excursion
    (when pos (goto-char pos))
    (if-let* ((block (chai--block-at-point)))
        (let* ((type (chai--block-type block))
               (note (chai--block-note block))
               (new-type (completing-read "New type: "
                                          (mapcar #'car chai-highlight-types)
                                          nil t type)))
          (chai--replace-block-header block new-type note)
          (chai--after-org-structure-change)
          (chai-refresh-annotations))
      (let* ((elem (org-element-context))
             (path (org-element-property :path elem))
             (parsed (chai-parse-link-path path))
             (type (car parsed))
             (note (cdr parsed))
             (text (or (chai--link-text-at-point) ""))
             (begin (org-element-property :begin elem))
             (end (chai--link-end elem))
             (new-type (completing-read "New type: "
                                        (mapcar #'car chai-highlight-types)
                                        nil t type)))
        (chai-clear-annotations begin end)
        (delete-region begin end)
        (goto-char begin)
        (insert (format "[[chai:%s%s][%s]]"
                        new-type
                        (if note (concat ":" note) "")
                        text))
        (chai--after-org-structure-change)
        (chai-refresh-annotations)))))

(defun chai-mouse-edit-annotation (&optional pos)
  "Edit the annotation of the chai highlight at point (or POS)."
  (interactive)
  (unless (chai--highlight-at-point-p pos)
    (user-error "No chai highlight at point"))
  (save-excursion
    (when pos (goto-char pos))
    (if-let* ((block (chai--block-at-point)))
        (let* ((type (chai--block-type block))
               (note (chai--block-note block))
               (new-note (read-string "Note: " (or note ""))))
          (chai--replace-block-header block type new-note)
          (chai--after-org-structure-change)
          (chai-refresh-annotations))
      (let* ((elem (org-element-context))
             (path (org-element-property :path elem))
             (parsed (chai-parse-link-path path))
             (type (car parsed))
             (note (cdr parsed))
             (text (or (chai--link-text-at-point) ""))
             (begin (org-element-property :begin elem))
             (end (chai--link-end elem))
             (new-note (read-string "Note: " (or note ""))))
        (chai-clear-annotations begin end)
        (delete-region begin end)
        (goto-char begin)
        (if (string-empty-p new-note)
            (insert (format "[[chai:%s][%s]]" type text))
          (insert (format "[[chai:%s:%s][%s]]" type new-note text)))
        (chai--after-org-structure-change)
        (chai-refresh-annotations)))))

(defun chai-mouse-remove-highlight (&optional pos)
  "Remove the chai highlight at point (or POS)."
  (interactive)
  (chai-remove-highlight pos))

(defun chai-mouse-copy-text (&optional pos)
  "Copy the highlighted text at point (or POS)."
  (interactive)
  (let ((text (or (chai--highlight-text-at-point pos)
                  (user-error "No chai highlight at point"))))
    (kill-new text)
    (message "Copied: %s" text)))

(defun chai--context-menu-highlight-key (type)
  "Return the context menu key for highlight TYPE."
  (intern (format "chai-highlight-type-%s" type)))

(defun chai-context-menu (menu click)
  "Populate MENU with Chai actions for CLICK event.
Adds a basic Chai action in Org buffers, highlight actions when right-clicking
a Chai highlight, and create-highlight actions when a region is active.

Menu commands capture the clicked position so they work even if point has moved
after the menu was opened."
  (save-excursion
    (let* ((pos (posn-point (event-start click)))
           (region-p (use-region-p))
           (region-start (and region-p (region-beginning)))
           (region-end (and region-p (region-end)))
           (on-highlight (progn (goto-char pos) (chai--highlight-at-point-p))))
      (define-key-after menu [chai-separator]
        '(menu-item "--"))
      (define-key-after menu [chai-add-comment]
        (list 'menu-item "Chai: add comment"
              (lambda () (interactive)
                (if region-p
                    (chai-insert-comment region-start region-end)
                  (chai-insert-comment)))
              :help "Add a comment block, wrapping region if active"))
      (when on-highlight
        (define-key-after menu [chai-change-type]
          (list 'menu-item "Chai: change type"
                (lambda () (interactive) (chai-mouse-change-type pos))
                :help "Change the highlight type"))
        (define-key-after menu [chai-edit-annotation]
          (list 'menu-item "Chai: edit annotation"
                (lambda () (interactive) (chai-mouse-edit-annotation pos))
                :help "Edit the highlight annotation"))
        (define-key-after menu [chai-remove]
          (list 'menu-item "Chai: remove highlight"
                (lambda () (interactive) (chai-mouse-remove-highlight pos))
                :help "Remove this highlight"))
        (define-key-after menu [chai-copy-text]
          (list 'menu-item "Chai: copy text"
                (lambda () (interactive) (chai-mouse-copy-text pos))
                :help "Copy the highlighted text")))
      (define-key-after menu [chai-region-separator]
        '(menu-item "--"))
      (define-key-after menu [chai-highlight-region]
        (list 'menu-item "Chai: highlight region..."
              (lambda () (interactive)
                (if region-p
                    (chai-highlight-region
                     region-start region-end
                     (completing-read "Highlight type: " (mapcar #'car chai-highlight-types)))
                  (call-interactively #'chai-highlight-region)))
              :help "Highlight region with any type"))
      (dolist (type-def chai-highlight-types)
        (let* ((type (car type-def))
               (menu-key (vector (chai--context-menu-highlight-key type))))
          (define-key-after menu menu-key
            (list 'menu-item (format "Chai: highlight %s" type)
                  (lambda () (interactive)
                    (if region-p
                        (chai-highlight-region region-start region-end type)
                      (if (use-region-p)
                          (chai-highlight-region (region-beginning) (region-end) type)
                        (user-error "No region selected"))))
                  :help (format "Highlight region as %s" type)))))
      (define-key-after menu [chai-highlight-separator2]
        '(menu-item "--"))
      (define-key-after menu [chai-highlight-annotate]
        (list 'menu-item "Chai: highlight with note..."
              (lambda () (interactive)
                (if region-p
                    (let* ((type (completing-read "Highlight type: " (mapcar #'car chai-highlight-types)))
                           (note (read-string "Note: ")))
                      (if (string-empty-p note)
                          (user-error
                           "Note cannot be empty; use chai-highlight-region for plain highlights")
                        (chai-highlight-annotate region-start region-end type note)))
                  (call-interactively #'chai-highlight-annotate)))
              :help "Highlight region with a note"))))
  menu)

;;; Highlight Commands

(defun chai--after-org-structure-change ()
  "Reset Org element cache after Chai changes Org syntax."
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-element-cache-reset))
    (org-element-cache-reset)))

(defun chai--insert-highlight-block (start end type &optional note)
  "Replace START..END with a Chai source block of TYPE and optional NOTE."
  (let ((text (buffer-substring-no-properties start end)))
    (delete-region start end)
    (goto-char start)
    (unless (bolp)
      (insert "\n"))
    (insert (chai--block-header-line type note) "\n" text)
    (unless (bolp)
      (insert "\n"))
    (insert "#+END_CHAI")
    (unless (eolp)
      (insert "\n"))
    (chai--after-org-structure-change)
    (chai-refresh-annotations)))

(defun chai--insert-highlight-link (start end type &optional note)
  "Replace START..END with a Chai link of TYPE and optional NOTE."
  (let ((text (buffer-substring-no-properties start end)))
    (delete-region start end)
    (goto-char start)
    (insert (format "[[chai:%s%s][%s]]"
                    type
                    (if (and note (not (string-empty-p note)))
                        (concat ":" note)
                      "")
                    text))
    (chai--after-org-structure-change)
    (chai-refresh-annotations)))

(defun chai--insert-highlight (start end type &optional note)
  "Replace START..END with a Chai link or source block.
Selections without a literal newline use a Chai link; selections containing a
newline use a source block so their original line structure remains intact."
  (if (string-match-p "\n" (buffer-substring-no-properties start end))
      (chai--insert-highlight-block start end type note)
    (chai--insert-highlight-link start end type note)))

;;;###autoload
(defun chai-highlight-region (start end type)
  "Highlight the region from START to END with TYPE."
  (interactive
   (if (use-region-p)
       (list (region-beginning)
             (region-end)
             (completing-read "Highlight type: " (mapcar #'car chai-highlight-types)))
     (user-error "No region selected")))
  (chai--insert-highlight start end type))

;;;###autoload
(defun chai-highlight-annotate (start end type note)
  "Highlight the region from START to END with TYPE and NOTE."
  (interactive
   (if (use-region-p)
       (let* ((type (completing-read "Highlight type: " (mapcar #'car chai-highlight-types)))
              (note (read-string "Note: ")))
         (if (string-empty-p note)
             (user-error "Note cannot be empty; use chai-highlight-region for plain highlights")
           (list (region-beginning) (region-end) type note)))
     (user-error "No region selected")))
  (chai--insert-highlight start end type note))

;;;###autoload
(defun chai-remove-highlight (&optional pos)
  "Remove the chai highlight at point, restoring the plain text.
If POS is non-nil, remove the highlight at that position instead.
Works for Chai links and source blocks."
  (interactive)
  (save-excursion
    (when pos (goto-char pos))
    (if-let* ((block (chai--block-at-point)))
        (let ((begin (org-element-property :begin block))
              (end (chai--element-end block))
              (text (chai--block-text block)))
          (delete-region begin end)
          (goto-char begin)
          (insert text)
          (chai--after-org-structure-change)
          (chai-refresh-annotations)
          (goto-char begin))
      (let ((elem (org-element-context)))
        (unless (and elem
                     (eq (org-element-type elem) 'link)
                     (string= (org-element-property :type elem) "chai"))
          (user-error "No chai highlight at point"))
        (let* ((begin (org-element-property :begin elem))
               (end   (chai--link-end elem))
               (text  (buffer-substring-no-properties
                       (org-element-property :contents-begin elem)
                       (org-element-property :contents-end elem))))
          (chai-clear-annotations begin end)
          (delete-region begin end)
          (insert text)
          (chai--after-org-structure-change)
          (chai-refresh-annotations)
          (goto-char begin))))))

;;; Annotation Rendering

(defun chai-parse-link-path (path)
  "Parse a chai link PATH and return (TYPE . NOTE).
PATH format is either \\='TYPE\\=' or \\='TYPE:NOTE\\='."
  (let ((parts (split-string path ":" nil)))
    (cons (car parts)
          (when (cdr parts)
            (mapconcat #'identity (cdr parts) ":")))))

(defun chai--chai-block-p (element)
  "Return non-nil when ELEMENT is a Chai source special block."
  (and element
       (eq (org-element-type element) 'special-block)
       (string= (upcase (org-element-property :type element)) "CHAI")))

(defun chai--block-at-point (&optional pos)
  "Return the Chai source block at point or POS, or nil."
  (save-excursion
    (when pos (goto-char pos))
    (let* ((element (ignore-errors (org-element-context)))
           (block (or (and (chai--chai-block-p element) element)
                      (and element
                           (org-element-lineage element '(special-block) t)))))
      (and (chai--chai-block-p block) block))))

(defun chai--block-params (block)
  "Return BLOCK's parsed Chai header parameters."
  (org-babel-parse-header-arguments
   (or (org-element-property :parameters block) "")))

(defun chai--block-type (block)
  "Return BLOCK's Chai highlight type, or nil."
  (cdr (assq :type (chai--block-params block))))

(defun chai--block-note (block)
  "Return BLOCK's Chai note, or nil."
  (cdr (assq :note (chai--block-params block))))

(defun chai--block-text (block)
  "Return BLOCK's text without the structural newline before its end marker."
  (let ((begin (org-element-property :contents-begin block))
        (end (org-element-property :contents-end block)))
    (when (and begin end)
      (string-remove-suffix
       "\n" (buffer-substring-no-properties begin end)))))

(defun chai--element-end (element)
  "Return ELEMENT's end position without trailing blank lines."
  (- (org-element-property :end element)
     (or (org-element-property :post-blank element) 0)))

(defun chai--block-header-line (type &optional note)
  "Return a Chai source block header for TYPE and optional NOTE."
  (concat "#+BEGIN_CHAI :type " type
          (if (and note (not (string-empty-p note)))
              (format " :note %S" note)
            "")))

(defun chai--replace-block-header (block type &optional note)
  "Update BLOCK's Chai TYPE and optional NOTE."
  (save-excursion
    (goto-char (org-element-property :begin block))
    (delete-region (line-beginning-position) (line-end-position))
    (insert (chai--block-header-line type note))))

(defun chai--highlight-at-point-p (&optional pos)
  "Return non-nil when point or POS is on a Chai link or source block."
  (and (chai--highlight-entry-at-point pos) t))

(defun chai--highlight-text-at-point (&optional pos)
  "Return Chai highlight text at point or POS, or nil."
  (plist-get (chai--highlight-entry-at-point pos) :text))

(defun chai--highlight-entry-at-point (&optional pos)
  "Return the normalized Chai highlight entry at point or POS, or nil."
  (save-excursion
    (when pos (goto-char pos))
    (let ((element (ignore-errors (org-element-context))))
      (or (chai--highlight-entry-from-element element)
          (when-let* ((block (chai--block-at-point pos)))
            (chai--highlight-entry-from-element block))))))

(defun chai-clear-annotations (&optional start end)
  "Remove all chai annotation overlays in region START to END."
  (remove-overlays (or start (point-min))
                   (or end (point-max))
                   'chai-note-ov t))

(defvar-local chai--block-highlight-overlays nil
  "Face overlays currently applied to Chai source blocks.")

(defun chai--refresh-face-specs ()
  "Recalculate configured highlight faces for the current frame."
  (dolist (face (delete-dups (mapcar #'cdr chai-highlight-types)))
    (when (and (facep face)
               (not (get face 'face-modified)))
      (face-spec-recalc face nil))))

(defun chai--clear-block-highlights ()
  "Remove face overlays from Chai source blocks in the current buffer."
  (mapc #'delete-overlay chai--block-highlight-overlays)
  (setq chai--block-highlight-overlays nil))

(defun chai--render-block-highlights (entries)
  "Apply Org's quote face to Chai block contents in normalized ENTRIES."
  (chai--clear-block-highlights)
  (dolist (entry entries)
    (when (and (eq (plist-get entry :storage) 'block)
               (plist-get entry :contents-beg)
               (plist-get entry :contents-end))
      (let ((overlay (make-overlay (plist-get entry :contents-beg)
                                   (plist-get entry :contents-end))))
        (overlay-put overlay 'face 'org-quote)
        (overlay-put overlay 'chai-block-ov t)
        (overlay-put overlay 'evaporate t)
        (push overlay chai--block-highlight-overlays)))))

(defun chai--render-note-overlays (entries start end)
  "Create after-string overlays for normalized ENTRIES with notes in START..END."
  (dolist (entry entries)
    (let ((lbegin (plist-get entry :beg))
          (lend (plist-get entry :end))
          (type (plist-get entry :type))
          (note (plist-get entry :note)))
      (when (and note
                 lbegin
                 lend
                 (>= lbegin start)
                 (<= lend end))
        (let* ((face (or (cdr (assoc type chai-highlight-types)) 'default))
               (clean-note (substring-no-properties note))
               (body (propertize (concat " " clean-note)
                                 'face `(:inherit ,face
                                         :foreground "#888888"
                                         :slant italic
                                         :height 0.85)))
               (ov (make-overlay lend lend)))
          (overlay-put ov 'after-string body)
          (overlay-put ov 'chai-note-ov t)
          (overlay-put ov 'priority 90))))))

(defun chai--render-annotations ()
  "Render Chai annotations and source block faces from one parsed entry list."
  (chai--refresh-face-specs)
  (let ((entries (chai--collect-entries)))
    (chai-clear-annotations)
    (chai--render-note-overlays entries (point-min) (point-max))
    (chai--render-block-highlights entries)))

;;;###autoload
(defun chai-refresh-annotations ()
  "Refresh Chai annotations and source block faces in the current buffer."
  (interactive)
  (chai--render-annotations)
  (message "Chai highlights refreshed."))

;;; Context Panel

(defvar chai--context-panel-buf-name "*chai-context*"
  "Buffer name for the chai context panel.")

(defvar-local chai--context-panel-source-buf nil
  "The main buffer that the context panel is reflecting.")

(defvar-local chai--context-panel-timer nil
  "Idle timer for debounced context panel updates.")

(defun chai--install-context-menu-keys ()
  "Install Chai's context menu keys in the current buffer."
  (when (boundp 'context-menu-mode-map)
    (let ((map (copy-keymap (current-local-map))))
      (define-key map [down-mouse-3] (lookup-key context-menu-mode-map [down-mouse-3]))
      (define-key map [mouse-3] #'ignore)
      (use-local-map map))))

(defun chai--highlight-entry-from-element (element)
  "Return a normalized highlight plist for Chai ELEMENT, or nil."
  (cond
   ((and (eq (org-element-type element) 'link)
         (string= (org-element-property :type element) "chai"))
    (let* ((path (org-element-property :path element))
           (parsed (chai-parse-link-path path))
           (cbeg (org-element-property :contents-begin element))
           (cend (org-element-property :contents-end element))
           (begin (org-element-property :begin element)))
      (list :kind 'highlight
            :storage 'link
            :type (car parsed)
            :note (cdr parsed)
            :text (if (and cbeg cend)
                      (buffer-substring-no-properties cbeg cend)
                    "")
            :line (line-number-at-pos begin)
            :beg begin
            :end (chai--link-end element))))
   ((chai--chai-block-p element)
    (when-let* ((type (chai--block-type element))
                (text (chai--block-text element))
                (begin (org-element-property :begin element)))
      (let ((content-begin (org-element-property :contents-begin element)))
        (list :kind 'highlight
              :storage 'block
              :type type
              :note (chai--block-note element)
              :text text
              :line (line-number-at-pos (or content-begin begin))
              :beg begin
              :end (chai--element-end element)
              :contents-beg content-begin
              :contents-end (org-element-property :contents-end element)))))))

(defun chai--comment-entry-from-element (element)
  "Return a normalized comment plist for a CHAI_COMMENT ELEMENT, or nil."
  (when (and (eq (org-element-type element) 'special-block)
             (string= (upcase (org-element-property :type element))
                      "CHAI_COMMENT"))
    (let* ((cbeg (org-element-property :contents-begin element))
           (cend (org-element-property :contents-end element))
           (begin (org-element-property :begin element)))
      (list :kind 'comment
            :storage 'block
            :text (if (and cbeg cend)
                      (string-trim (buffer-substring-no-properties cbeg cend))
                    "")
            :line (line-number-at-pos begin)
            :beg begin))))

(defun chai--entry-before-p (left right)
  "Return non-nil when normalized entry LEFT precedes RIGHT in the source."
  (let ((left-line (or (plist-get left :line) 0))
        (right-line (or (plist-get right :line) 0)))
    (if (/= left-line right-line)
        (< left-line right-line)
      (< (or (plist-get left :beg) 0)
         (or (plist-get right :beg) 0)))))

(defun chai--collect-entries ()
  "Parse the current buffer into normalized Chai highlight/comment entries."
  (let ((tree (org-element-parse-buffer))
        entries)
    (org-element-map tree 'link
      (lambda (element)
        (when-let* ((entry (chai--highlight-entry-from-element element)))
          (push entry entries))))
    (org-element-map tree 'special-block
      (lambda (element)
        (when-let* ((entry (or (chai--highlight-entry-from-element element)
                               (chai--comment-entry-from-element element))))
          (push entry entries))))
    (sort entries #'chai--entry-before-p)))

(defun chai--collect-highlights ()
  "Return normalized Chai highlight entries in source order."
  (cl-remove-if-not
   (lambda (entry) (eq (plist-get entry :kind) 'highlight))
   (chai--collect-entries)))

(defun chai--collect-comments ()
  "Return normalized free-standing comment entries in source order."
  (cl-remove-if-not
   (lambda (entry) (eq (plist-get entry :kind) 'comment))
   (chai--collect-entries)))

;;;###autoload
(defun chai-insert-comment (&optional start end)
  "Insert a CHAI_COMMENT block.
If START and END are provided, wrap that range in the block.
Otherwise, wrap the active region or insert an empty block at point."
  (interactive)
  (let* ((range-p (or (and start end) (use-region-p)))
         (start (or start (if range-p (region-beginning) (point))))
         (end (or end (if range-p (region-end) (point))))
         (body (if range-p
                   (buffer-substring-no-properties start end)
                 ""))
         (empty-p (string-empty-p body)))
    (when range-p
      (delete-region start end))
    (goto-char start)
    (insert (if empty-p
                "#+BEGIN_CHAI_COMMENT\n#+END_CHAI_COMMENT\n"
              (format "#+BEGIN_CHAI_COMMENT\n%s\n#+END_CHAI_COMMENT\n" body)))
    (chai--after-org-structure-change)
    (forward-line -1)
    (when empty-p
      (end-of-line))))

;;;###autoload
(defun chai-add-comment (text)
  "Prompt for TEXT and insert it as a CHAI_COMMENT block at point."
  (interactive "sComment: ")
  (when (string-empty-p (string-trim text))
    (user-error "Comment cannot be empty"))
  (insert (format "#+BEGIN_CHAI_COMMENT\n%s\n#+END_CHAI_COMMENT\n" text))
  (chai--after-org-structure-change))

(defun chai--current-highlight-type ()
  "Return the Chai highlight type at point, or nil."
  (plist-get (chai--highlight-entry-at-point) :type))

(defun chai--render-context-panel (highlights current-type)
  "Render HIGHLIGHTS grouped by type into the context panel buffer.
CURRENT-TYPE is the type at point, used for highlighting the group header."
  (let ((buf (get-buffer-create chai--context-panel-buf-name))
        (groups '()))
    (dolist (type-def chai-highlight-types)
      (let* ((type    (car type-def))
             (entries (cl-remove-if-not
                       (lambda (h) (string= (plist-get h :type) type))
                       highlights)))
        (when entries
          (push (cons type entries) groups))))
    (setq groups (nreverse groups))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (group groups)
          (let* ((type    (car group))
                 (entries (cdr group))
                 (face    (or (cdr (assoc type chai-highlight-types)) 'default))
                 (header-face (if (string= type current-type)
                                  `(:inherit ,face :inverse-video t :weight bold)
                                `(:inherit ,face :weight bold))))
            (insert (propertize (format " %s (%d)\n" (upcase type) (length entries))
                                'face header-face))
            (dolist (entry entries)
              (let* ((note (plist-get entry :note))
                     (text (plist-get entry :text))
                     (lnum (plist-get entry :line)))
                (insert (propertize (format "  L%-4d " lnum)
                                    'face '(:foreground "#666666")))
                (insert (propertize text 'face `(:inherit ,face)))
                (insert "\n")
                (when note
                  (insert (propertize (format "        %s\n" note)
                                      'face '(:foreground "#888888"
                                              :slant italic
                                              :height 0.9))))))
            (insert "\n")))))
    buf))

(defun chai--update-context-panel ()
  "Update the context panel to reflect the current buffer state."
  (when (and chai--context-panel-source-buf
             (buffer-live-p chai--context-panel-source-buf))
    (let* ((highlights    (with-current-buffer chai--context-panel-source-buf
                            (chai--collect-highlights)))
           (current-type  (with-current-buffer chai--context-panel-source-buf
                            (chai--current-highlight-type)))
           (panel-buf     (chai--render-context-panel highlights current-type))
           (panel-win     (get-buffer-window panel-buf)))
      (when panel-win
        (with-selected-window panel-win
          (goto-char (point-min)))))))

(defun chai--schedule-context-update ()
  "Debounced trigger for context panel update."
  (when chai--context-panel-timer
    (cancel-timer chai--context-panel-timer))
  (setq chai--context-panel-timer
        (run-with-idle-timer 0.3 nil #'chai--update-context-panel)))

;;;###autoload
(defun chai-context-panel-toggle ()
  "Toggle the chai context panel side window."
  (interactive)
  (let ((panel-win (get-buffer-window chai--context-panel-buf-name)))
    (cond
     (panel-win
      (delete-window panel-win)
      (remove-hook 'post-command-hook #'chai--schedule-context-update t))
     (t
      (setq chai--context-panel-source-buf (current-buffer))
      (let ((win (display-buffer-in-side-window
                  (get-buffer-create chai--context-panel-buf-name)
                  '((side . right)
                    (window-width . 35)
                    (slot . 0)))))
        (with-selected-window win
          (setq-local mode-line-format
                      (list (propertize " Chai Context" 'face '(:weight bold))))
          (setq-local header-line-format nil)
          (setq-local cursor-type nil)
          (setq-local truncate-lines nil)
          (read-only-mode 1)))
      (chai--update-context-panel)
      (add-hook 'post-command-hook #'chai--schedule-context-update nil t)))))

;;; Export - Highlights

(defun chai--export-source-id (file-path)
  "Return the Chai Library ID encoded in FILE-PATH, or nil."
  (when (and file-path
             (string-match
              "\\`\\([0-9]\\{14\\}\\|[0-9]\\{8\\}T[0-9]\\{6\\}\\)__"
              (file-name-nondirectory file-path)))
    (match-string 1 (file-name-nondirectory file-path))))

(defun chai--export-make-file-links (file-path lnum text)
  "Return a list with the line link for FILE-PATH and LNUM.
TEXT is accepted for compatibility with older callers."
  (ignore text)
  (let ((id (chai--export-source-id file-path)))
    (list (when (and file-path lnum)
            (if id
                (format "[[chai:%s::%d][L%d]]" id lnum lnum)
              (format "[[file:%s::%d][L%d]]" file-path lnum lnum))))))

(defun chai--export-heading-path (position)
  "Return the source heading path at POSITION, outermost first.
Each heading records its source position, original level and title.
The caller must widen the buffer to include ancestors outside the scope."
  (save-excursion
    (goto-char position)
    (unless (org-before-first-heading-p)
      (org-back-to-heading t)
      (let (path)
        (while
            (progn
              (push (list :beg (point)
                          :level (org-outline-level)
                          :title (org-get-heading t t t t))
                    path)
              (org-up-heading-safe)))
        path))))

(defun chai--collect-items ()
  "Collect Chai items in source order with their export heading paths.
Collect before widening so scope only selects notes, not their ancestors."
  (let ((items (chai--collect-entries)))
    (save-restriction
      (widen)
      (dolist (item items)
        (plist-put item :heading-path
                   (chai--export-heading-path (plist-get item :beg)))
        (plist-put item :line
                   (line-number-at-pos (or (plist-get item :contents-beg)
                                           (plist-get item :beg))))))
    items))

(defun chai--collect-items-in-scope (scope)
  "Collect Chai items for SCOPE.
SCOPE is one of: \\='buffer\\=, \\='region\\=, or \\='subtree\\=."
  (pcase scope
    ('region
     (if (use-region-p)
         (save-restriction
           (narrow-to-region (region-beginning) (region-end))
           (chai--collect-items))
       (user-error "No region selected")))
    ('subtree
     (unless (eq major-mode 'org-mode)
       (user-error "Not in an Org buffer"))
     (save-restriction
       (org-narrow-to-subtree)
       (chai--collect-items)))
    (_
     (chai--collect-items))))

(defun chai--export-render-source (file-path line &optional text)
  "Return the source line link for FILE-PATH and LINE.
TEXT is accepted for compatibility with older callers."
  (ignore text)
  (if file-path
      (string-join (delq nil (chai--export-make-file-links file-path line nil)) " ")
    ""))

(defun chai--export-one-line-title (text)
  "Collapse TEXT to one line for an Org headline."
  (let* ((one-line (replace-regexp-in-string "[\n\r\t]+" " " (or text "")))
         (spaced (replace-regexp-in-string "  +" " " one-line)))
    (string-trim spaced)))

(defun chai--export-render-property-drawer (file-path line)
  "Return the single source property drawer for FILE-PATH and LINE."
  (concat ":PROPERTIES:\n"
          ":SOURCE: " (chai--export-render-source file-path line) "\n"
          ":END:"))

(defun chai--export-render-entry (item file-path &optional level)
  "Render normalized ITEM as an Org entry at LEVEL (default one)."
  (let* ((kind (plist-get item :kind))
         (text (or (plist-get item :text) ""))
         (type (if (eq kind 'comment)
                   "COMMENT"
                 (upcase (or (plist-get item :type) ""))))
         (title (chai--export-one-line-title text))
         (entry (concat (format "%s [%s]%s"
                               (make-string (or level 1) ?*)
                               type
                               (if (string-empty-p title)
                                   ""
                                 (concat " " title)))
                       "\n"
                       (chai--export-render-property-drawer
                        file-path (plist-get item :line))))
         (body '()))
    (unless (string-empty-p text)
      (when (string-match-p "\n" text)
        (push text body))
      (when-let* ((note (plist-get item :note)))
        (unless (string-empty-p note)
          (push note body))))
    (unless (and (string-empty-p text)
                 (eq kind 'comment))
      (if body
          (concat entry "\n\n" (string-join (nreverse body) "\n\n"))
        entry))))

(defun chai--export-items-as-org (items &optional file-path)
  "Render normalized ITEMS beneath their original source heading paths.
Only ancestors of rendered notes are included, once per source position.
Each note owns its source link in a properties drawer."
  (let ((seen (make-hash-table :test #'eql))
        entries)
    (dolist (item items)
      (let* ((path (plist-get item :heading-path))
             (parent (car (last path)))
             (level (if parent (1+ (plist-get parent :level)) 1))
             (entry (chai--export-render-entry item file-path level)))
        (when entry
          (dolist (heading path)
            (unless (gethash (plist-get heading :beg) seen)
              (puthash (plist-get heading :beg) t seen)
              (push (concat (make-string (plist-get heading :level) ?*) " "
                            (plist-get heading :title))
                    entries)))
          (push entry entries))))
    (if entries
        (concat (string-join (nreverse entries) "\n\n") "\n")
      "")))

(defun chai--export-preview-file-name (source-file)
  "Return the preview file path for SOURCE-FILE.
The name is `<source-file-base>_chai.org' under
`chai-export-preview-directory'."
  (unless source-file
    (user-error "Source buffer has no file name"))
  (expand-file-name
   (concat (file-name-base source-file) "_chai.org")
   chai-export-preview-directory))

(defun chai--export-items-as-text (items &optional file-path)
  "Render ITEMS into plain text.
ITEMS is a list of plists representing highlights and comments.  FILE-PATH is
included as context when available."
  (let ((lines '()))
    (dolist (item items)
      (pcase (plist-get item :kind)
        ('highlight
         (let ((note (plist-get item :note))
               (text (plist-get item :text)))
           (push (format "- %s" (or text "")) lines)
           (when (and note (not (string-empty-p note)))
             (push (format "-- %s" note) lines))))
        ('comment
         (let ((text (plist-get item :text)))
           (unless (string-empty-p text)
             (push (format "[Comment] %s" text) lines))))
        (_ nil)))
    (when (and file-path (not (string-empty-p file-path)))
      (when lines (push "" lines))
      (push file-path lines))
    (string-join (nreverse lines) "\n")))

;;;###autoload
(defun chai-export-highlights-copy (&optional scope)
  "Copy highlights and comments to the kill ring as plain text.
Scope selection precedence:
- Active region exports the region.
- With prefix arg, exports the current subtree.
- Otherwise exports the whole buffer."
  (interactive
   (list (cond
          ((use-region-p) 'region)
          (current-prefix-arg 'subtree)
          (t 'buffer))))
  (let* ((items (chai--collect-items-in-scope scope))
         (file-path (buffer-file-name))
         (out (chai--export-items-as-text items file-path)))
    (kill-new out)
    (message "Copied %d item(s) as text." (length items))))

;;;###autoload
(defun chai-export-highlights-copy-org (&optional scope)
  "Copy highlights and comments to the kill ring as clean Org.
Scope selection precedence:
- Active region exports the region.
- With prefix arg, exports the current subtree.
- Otherwise exports the whole buffer."
  (interactive
   (list (cond
          ((use-region-p) 'region)
          (current-prefix-arg 'subtree)
          (t 'buffer))))
  (let* ((items (chai--collect-items-in-scope scope))
         (file-path (buffer-file-name))
         (out (chai--export-items-as-org items file-path)))
    (kill-new out)
    (message "Copied %d item(s) as Org." (length items))))

;;;###autoload
(defun chai-export-heading (&optional scope)
  "Open the current Chai export in an editable Org heading buffer.
SCOPE selection mirrors `chai-export-highlights-copy-org'.  When
`chai-export-heading-file' is set, associate the buffer with that file so
normal Emacs saving writes the edited export there."
  (interactive
   (list (cond
          ((use-region-p) 'region)
          (current-prefix-arg 'subtree)
          (t 'buffer))))
  (let* ((source-file (buffer-file-name))
         (items (chai--collect-items-in-scope scope))
         (out (chai--export-items-as-org items source-file))
         (buf (get-buffer-create "*Chai Heading Export*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (when (and (buffer-modified-p)
                   (not (y-or-n-p "Heading export has unsaved edits; replace it? ")))
          (user-error "Heading export update cancelled"))
        (when buffer-file-name
          (set-visited-file-name nil t))
        (erase-buffer)
        (insert out)
        (org-mode)
        (org-set-regexps-and-options)
        (when (and chai-export-heading-file
                   (not (string-empty-p chai-export-heading-file)))
          (set-visited-file-name (expand-file-name chai-export-heading-file)
                                 t nil))
        (rename-buffer "*Chai Heading Export*" t)
        (set-buffer-modified-p t)))
    (pop-to-buffer buf)))

;;;###autoload
(defun chai-export-preview (&optional scope)
  "Open an editable Org preview buffer of the current Chai export.
SCOPE selection mirrors `chai-export-highlights-copy-org':
- Active region exports the region.
- With prefix arg, exports the current subtree.
- Otherwise exports the whole buffer."
  (interactive
   (list (cond
          ((use-region-p) 'region)
          (current-prefix-arg 'subtree)
          (t 'buffer))))
  (let* ((source-file (buffer-file-name))
         (items (chai--collect-items-in-scope scope))
         (out (chai--export-items-as-org items source-file))
         (preview-file (chai--export-preview-file-name source-file))
         (buf (get-buffer-create "*Chai Export Preview*")))
    (unless (file-directory-p chai-export-preview-directory)
      (make-directory chai-export-preview-directory t))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (when (and (buffer-modified-p)
                   (not (y-or-n-p "Preview buffer has unsaved edits; replace it? ")))
          (user-error "Preview update cancelled"))
        (erase-buffer)
        (insert out)
        (org-mode)
        (org-set-regexps-and-options)
        (set-visited-file-name preview-file t nil)
        (rename-buffer "*Chai Export Preview*" t)
        (set-buffer-modified-p t)))
    (pop-to-buffer buf)))

;;;###autoload
(defun chai-export-preview-save (&optional scope)
  "Write the current Chai Org export directly to its preview file.
Uses the same scope and file naming as `chai-export-preview'."
  (interactive
   (list (cond
          ((use-region-p) 'region)
          (current-prefix-arg 'subtree)
          (t 'buffer))))
  (let* ((source-file (buffer-file-name))
         (items (chai--collect-items-in-scope scope))
         (out (chai--export-items-as-org items source-file))
         (preview-file (chai--export-preview-file-name source-file)))
    (unless (file-directory-p chai-export-preview-directory)
      (make-directory chai-export-preview-directory t))
    (with-temp-file preview-file
      (insert out))
    (message "Saved Chai export: %s" preview-file)
    preview-file))

;;; Chirp Capture

(defun chai--chirp-entry-value (entry key)
  "Return ENTRY's string value at KEY without text properties, or nil."
  (let ((value (plist-get entry key)))
    (when (and (stringp value) (not (string-empty-p (string-trim value))))
      (substring-no-properties value))))

(defun chai--capture-title (text)
  "Return a short, one-line title derived from captured TEXT."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim text))
   80 nil nil "…"))

(defun chai--capture-quote-text (text)
  "Return TEXT safe to place inside a standard Org QUOTE block."
  (let ((case-fold-search t))
    (replace-regexp-in-string
     (rx line-start "#+END_QUOTE" word-end) ",#+END_QUOTE" text)))

(defun chai--chirp-entry-as-org (entry capture-id title)
  "Render Chirp ENTRY as a normal Org document with CAPTURE-ID and TITLE."
  (let* ((text (or (chai--chirp-entry-value entry :raw-text)
                   (chai--chirp-entry-value entry :text)))
         (tweet-id (chai--chirp-entry-value entry :id))
         (url (chai--chirp-entry-value entry :url))
         (handle (or (chai--chirp-entry-value entry :author-handle) "unknown"))
         (author (or (chai--chirp-entry-value entry :author-name) handle))
         (created-at (or (chai--chirp-entry-value entry :created-at) "unknown")))
    (unless text
      (user-error "Chirp entry has no text to capture"))
    (unless tweet-id
      (user-error "Chirp entry has no tweet ID"))
    (unless url
      (user-error "Chirp entry has no source URL"))
    (format (concat "#+TITLE: %s\n"
                    "#+AUTHOR: %s\n"
                    "#+FILETAGS: :chirp:\n"
                    "#+DATE: %s\n\n"
                    "* %s\n"
                    ":PROPERTIES:\n"
                    ":ID: %s\n"
                    ":CHIRP_ID: %s\n"
                    ":CHIRP_URL: %s\n"
                    ":CHIRP_CREATED_AT: %s\n"
                    ":END:\n\n"
                    "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n\n"
                    "Source: %s\n")
            title author created-at title capture-id tweet-id url created-at
            (chai--capture-quote-text text)
            (org-link-make-string url (format "@%s · %s" handle created-at)))))

;;;###autoload
(defun chai-capture-chirp-entry ()
  "Save the Chirp tweet at point as a normal Chai Library Org file."
  (interactive)
  (unless (derived-mode-p 'chirp-view-mode)
    (user-error "Chai capture is only available in a Chirp timeline"))
  (unless (fboundp 'chirp-entry-at-point)
    (user-error "Chirp is not loaded"))
  (let ((entry (chirp-entry-at-point)))
    (unless (eq (plist-get entry :kind) 'tweet)
      (user-error "No Chirp tweet at point"))
    (require 'chai-library)
    (let* ((capture-id (chai-library--generate-id))
           (text (or (chai--chirp-entry-value entry :raw-text)
                     (chai--chirp-entry-value entry :text)))
           (tweet-id (chai--chirp-entry-value entry :id))
           (url (chai--chirp-entry-value entry :url))
           (handle (or (chai--chirp-entry-value entry :author-handle) "unknown")))
      (unless text
        (user-error "Chirp entry has no text to capture"))
      (unless tweet-id
        (user-error "Chirp entry has no tweet ID"))
      (unless url
        (user-error "Chirp entry has no source URL"))
      (let* ((title (string-trim
                     (read-string "Chirp title: " (chai--capture-title text))))
             (book (chai-book-create :id capture-id
                                     :author handle
                                     :title title
                                     :keywords '("chirp")))
             (file (expand-file-name (chai-library--generate-filename book)
                                     chai-library-directory)))
        (when (string-empty-p title)
          (user-error "Chirp capture title cannot be empty"))
        (when (file-exists-p file)
          (user-error "Chai capture already exists: %s" file))
        (make-directory chai-library-directory t)
        (with-temp-file file
          (insert (chai--chirp-entry-as-org entry capture-id title)))
        (find-file file)
        (message "Saved Chirp capture: %s" file)
        file))))

;;; Telega Capture

;;;###autoload
(defun chai-capture-telega-message ()
  "Save the Telega message at point as a normal Chai Library Org file."
  (interactive)
  (unless (derived-mode-p 'telega-chat-mode)
    (user-error "Chai capture is only available in a Telega chat"))
  (unless (and (fboundp 'telega-msg-for-interactive)
               (fboundp 'telega-msg-content-text)
               (fboundp 'telega-msg-sender)
               (fboundp 'telega-msg-sender-title)
               (fboundp 'telega-msg-sender-username)
               (fboundp 'telega-msg-chat)
               (fboundp 'telega-chat-title)
               (fboundp 'telega-tme-internal-link-to))
    (user-error "Telega is not loaded"))
  (let ((message (telega-msg-for-interactive)))
    (require 'chai-library)
    (let* ((text (when-let* ((value (telega-msg-content-text message)))
                   (let ((clean (string-trim (substring-no-properties value))))
                     (unless (string-empty-p clean) clean))))
           (message-id (plist-get message :id))
           (chat-id (plist-get message :chat_id))
           (sender (telega-msg-sender message))
           (author (if sender
                       (substring-no-properties (telega-msg-sender-title sender))
                     "unknown"))
           (username (and sender (telega-msg-sender-username sender)))
           (chat (telega-msg-chat message 'offline))
           (chat-title (if chat
                           (substring-no-properties (telega-chat-title chat nil t))
                         "unknown"))
           (timestamp (plist-get message :date))
           (created-at (if (numberp timestamp)
                           (format-time-string "%Y-%m-%d %H:%M"
                                               (seconds-to-time timestamp))
                         "unknown"))
           (source (condition-case nil
                       (telega-tme-internal-link-to message)
                     (error nil))))
      (unless text
        (user-error "Telega message has no text or caption to capture"))
      (unless (and (integerp message-id) (> message-id 0)
                   (integerp chat-id))
        (user-error "Telega message has no stable chat or message ID"))
      (unless source
        (user-error "Telega message has no source link"))
      (let* ((capture-id (chai-library--generate-id))
             (title (chai--capture-title text))
             (book (chai-book-create :id capture-id
                                     :author (or username author "unknown")
                                     :title (format "%s-%s-%s" title chat-id message-id)
                                     :keywords '("telega")))
             (file (expand-file-name (chai-library--generate-filename book)
                                     chai-library-directory)))
        (when (file-exists-p file)
          (user-error "Chai capture already exists: %s" file))
        (make-directory chai-library-directory t)
        (with-temp-file file
          (insert
           (format (concat "#+TITLE: %s\n"
                           "#+AUTHOR: %s\n"
                           "#+FILETAGS: :telega:\n"
                           "#+DATE: %s\n\n"
                           "* %s\n"
                           ":PROPERTIES:\n"
                           ":ID: %s\n"
                           ":TELEGA_CHAT_ID: %s\n"
                           ":TELEGA_MESSAGE_ID: %s\n"
                           ":TELEGA_CHAT: %s\n"
                           ":TELEGA_URL: %s\n"
                           ":TELEGA_CREATED_AT: %s\n"
                           ":END:\n\n"
                           "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n\n"
                           "Source: %s\n")
                   title author created-at title capture-id chat-id message-id
                   chat-title source created-at (chai--capture-quote-text text)
                   (org-link-make-string
                    source (format "%s · %s · %s" chat-title author created-at)))))
        (find-file file)
        (message "Saved Telega capture: %s" file)
        file))))

;;; EWW Capture

;;;###autoload
(defun chai-capture-eww-region ()
  "Save the active EWW page region as a normal Chai Library Org file."
  (interactive)
  (unless (derived-mode-p 'eww-mode)
    (user-error "Chai capture is only available in an EWW page"))
  (unless (use-region-p)
    (user-error "Select EWW content to capture"))
  (let* ((text (string-trim
                (buffer-substring-no-properties (region-beginning) (region-end))))
         (url (and (boundp 'eww-current-url) eww-current-url))
         (raw-title (and (boundp 'eww-current-title) eww-current-title))
         (page-title (if (and (stringp raw-title)
                              (not (string-empty-p (string-trim raw-title))))
                         (string-trim (substring-no-properties raw-title))
                       url)))
    (when (string-empty-p text)
      (user-error "Selected EWW content is empty"))
    (unless (and (stringp url) (not (string-empty-p (string-trim url))))
      (user-error "EWW page has no source URL"))
    (require 'chai-library)
    (let* ((capture-id (chai-library--generate-id))
           (created-at (format-time-string "%Y-%m-%d %H:%M"))
           (title (chai--capture-title text))
           (book (chai-book-create :id capture-id
                                   :author "web"
                                   :title page-title
                                   :keywords '("eww")))
           (file (expand-file-name (chai-library--generate-filename book)
                                   chai-library-directory)))
      (when (file-exists-p file)
        (user-error "Chai capture already exists: %s" file))
      (make-directory chai-library-directory t)
      (with-temp-file file
        (insert
         (format (concat "#+TITLE: %s\n"
                         "#+AUTHOR: web\n"
                         "#+FILETAGS: :eww:\n"
                         "#+DATE: %s\n\n"
                         "* %s\n"
                         ":PROPERTIES:\n"
                         ":ID: %s\n"
                         ":EWW_URL: %s\n"
                         ":EWW_TITLE: %s\n"
                         ":EWW_CAPTURED_AT: %s\n"
                         ":END:\n\n"
                         "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n\n"
                         "Source: %s\n")
                 page-title created-at title capture-id url page-title created-at
                 (chai--capture-quote-text text)
                 (org-link-make-string url page-title))))
      (find-file file)
      (message "Saved EWW capture: %s" file)
      file)))

;;; Generic Capture

;;;###autoload
(defun chai-capture ()
  "Save the current selection, or the current line, as a Chai Library Org file.

Works in any buffer.  File buffers record the file and line as source;
non-file buffers record the buffer name and major mode."
  (interactive)
  (let* ((region (use-region-p))
         (text (string-trim
                (if region
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))))
         (file-name (buffer-file-name))
         (mode (symbol-name major-mode))
         (source (or file-name (buffer-name)))
         (line (line-number-at-pos (when region (region-beginning)))))
    (when (string-empty-p text)
      (user-error "Nothing to capture"))
    (require 'chai-library)
    (let* ((capture-id (chai-library--generate-id))
           (created-at (format-time-string "%Y-%m-%d %H:%M"))
           (title (chai--capture-title text))
           (kw (replace-regexp-in-string "-mode$" "" mode))
           (book (chai-book-create
                  :id capture-id
                  :author (file-name-nondirectory source)
                  :title title
                  :keywords (list "capture" kw)))
           (file (expand-file-name (chai-library--generate-filename book)
                                   chai-library-directory))
           (source-link (if file-name
                            (org-link-make-string
                             (format "file:%s::%d" file-name line)
                             (format "%s:%d" (abbreviate-file-name file-name) line))
                          (format "%s (%s)" source mode))))
      (when (file-exists-p file)
        (user-error "Chai capture already exists: %s" file))
      (make-directory chai-library-directory t)
      (with-temp-file file
        (insert (format (concat "#+TITLE: %s\n"
                                "#+AUTHOR: %s\n"
                                "#+FILETAGS: :capture:%s:\n"
                                "#+DATE: %s\n\n"
                                "* %s\n"
                                ":PROPERTIES:\n"
                                ":ID: %s\n"
                                ":CHAI_SOURCE: %s\n"
                                ":CHAI_SOURCE_LINE: %s\n"
                                ":CHAI_SOURCE_MODE: %s\n"
                                ":CHAI_CAPTURED_AT: %s\n"
                                ":END:\n\n"
                                "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n\n"
                                "Source: %s\n")
                        title (file-name-nondirectory source) kw created-at
                        title capture-id source (number-to-string line)
                        mode created-at
                        (chai--capture-quote-text text)
                        source-link)))
      (find-file file)
      (message "Saved Chai capture: %s" file)
      file)))

;;; Annotation display in ordinary Org buffers

(defun chai--org-buffer-render-annotations ()
  "Render Chai annotations and source block faces in the current Org buffer."
  (chai--render-annotations))

(defun chai--org-buffer-setup ()
  "Setup Chai features in the current Org buffer."
  (chai--org-buffer-render-annotations)
  (when (boundp 'context-menu-functions)
    (add-hook 'context-menu-functions #'chai-context-menu nil t)
    (chai--install-context-menu-keys)))

(add-hook 'org-mode-hook #'chai--org-buffer-setup)

(when (derived-mode-p 'org-mode)
  (chai--org-buffer-setup))

;;; Integration with Chai Library

(autoload 'chai-library-open "chai-library-table" "Open the Chai Library interface." t)
(autoload 'chai-library-open-book "chai-library" "Select a Chai Library filename and open it." t)
(autoload 'chai-library-import "chai-library" "Import external files into Chai Library." t)
(autoload 'chai-library-open-book-by-id "chai-library" "Open book by ID." t)

;;; Integration with Chai Search

;; Declared here rather than left to `;;;###autoload' cookies: those only take
;; effect when a package manager generates an autoloads file, and Chai documents
;; a plain `load-path' installation.  These forms keep the search commands
;; visible in `M-x' with no configuration, while the modules themselves load on
;; first use.

(autoload 'chai-index-rebuild "chai-index" "Bring the Chai search index up to date with the Library." t)
(autoload 'chai-index-update-file "chai-index" "Re-index one book in the Chai search index." t)
(autoload 'chai-index-status "chai-index" "Report what the Chai search index holds." t)
(autoload 'chai-index-reset "chai-index" "Delete the Chai search index." t)
(autoload 'chai-index-stop "chai-index" "Stop a running Chai index rebuild." t)
(autoload 'chai-search "chai-search" "Search the Chai Library and show the ranked passages." t)
(autoload 'chai-search-highlights "chai-search" "Search only the annotated passages of the Chai Library." t)
(autoload 'chai-search-query "chai-search" "Return ranked hits for a query across the indexed Chai Library.")
(autoload 'chai-ask "chai-ask" "Answer a question from the Chai Library, citing the passages used." t)
(autoload 'chai-context-for "chai-context" "Return numbered Library passages relevant to a query.")
(autoload 'chai-superchat-mode "chai-superchat" "Wire Chai into superchat." t)
(autoload 'chai-superchat-cowork "chai-superchat" "Answer this superchat conversation from the Chai Library." t)

;; The superchat integration registers hooks and a slash command when superchat
;; loads.  That registration cannot be an `autoload' form — nothing calls it —
;; and putting the `with-eval-after-load' inside `chai-superchat.el' would make
;; it dead code, since that file is only loaded when one of its commands runs.
;; So the form lives here, in the file every configuration requires.
(with-eval-after-load 'superchat
  (require 'chai-superchat nil t))
(autoload 'chai-ask-again "chai-ask" "Ask the last Chai question again." t)
(autoload 'chai-search-semantic "chai-search" "Search the Chai Library by meaning as well as wording." t)
(autoload 'chai-library-search "chai-search" "Search the whole Chai Library." t)
(autoload 'chai-library-search-book "chai-search" "Search only the book at point." t)
(autoload 'chai-index-auto-mode "chai-index" "Keep the Chai search index in step with the Library." t)
(autoload 'chai-vector-build "chai-vector" "Give the Chai Library semantic recall." t)
(autoload 'chai-vector-stop "chai-vector" "Stop a running Chai embedding run." t)
(autoload 'chai-vector-status "chai-vector" "Report how much of the Chai Library has semantic recall." t)

(provide 'chai)

;;; chai.el ends here
