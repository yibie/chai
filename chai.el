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
(require 'cl-lib)
(require 'subr-x)

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

;;; Faces

(defgroup chai-faces nil
  "Faces for Chai highlighting."
  :group 'chai)

(defface chai-highlight-important
  '((((background dark))  :background "#5a2d2d" :extend nil)
    (((background light)) :background "#FFE0B2" :foreground "#000000" :extend nil))
  "Face for \\='important\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-idea
  '((((background dark))  :background "#1e4040" :extend nil)
    (((background light)) :background "#B2DFDB" :foreground "#000000" :extend nil))
  "Face for \\='idea\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-question
  '((((background dark))  :background "#1e3050" :extend nil)
    (((background light)) :background "#BBDEFB" :foreground "#000000" :extend nil))
  "Face for \\='question\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-critical
  '((((background dark))  :background "#4a3010" :extend nil)
    (((background light)) :background "#FFB74D" :foreground "#000000" :extend nil))
  "Face for \\='critical\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-key
  '((((background dark))  :background "#4a4010" :extend nil)
    (((background light)) :background "#FFFDE7" :foreground "#000000" :extend nil))
  "Face for \\='key\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-core
  '((((background dark))  :background "#5a2020" :extend nil)
    (((background light)) :background "#FFCCBC" :foreground "#000000" :extend nil))
  "Face for \\='core\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-detail
  '((((background dark))  :background "#1a3a28" :extend nil)
    (((background light)) :background "#DCEDC8" :foreground "#000000" :extend nil))
  "Face for \\='detail\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-example
  '((((background dark))  :background "#1a2e3a" :extend nil)
    (((background light)) :background "#E3F2FD" :foreground "#000000" :extend nil))
  "Face for \\='example\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-hard
  '((((background dark))  :background "#2e1e3a" :extend nil)
    (((background light)) :background "#EDE7F6" :foreground "#000000" :extend nil))
  "Face for \\='hard\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-block
  '((((background dark))  :background "#1e1e2a" :extend nil)
    (((background light)) :background "#ECEFF1" :foreground "#000000" :extend nil))
  "Face for \\='block\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-view
  '((((background dark))  :background "#2a1e3a" :extend nil)
    (((background light)) :background "#F3E5F5" :foreground "#000000" :extend nil))
  "Face for \\='view\\=' highlights."
  :group 'chai-faces)

(defface chai-highlight-outdated
  '((((background dark))  :foreground "#666666" :strike-through t :extend nil)
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
If PATH is an ID (14 digits or Denote-timestamp), open the corresponding book.
Otherwise, treated as a highlight tag (no action)."
  (if (string-match-p "\\`\\([0-9]\\{14\\}\\|[0-9]\\{8\\}T[0-9]\\{6\\}\\)\\'" path)
      (if (fboundp 'chai-library-open-book-by-id)
          (chai-library-open-book-by-id path)
        (user-error "Chai Library not loaded. Cannot open book with ID: %s" path))
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
  (unless (chai--link-at-point-p pos)
    (user-error "No chai highlight at point"))
  (save-excursion
    (when pos (goto-char pos))
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
      (chai-refresh-annotations))))

(defun chai-mouse-edit-annotation (&optional pos)
  "Edit the annotation of the chai highlight at point (or POS)."
  (interactive)
  (unless (chai--link-at-point-p pos)
    (user-error "No chai highlight at point"))
  (save-excursion
    (when pos (goto-char pos))
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
      (chai-refresh-annotations))))

(defun chai-mouse-remove-highlight (&optional pos)
  "Remove the chai highlight at point (or POS)."
  (interactive)
  (chai-remove-highlight pos))

(defun chai-mouse-copy-text (&optional pos)
  "Copy the highlighted text of the chai link at point (or POS)."
  (interactive)
  (let ((text (or (chai--link-text-at-point pos)
                  (user-error "No chai highlight at point"))))
    (kill-new text)
    (message "Copied: %s" text)))

(defun chai-mouse-highlight-region (type)
  "Highlight the active region with TYPE."
  (interactive)
  (if (use-region-p)
      (chai-highlight-region (region-beginning) (region-end) type)
    (user-error "No region selected")))

(defun chai-mouse-highlight-important ()
  "Highlight the active region as important."
  (interactive)
  (chai-mouse-highlight-region "important"))

(defun chai-mouse-highlight-idea ()
  "Highlight the active region as idea."
  (interactive)
  (chai-mouse-highlight-region "idea"))

(defun chai-mouse-highlight-question ()
  "Highlight the active region as question."
  (interactive)
  (chai-mouse-highlight-region "question"))

(defun chai-mouse-highlight-key ()
  "Highlight the active region as key."
  (interactive)
  (chai-mouse-highlight-region "key"))

(defun chai-mouse-highlight-other ()
  "Highlight the active region, prompting for type."
  (interactive)
  (if (use-region-p)
      (call-interactively #'chai-highlight-region)
    (user-error "No region selected")))

(defun chai-context-menu (menu click)
  "Populate MENU with Chai actions for CLICK event.
Adds highlight actions when right-clicking a chai link, and create-highlight
actions when a region is active.

Menu commands capture the clicked position so they work even if point has moved
after the menu was opened."
  (save-excursion
    (let* ((pos (posn-point (event-start click)))
           (on-link (progn (goto-char pos) (chai--link-at-point-p))))
      (when on-link
        (define-key-after menu [chai-separator]
          '(menu-item "--"))
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
      (when (use-region-p)
        (define-key-after menu [chai-region-separator]
          '(menu-item "--"))
        (define-key-after menu [chai-highlight-other]
          '(menu-item "Highlight other..." chai-mouse-highlight-other
                      :help "Highlight region with any type"))
        (define-key-after menu [chai-highlight-key]
          '(menu-item "Highlight key" chai-mouse-highlight-key
                      :help "Highlight region as key"))
        (define-key-after menu [chai-highlight-question]
          '(menu-item "Highlight question" chai-mouse-highlight-question
                      :help "Highlight region as question"))
        (define-key-after menu [chai-highlight-idea]
          '(menu-item "Highlight idea" chai-mouse-highlight-idea
                      :help "Highlight region as idea"))
        (define-key-after menu [chai-highlight-important]
          '(menu-item "Highlight important" chai-mouse-highlight-important
                      :help "Highlight region as important")))))
  menu)

;;; Highlight Commands

;;;###autoload
(defun chai-highlight-region (start end type)
  "Highlight the region from START to END with TYPE, no annotation.
Format: [[chai:TYPE][TEXT]]"
  (interactive
   (if (use-region-p)
       (list (region-beginning)
             (region-end)
             (completing-read "Highlight type: " (mapcar #'car chai-highlight-types)))
     (user-error "No region selected")))
  (let* ((text (buffer-substring-no-properties start end))
         (link-path (format "chai:%s" type)))
    (delete-region start end)
    (insert (format "[[%s][%s]]" link-path text))))

;;;###autoload
(defun chai-highlight-annotate (start end type note)
  "Highlight the region from START to END with TYPE and NOTE.
Format: [[chai:TYPE:NOTE][TEXT]]"
  (interactive
   (if (use-region-p)
       (let* ((type (completing-read "Highlight type: " (mapcar #'car chai-highlight-types)))
              (note (read-string "Note: ")))
         (if (string-empty-p note)
             (user-error "Note cannot be empty; use chai-highlight-region for plain highlights")
           (list (region-beginning) (region-end) type note)))
     (user-error "No region selected")))
  (let* ((text (buffer-substring-no-properties start end))
         (link-path (format "chai:%s:%s" type note)))
    (delete-region start end)
    (insert (format "[[%s][%s]]" link-path text))
    (chai--render-note-overlays (- (point) (length text) (length link-path) 6)
                                (point))))

;;;###autoload
(defun chai-remove-highlight (&optional pos)
  "Remove the chai highlight at point, restoring the plain text.
If POS is non-nil, remove the highlight at that position instead.
Works whether the link has a note or not."
  (interactive)
  (save-excursion
    (when pos (goto-char pos))
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
        (goto-char begin)))))

;;; Annotation Rendering

(defun chai-parse-link-path (path)
  "Parse a chai link PATH and return (TYPE . NOTE).
PATH format is either \\='TYPE\\=' or \\='TYPE:NOTE\\='."
  (let ((parts (split-string path ":" nil)))
    (cons (car parts)
          (when (cdr parts)
            (mapconcat #'identity (cdr parts) ":")))))

(defun chai-clear-annotations (&optional start end)
  "Remove all chai annotation overlays in region START to END."
  (remove-overlays (or start (point-min))
                   (or end (point-max))
                   'chai-note-ov t))

(defun chai--render-note-overlays (start end)
  "Create after-string overlays for all chai links with notes in START..END."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (elem)
      (let* ((lbegin (org-element-property :begin elem))
             (lend   (org-element-property :end elem)))
        (when (and (>= lbegin start)
                   (<= lend end)
                   (string= (org-element-property :type elem) "chai"))
          (let* ((path   (org-element-property :path elem))
                 (parsed (chai-parse-link-path path))
                 (type   (car parsed))
                 (note   (cdr parsed))
                 (face   (or (cdr (assoc type chai-highlight-types)) 'default)))
            (when note
              (let* ((clean-note (substring-no-properties note))
                     (body (propertize (concat " " clean-note)
                                       'face `(:inherit ,face
                                               :foreground "#888888"
                                               :slant italic
                                               :height 0.85)))
                     (ov   (make-overlay lend lend)))
                (overlay-put ov 'after-string body)
                (overlay-put ov 'chai-note-ov t)
                (overlay-put ov 'priority 90)))))))))

;;;###autoload
(defun chai-refresh-annotations ()
  "Refresh all chai annotations in the current buffer."
  (interactive)
  (chai-clear-annotations)
  (chai--render-note-overlays (point-min) (point-max))
  (message "Chai annotations refreshed."))

;;; Context Panel

(defvar chai--context-panel-buf-name "*chai-context*"
  "Buffer name for the chai context panel.")

(defvar-local chai--context-panel-source-buf nil
  "The main buffer that the context panel is reflecting.")

(defvar-local chai--context-panel-timer nil
  "Idle timer for debounced context panel updates.")

(defun chai--collect-highlights ()
  "Scan current buffer and return all chai highlights.
Returns a list of (TYPE NOTE TEXT LINE-NUM MATCH-BEG)."
  (let (results)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (elem)
        (when (string= (org-element-property :type elem) "chai")
          (let* ((path   (org-element-property :path elem))
                 (parsed (chai-parse-link-path path))
                 (type   (car parsed))
                 (note   (cdr parsed))
                 (cbeg   (org-element-property :contents-begin elem))
                 (cend   (org-element-property :contents-end elem))
                 (text   (if (and cbeg cend)
                             (buffer-substring-no-properties cbeg cend)
                           ""))
                 (mbeg   (org-element-property :begin elem))
                 (lnum   (line-number-at-pos mbeg)))
            (push (list type note text lnum mbeg) results)))))
    (nreverse results)))

(defun chai--collect-comments ()
  "Scan current buffer and return all CHAI_COMMENT blocks.
Returns a list of plists (:kind comment :text TEXT :line LINE)."
  (let (results)
    (org-element-map (org-element-parse-buffer) 'special-block
      (lambda (elem)
        (when (string= (upcase (org-element-property :type elem)) "CHAI_COMMENT")
          (let* ((cbeg (org-element-property :contents-begin elem))
                 (cend (org-element-property :contents-end elem))
                 (text (if (and cbeg cend)
                           (string-trim (buffer-substring-no-properties cbeg cend))
                         ""))
                 (mbeg (org-element-property :begin elem))
                 (lnum (line-number-at-pos mbeg)))
            (push (list :kind 'comment :text text :line lnum) results)))))
    (nreverse results)))

;;;###autoload
(defun chai-insert-comment ()
  "Insert a CHAI_COMMENT block.
If a region is active, wrap the selected text in the block.
Otherwise insert an empty block at point."
  (interactive)
  (let* ((region-p (use-region-p))
         (body (if region-p
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 ""))
         (start (if region-p (region-beginning) (point)))
         (end (if region-p (region-end) (point))))
    (when region-p
      (delete-region start end))
    (goto-char start)
    (insert (if (string-empty-p body)
                "#+BEGIN_CHAI_COMMENT\n#+END_CHAI_COMMENT\n"
              (format "#+BEGIN_CHAI_COMMENT\n%s\n#+END_CHAI_COMMENT\n" body)))
    (forward-line -1)
    (when (string-empty-p body)
      (end-of-line))))

;;;###autoload
(defun chai-add-comment (text)
  "Prompt for TEXT and insert it as a CHAI_COMMENT block at point."
  (interactive "sComment: ")
  (when (string-empty-p (string-trim text))
    (user-error "Comment cannot be empty"))
  (insert (format "#+BEGIN_CHAI_COMMENT\n%s\n#+END_CHAI_COMMENT\n" text)))

(defun chai--current-highlight-type ()
  "Return the chai highlight type at point, or nil."
  (let ((elem (ignore-errors (org-element-context))))
    (when (and elem
               (eq (org-element-type elem) 'link)
               (string= (org-element-property :type elem) "chai"))
      (car (chai-parse-link-path (org-element-property :path elem))))))

(defun chai--render-context-panel (highlights current-type)
  "Render HIGHLIGHTS grouped by type into the context panel buffer.
CURRENT-TYPE is the type at point, used for highlighting the group header."
  (let ((buf (get-buffer-create chai--context-panel-buf-name))
        (groups '()))
    (dolist (type-def chai-highlight-types)
      (let* ((type    (car type-def))
             (entries (cl-remove-if-not
                       (lambda (h) (string= (car h) type))
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
              (let* ((note (nth 1 entry))
                     (text (nth 2 entry))
                     (lnum (nth 3 entry)))
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

(defun chai--export-make-file-links (file-path lnum text)
  "Return a list with the line link for FILE-PATH and LNUM.
TEXT is accepted for compatibility with older callers."
  (ignore text)
  (list (when (and file-path lnum)
          (format "[[file:%s::%d][L%d]]" file-path lnum lnum))))

(defun chai--collect-items ()
  "Collect all Chai items (highlights and comments) sorted by line.
Returns a list of plists.  When two items share a line, their buffer
positions (`:beg') keep the order deterministic."
  (let ((items '()))
    (dolist (h (chai--collect-highlights))
      (push (list :kind 'highlight
                  :type (nth 0 h)
                  :note (nth 1 h)
                  :text (nth 2 h)
                  :line (nth 3 h)
                  :beg (nth 4 h))
            items))
    (dolist (c (chai--collect-comments))
      (push c items))
    (sort items (lambda (a b)
                  (let ((la (or (plist-get a :line) 0))
                        (lb (or (plist-get b :line) 0)))
                    (if (/= la lb)
                        (< la lb)
                      (< (or (plist-get a :beg) 0)
                         (or (plist-get b :beg) 0))))))))

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

(defun chai--export-todo-keywords ()
  "Return the dynamic `#+SEQ_TODO:' value for Org export.
The value is derived from `chai-highlight-types' with `COMMENT' appended."
  (string-join
   (append (mapcar (lambda (type-pair) (upcase (car type-pair)))
                   chai-highlight-types)
           '("COMMENT"))
   " "))

(defun chai--export-source-title (&optional file-path)
  "Return the source title for the current export.
Prefer the current Org buffer's #+TITLE, then FILE-PATH base name."
  (or (and (derived-mode-p 'org-mode)
           (cadr (assoc "TITLE" (org-collect-keywords '("TITLE")))))
      (and file-path (file-name-base file-path))
      "Chai Export"))

(defun chai--export-file-header (file-path)
  "Return the Org file header for an export from FILE-PATH."
  (string-join
   (list (concat "#+TITLE: " (chai--export-source-title file-path))
         (concat "#+SOURCE: " (or file-path ""))
         (concat "#+EXPORTED_AT: " (format-time-string "%Y-%m-%d %H:%M"))
         (concat "#+SEQ_TODO: " (chai--export-todo-keywords)))
   "\n"))

(defun chai--export-one-line-title (text)
  "Return a single-line title from TEXT, suitable for an Org headline.
Newlines and runs of whitespace are collapsed to a single space."
  (let* ((one-line (replace-regexp-in-string "[\n\r\t]+" " " (or text "")))
         (spaced (replace-regexp-in-string "  +" " " one-line)))
    (string-trim spaced)))

(defun chai--export-render-source (file-path line text)
  "Return the `:SOURCE:' property value for an item.
FILE-PATH and LINE are used to build the line jump link.
TEXT is accepted for compatibility with older callers."
  (if file-path
      (string-join (delq nil (chai--export-make-file-links file-path line text)) " ")
    ""))

(defun chai--export-render-property-drawer (file-path line text)
  "Return a PROPERTIES drawer with `:SOURCE:' metadata."
  (concat ":PROPERTIES:\n"
          ":SOURCE: " (chai--export-render-source file-path line text) "\n"
          ":END:"))

(defun chai--export-render-annotation (note)
  "Return a CHAI_ANNOTATION block for NOTE, or nil if NOTE is empty."
  (when (and note (not (string-empty-p note)))
    (concat "#+BEGIN_CHAI_ANNOTATION\n"
            note "\n"
            "#+END_CHAI_ANNOTATION")))

(defun chai--export-render-headline (item file-path)
  "Render highlight ITEM as an Org headline.
FILE-PATH is used to generate source metadata in a property drawer.
Multiline highlight text is collapsed in the headline title and preserved
as body text below the property drawer."
  (let* ((type (or (plist-get item :type) ""))
         (text (or (plist-get item :text) ""))
         (title (chai--export-one-line-title text))
         (note (plist-get item :note))
         (lnum (plist-get item :line))
         (body '()))
    (push (format "* %s %s" (upcase type) title) body)
    (push (chai--export-render-property-drawer file-path lnum text) body)
    (when (string-match-p "\n" text)
      (push text body))
    (let ((annotation (chai--export-render-annotation note)))
      (when annotation
        (push annotation body)))
    (string-join (nreverse body) "\n")))

(defun chai--export-render-comment-headline (item file-path)
  "Render comment ITEM as a COMMENT headline.
FILE-PATH is used to generate source metadata in a property drawer.
Multiline comment text is collapsed in the headline title and preserved
as body text below the property drawer."
  (let* ((text (or (plist-get item :text) ""))
         (title (chai--export-one-line-title text))
         (lnum (plist-get item :line))
         (body '()))
    (push (if (string-empty-p title) "* COMMENT" (format "* COMMENT %s" title)) body)
    (push (chai--export-render-property-drawer file-path lnum nil) body)
    (when (string-match-p "\n" text)
      (push text body))
    (string-join (nreverse body) "\n")))

(defun chai--export-items-as-org (items &optional file-path)
  "Render ITEMS into Org headlines.
ITEMS is a list of plists representing highlights and comments.  FILE-PATH is
used to generate per-item source metadata."
  (if (null items)
      ""
    (concat (chai--export-file-header file-path)
            "\n\n"
            (string-join
             (delq nil
                   (mapcar (lambda (item)
                             (pcase (plist-get item :kind)
                               ('highlight (chai--export-render-headline item file-path))
                               ('comment
                                (let ((text (or (plist-get item :text) "")))
                                  (unless (string-empty-p text)
                                    (chai--export-render-comment-headline item file-path))))
                               (_ nil)))
                           items))
             "\n\n")
            "\n")))

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

;;; Annotation display in ordinary Org buffers

(defun chai--org-buffer-render-annotations ()
  "Render chai annotations in the current Org buffer if any chai links exist."
  (when (cl-some (lambda (h) (nth 1 h))
                 (chai--collect-highlights))
    (chai--render-note-overlays (point-min) (point-max))))

(defun chai--org-buffer-setup ()
  "Setup Chai features in the current Org buffer."
  (chai--org-buffer-render-annotations)
  (when (boundp 'context-menu-functions)
    (add-hook 'context-menu-functions #'chai-context-menu nil t)))

(add-hook 'org-mode-hook #'chai--org-buffer-setup)

;;; Integration with Chai Library

(autoload 'chai-library-open "chai-library-table" "Open the Chai Library interface." t)
(autoload 'chai-library-open-book-by-id "chai-library" "Open book by ID." t)

(provide 'chai)

;;; chai.el ends here
