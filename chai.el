;;; chai.el --- Destructive Reading and Knowledge Digestion -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Keywords: outlines, hypermedia, knowledge-management, reading
;; URL: https://github.com/yibie/chai

;;; Commentary:

;; This package implements the "Chai" (拆 - break down, dismantle) workflow for
;; destructive reading and knowledge digestion. It allows users to "fork"
;; content (headings or regions) from source files into a dedicated "refinery"
;; (drafts inbox) for focused editing, rewriting, and compression.
;; The refined content can then be "saved" as new, permanent notes,
;; integrating seamlessly with existing note-taking systems like Org-roam or Denote.

;;; Code:

(require 'org)
(require 'org-element)

;;; Autoloads

;;; Customization

(defgroup chai nil
  "Customization group for Chai (拆) package."
  :group 'org)

(defcustom chai-refinery-file (expand-file-name "chai/refinery.org" user-emacs-directory)
  "File path for the refinery (drafts inbox).
This file stores all the forked content waiting to be refined."
  :type 'file
  :group 'chai)

(defcustom chai-notes-directory (expand-file-name "chai/notes/" user-emacs-directory)
  "Default directory to save new notes."
  :type 'directory
  :group 'chai)

(defcustom chai-note-saving-style 'multi-file
  "Determines the default saving behavior for `chai-refine-save`.
`single-file`: Append note to `chai-unified-notes-file`.
`multi-file`: Create a new file in `chai-notes-directory`."
  :type '(choice (const :tag "Single File (Append)" single-file)
                 (const :tag "Multi-File (New File)" multi-file))
  :group 'chai)

(defcustom chai-unified-notes-file (expand-file-name "chai/unified-notes.org" user-emacs-directory)
  "The path to the single Org file used for storing all refined notes when appending."
  :type 'file
  :group 'chai)

;;; Variables

(defvar-local chai-original-wc nil
  "Original word count of the refined entry, used to calculate compression ratio.")

;;; Core Logic - Refinery Management

(defun chai-ensure-refinery ()
  "Ensure the refinery file exists and has a proper header."
  (unless (file-exists-p chai-refinery-file)
    (let ((dir (file-name-directory chai-refinery-file)))
      (when (and dir (not (file-exists-p dir)))
        (make-directory dir t)))
    (with-temp-file chai-refinery-file
      (insert "#+TITLE: Chai Refinery (拆工作台)\n")
      (insert "#+AUTHOR: " (user-full-name) "\n")
      (insert "#+CATEGORY: Chai\n")
      (insert "#+STARTUP: content\n\n")
      (insert "* Inbox\n"))))

;;; Core Logic - Source Information Extraction

(defun chai--get-source-link ()
  "Generate an Org link to the current location.
Prioritizes Chai Library ID links ([[chai:ID][Title]]) if available.
Falls back to standard Org ID, CUSTOM_ID, or file links.
Returns a string: [[target][description]]."
  (let* ((source-buffer (current-buffer))
         (file-path (buffer-file-name source-buffer))
         (element (org-element-at-point))
         (title (or (org-element-property :title element)
                    (if file-path (file-name-base file-path))
                    "Untitled"))
         (clean-title (if (stringp title) 
                          (org-link-display-format title)
                        (org-link-display-format (org-element-interpret-data title))))
         ;; Try to get ID from standard Org property or CHAI_ID
         (id (or (org-entry-get nil "ID" t)
                 (org-entry-get nil "CHAI_ID" t))))

    ;; If no ID found in properties, try to extract from Chai Library filename pattern
    (unless id
      (when (and file-path
                 (string-match "\\`\\([0-9]\\{14\\}\\|[0-9]\\{8\\}T[0-9]\\{6\\}\\)__" 
                               (file-name-nondirectory file-path)))
        (setq id (match-string 1 (file-name-nondirectory file-path)))))
    
    (cond
     ;; 1. Prefer Chai ID if it looks like one (14 digits or Denote-style)
     ((and id (string-match-p "\\`\\([0-9]\\{14\\}\\|[0-9]\\{8\\}T[0-9]\\{6\\}\\)\\'" id))
      (format "[[chai:%s][%s]]" id clean-title))

     ;; 2. Standard Org ID (UUIDs etc)
     (id
      (format "[[id:%s][%s]]" id clean-title))
     
     ;; 3. Fallback to CUSTOM_ID
     ((org-element-property :CUSTOM_ID element)
      (format "[[file:%s::#%s][%s]]" file-path (org-element-property :CUSTOM_ID element) clean-title))
     
     ;; 4. Fallback to headline search
     ((eq (org-element-type element) 'headline)
      (format "[[file:%s::*%s][%s]]" file-path clean-title clean-title))
     
     ;; 5. Default to file link
     (t
      (format "[[file:%s][%s]]" file-path clean-title)))))

(defun chai--create-refinery-entry (title content source-link)
  "Create a refine entry in the refinery file.
SOURCE-LINK is a standard Org link string [[target][desc]].
Returns the marker pointing to the new entry."
  (chai-ensure-refinery)
  (let ((refinery-buffer (find-file-noselect chai-refinery-file))
        (wc (with-temp-buffer (insert content) (how-many "[^[:space:]]" (point-min) (point-max)))))
    (with-current-buffer refinery-buffer
      (org-with-wide-buffer
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       ;; 1. Create the container heading
       (insert "\n* TODO " title "\n")
       (let ((entry-marker (save-excursion (forward-line -1) (point-marker))))
         ;; 2. Insert Properties
         (insert ":PROPERTIES:\n")
         (insert ":CREATED: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n")
         (insert ":SOURCE: " source-link "\n")
         (insert ":ORIGINAL_WC: " (number-to-string wc) "\n")
         (insert ":END:\n\n")
         
         ;; 3. Insert Content and Normalize Levels
         (let ((content-start (point)))
           (insert content "\n")
           (let ((min-level 1000))
             (save-excursion
               (goto-char content-start)
               (while (re-search-forward "^\\(\\*+\\) " nil t)
                 (setq min-level (min min-level (- (match-end 1) (match-beginning 1))))))

             (when (and (< min-level 1000) (not (= min-level 2)))
               (let ((delta (- 2 min-level)))
                 (save-excursion
                   (goto-char content-start)
                   (while (re-search-forward "^\\(\\*+\\) " nil t)
                     (let* ((len (- (match-end 1) (match-beginning 1)))
                            (new-len (max 1 (+ len delta))))
                       (replace-match (concat (make-string new-len ?*) " ")))))))))
         
         ;; 4. Return the marker pointing to the TODO heading
         entry-marker)))))

(defun chai--popup-and-narrow (marker)
  "Pop up the refinery buffer, go to MARKER, and narrow to the entry."
  (let* ((buffer (marker-buffer marker)))
    (pop-to-buffer buffer)
    (goto-char marker)
    (org-narrow-to-subtree)
    (org-show-subtree)
    ;; Enable chai-mode
    (chai-mode 1)))

;;; Commands - Forking into Refinery

;;;###autoload
(defun chai-refine-heading (&optional keep-in-place)
  "Fork the current heading into the refinery.

By default, jump to the newly created draft and narrow to its subtree for
continued editing. With prefix arg KEEP-IN-PLACE, only create the draft without
switching buffers."
  (interactive "P")
  (unless (eq major-mode 'org-mode)
    (user-error "Not in an Org buffer"))
  
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))
    
    (unless (eq type 'headline)
      (user-error "Not at a heading"))
    
    (let* ((headline-title (org-element-property :title element))
           (clean-title
            (org-link-display-format
             (if (stringp headline-title)
                 headline-title
               (org-element-interpret-data headline-title))))
           ;; Only keep the subtree *content* (exclude the original heading and
           ;; its property drawer) to avoid duplicate titles in refinery.
           (cbeg (org-element-property :contents-begin element))
           (cend (org-element-property :contents-end element))
           (raw-content (if (and cbeg cend)
                            (buffer-substring-no-properties cbeg cend)
                          ""))
           (source-link (chai--get-source-link)))
      (setq raw-content (chai--strip-leading-drawers raw-content))
      (let ((marker (chai--create-refinery-entry clean-title raw-content source-link)))
        (message "Forked '%s' to Chai Refinery." clean-title)
        (unless keep-in-place
          (chai--popup-and-narrow marker))))))

;;;###autoload
(defun chai-refine-region (start end &optional keep-in-place)
  "Fork the selected region into the refinery.

By default, jump to the newly created draft and narrow to it. With prefix arg
KEEP-IN-PLACE, only create the draft without switching buffers."
  (interactive "r\nP")
  (unless (eq major-mode 'org-mode)
    (user-error "Not in an Org buffer"))
  (let* ((content (buffer-substring-no-properties start end))
         (source-link (chai--get-source-link))
         ;; Extract description from link for default title
         (link-desc (when (and source-link
                               (string-match "\\[\\[[^]]+\\]\\[\\([^]]+\\)\\]\\]" source-link))
                      (match-string 1 source-link)))
         (entry-title (read-string "Refine entry title: " (format "Selection from %s" (or link-desc "Source")))))
    
    (let ((marker (chai--create-refinery-entry entry-title content source-link)))
      (message "Forked region to Chai Refinery.")
      (unless keep-in-place
        (chai--popup-and-narrow marker)))))

(defun chai--strip-leading-drawers (content)
  "Remove leading Org drawers from CONTENT.

This is used when forking a headline into the refinery: we keep the headline's
body, but drop its :PROPERTIES: drawer (e.g. :CLASS:) to avoid clutter."
  (with-temp-buffer
    (insert (or content ""))
    (goto-char (point-min))
    (let ((case-fold-search t)
          (removed t))
      (while removed
        (setq removed nil)
        (skip-chars-forward " \t\n")
        (beginning-of-line)
        (when (looking-at "^:\\([A-Z0-9_]+\\):[ \t]*$")
          (let ((start (point)))
            (when (re-search-forward "^:END:[ \t]*$" nil t)
              (forward-line 1)
              (delete-region start (point))
              (setq removed t))))))
    (string-trim-left (buffer-string))))

;;; Core Logic - Saving Refined Notes

(defun chai--current-entry-is-refinable-p ()
  "Check if the current buffer is `chai-refinery-file` and the cursor is at a valid entry."
  (let ((current-file (and (buffer-file-name) (file-truename (buffer-file-name))))
        (refinery-file (file-truename chai-refinery-file)))
    (and (string= current-file refinery-file)
         (save-excursion
           (ignore-errors
             (org-back-to-heading t)
             (org-entry-get (point) "ORIGINAL_WC"))))))

(defun chai--get-refinery-entry-data ()
  "Extract data from the refinery entry at point.
Returns a list (title content source-link tags entry-point buffer)."
  (unless (chai--current-entry-is-refinable-p)
    (user-error "Not at a valid refinery entry. Ensure you are in refinery.org and on a Chai TODO item"))
  (save-excursion
    (org-back-to-heading t)
    (let* ((buffer (current-buffer))
           (element (org-element-at-point))
           (title (org-element-property :title element))
           (tags (org-element-property :tags element))
           (source-link (org-entry-get (point) "SOURCE"))
           (entry-point (point))
           ;; Extract content correctly whether narrowed or not
           (beg (org-element-property :contents-begin element))
           (end (org-element-property :contents-end element))
           (content (if (and beg end) 
                        (chai--strip-leading-drawers
                         (buffer-substring-no-properties beg (min end (point-max))))
                      "")))
      (list title content source-link tags entry-point buffer))))

(defun chai--save-append-to-file (title content source-link tags)
  "Append refined entry to the unified notes file as a subtree."
  (unless (and chai-unified-notes-file
               (not (string-empty-p chai-unified-notes-file)))
    (user-error "Unified notes file path is not configured"))
  
  (let ((notes-file (expand-file-name chai-unified-notes-file)))
    (unless (file-exists-p notes-file)
      (with-temp-file notes-file
        (insert "#+TITLE: Chai Unified Notes\n")
        (insert "#+AUTHOR: " (user-full-name) "\n")
        (insert "#+STARTUP: content\n\n")))
    
    (with-current-buffer (find-file-noselect notes-file)
      (org-with-wide-buffer
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (insert "\n* " title)
       (when tags
         (let ((col (max (+ (point) 1) org-tags-column)))
           (indent-to col)
           (insert ":" (string-join tags ":") ":")))
       (insert "\n")
       (insert ":PROPERTIES:\n")
       (insert ":CREATED: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n")
       (when source-link (insert ":SOURCE: " source-link "\n"))
       (insert ":END:\n\n")
       (insert content "\n"))
      (save-buffer)
      (message "Appended refined entry to %s" notes-file))))

(defun chai--save-create-new-file (title content source-link tags)
  "Save refined entry as a new Org file with proper headers.
Auto-generates filename. Prompts only if file exists."
  (let* ((default-dir (expand-file-name chai-notes-directory))
         (clean-title (replace-regexp-in-string "[/\\:*?\"<>|[:space:]]+" "-" title))
         (filename (format "%s.org" clean-title))
         (note-filepath (expand-file-name filename default-dir)))
    
    (unless (file-exists-p default-dir)
      (make-directory default-dir t))
    
    (when (file-exists-p note-filepath)
      ;; File exists, so we MUST prompt to avoid overwriting
      (setq note-filepath (read-file-name "File exists. Save as: " default-dir filename)))

    (with-current-buffer (find-file-noselect note-filepath)
      ;; File Headers
      (insert (format "#+TITLE: %s\n" title))
      (insert "#+CATEGORY: Chai Refined Note\n")
      (insert (format "#+DATE: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (when tags
        (insert (format "#+FILETAGS: :%s:\n" (string-join tags ":"))))
      (when source-link
        (insert (format "#+PROPERTY: SOURCE %s\n" source-link)))
      (insert "\n")
      
      ;; Content
      (insert content "\n")
      (save-buffer)
      (message "Saved new note: %s" note-filepath))
    note-filepath))

(defun chai--finalize-refine-entry (original-buffer entry-point title)
  "Internal helper to mark refinery entry as DONE and clean up metadata."
  (with-current-buffer original-buffer
    (goto-char entry-point)
    (when (org-entry-is-todo-p)
      (org-todo "DONE"))
    (save-buffer))
  (message "Refined entry '%s' saved and marked DONE." title))

;;; Commands - Saving Refined Notes

;;;###autoload
(defun chai-refine-save-append ()
  "Save to unified file (Append mode)."
  (interactive)
  (let* ((data (chai--get-refinery-entry-data))
         (title (nth 0 data))
         (content (nth 1 data))
         (source-link (nth 2 data))
         (tags (nth 3 data))
         (entry-point (nth 4 data))
         (original-buffer (nth 5 data)))

    (chai--save-append-to-file title content source-link tags)
    (chai--finalize-refine-entry original-buffer entry-point title)))

;;;###autoload
(defun chai-refine-save-new ()
  "Save to new file (Creation mode)."
  (interactive)
  (let* ((data (chai--get-refinery-entry-data))
         (title (nth 0 data))
         (content (nth 1 data))
         (source-link (nth 2 data))
         (tags (nth 3 data))
         (entry-point (nth 4 data))
         (original-buffer (nth 5 data)))

    (chai--save-create-new-file title content source-link tags)
    (chai--finalize-refine-entry original-buffer entry-point title)))

;;;###autoload
(defun chai-refine-save ()
  "Save the refined entry using the default style configured in `chai-note-saving-style'."
  (interactive)
  (if (eq chai-note-saving-style 'single-file)
      (chai-refine-save-append)
    (chai-refine-save-new)))

;;; Chai Mode

(defvar chai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'chai-refine-save)
    (define-key map (kbd "C-c h")   #'chai-highlight-region)
    (define-key map (kbd "C-c H")   #'chai-highlight-annotate)
    (define-key map (kbd "C-c d")   #'chai-remove-highlight)
    (define-key map (kbd "C-c p")   #'chai-context-panel-toggle)
    (define-key map (kbd "C-c C-r") #'chai-refresh-annotations)
    map)
  "Keymap for chai-mode.")

(defvar-local chai--header-line-saved nil
  "Saved header-line-format before enabling chai-mode.")

(defface chai-progress-bar
  '((t :inherit region))
  "Face for the compression progress bar in chai-mode."
  :group 'chai)

(defface chai-progress-text
  '((t :inherit mode-line-emphasis :weight bold))
  "Face for the text in chai-mode header line."
  :group 'chai)

(define-minor-mode chai-mode
  "Minor mode for refining entries in the refinery."
  :lighter "拆"
  :keymap chai-mode-map

  (if chai-mode
      (chai-setup)
    (chai-teardown)))

(defun chai-setup ()
  "Setup chai mode with header-line progress display."
  (let ((wc-prop (org-entry-get nil "ORIGINAL_WC")))
    (when wc-prop
      (setq chai-original-wc (string-to-number wc-prop))))
  (setq chai--header-line-saved header-line-format)
  (setq header-line-format '(:eval (chai--header-line)))
  (chai-refresh-annotations)
  (add-hook 'after-change-functions #'chai--after-change-refresh nil t))

(defun chai-teardown ()
  "Teardown chai mode."
  (setq chai-original-wc nil)
  (setq header-line-format chai--header-line-saved)
  (when chai--refresh-timer
    (cancel-timer chai--refresh-timer)
    (setq chai--refresh-timer nil))
  (chai-clear-annotations)
  (remove-hook 'after-change-functions #'chai--after-change-refresh t))

(defvar-local chai--refresh-timer nil
  "Idle timer for debounced annotation refresh.")

(defun chai--after-change-refresh (beg end _len)
  "Schedule a debounced annotation refresh after buffer changes."
  (when chai--refresh-timer
    (cancel-timer chai--refresh-timer))
  (setq chai--refresh-timer
        (run-with-idle-timer
         0.5 nil
         (lambda (buf b e)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (let ((line-beg (save-excursion (goto-char b) (line-beginning-position)))
                     (line-end (save-excursion (goto-char e) (line-end-position))))
                 (chai-clear-annotations line-beg line-end)
                 (chai--render-note-overlays line-beg line-end)))))
         (current-buffer) beg end)))

;;;###autoload
(defun chai-recalculate-original-wc ()
  "Recalculate and update ORIGINAL_WC for current refinery entry.
Use this to fix entries created with old word-counting algorithm."
  (interactive)
  (unless (chai--current-entry-is-refinable-p)
    (user-error "Not at a valid refinery entry"))
  (save-excursion
    (org-back-to-heading t)
    (let* ((element (org-element-at-point))
           (beg (org-element-property :contents-begin element))
           (end (org-element-property :contents-end element))
           (new-wc (if (and beg end)
                       (how-many "[^[:space:]]" beg (min end (point-max)))
                     0)))
      (org-entry-put (point) "ORIGINAL_WC" (number-to-string new-wc))
      (setq chai-original-wc new-wc)
      (message "Updated ORIGINAL_WC to %d (based on current content)" new-wc))))

(defun chai--header-line ()
  "Generate header-line with compression progress bar."
  (if (and chai-original-wc (> chai-original-wc 0))
      (let* ((current-wc (how-many "[^[:space:]]" (point-min) (point-max)))
             (ratio (max 0 (min 100 (* 100 (- 1 (/ (float current-wc) chai-original-wc))))))
             (width (- (window-width) 20))
             (filled (truncate (* width (/ ratio 100.0))))
             (empty (- width filled))
             (bar (concat (propertize (make-string filled ?█) 'face 'chai-progress-bar)
                          (make-string empty ?░)))
             (text (format "拆 %d%% (%d → %d) " 
                          (truncate ratio) chai-original-wc current-wc)))
        (concat (propertize text 'face 'chai-progress-text) bar))
    (propertize " 拆 Refinery " 'face 'chai-progress-text)))

;;; Commands - Navigation

;;;###autoload
(defun chai-switch-draft ()
  "Select an active draft from the refinery and open it in focused mode.
Lists all TODO entries in the refinery."
  (interactive)
  (chai-ensure-refinery)
  (let ((refinery-buffer (find-file-noselect chai-refinery-file))
        (candidates '()))

    (with-current-buffer refinery-buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\*+ +TODO +\\(.*\\)$" nil t)
          (let* ((title (string-trim (match-string-no-properties 1)))
                 (heading-pos (match-beginning 0))
                 (marker (save-excursion (goto-char heading-pos) (point-marker)))
                 (source (save-excursion
                           (goto-char heading-pos)
                           (org-entry-get (point) "SOURCE")))
                 ;; Extract title from source link for display
                 (source-title (when (and source (string-match "\\[\\[[^]]+\\]\\[\\([^]]+\\)\\]\\]" source))
                                 (match-string 1 source)))
                 (display-str (if source-title
                                  (format "%s  (from %s)" title source-title)
                                title)))
            (push (cons display-str marker) candidates)))))

    (if (null candidates)
        (message "No active drafts found.")
      (let* ((selection (completing-read "Select Draft to Refine: " (mapcar #'car candidates) nil t))
             (marker (cdr (assoc selection candidates))))
        (when marker
          (chai--popup-and-narrow marker))))))

;;; Highlighting System

(defgroup chai-faces nil
  "Faces for Chai highlighting."
  :group 'chai)

(defface chai-highlight-important
  '((((background dark))  :background "#5a2d2d" :extend nil)
    (((background light)) :background "#FFE0B2" :foreground "#000000" :extend nil))
  "Face for 'important' highlights."
  :group 'chai-faces)

(defface chai-highlight-idea
  '((((background dark))  :background "#1e4040" :extend nil)
    (((background light)) :background "#B2DFDB" :foreground "#000000" :extend nil))
  "Face for 'idea' highlights."
  :group 'chai-faces)

(defface chai-highlight-question
  '((((background dark))  :background "#1e3050" :extend nil)
    (((background light)) :background "#BBDEFB" :foreground "#000000" :extend nil))
  "Face for 'question' highlights."
  :group 'chai-faces)

(defface chai-highlight-critical
  '((((background dark))  :background "#4a3010" :extend nil)
    (((background light)) :background "#FFB74D" :foreground "#000000" :extend nil))
  "Face for 'critical' highlights."
  :group 'chai-faces)

;; ── 新增类型 ──────────────────────────────────────────────────────────

(defface chai-highlight-key
  '((((background dark))  :background "#4a4010" :extend nil)
    (((background light)) :background "#FFFDE7" :foreground "#000000" :extend nil))
  "Face for 'key' highlights (中心思想/主旨句, yellow)."
  :group 'chai-faces)

(defface chai-highlight-core
  '((((background dark))  :background "#5a2020" :extend nil)
    (((background light)) :background "#FFCCBC" :foreground "#000000" :extend nil))
  "Face for 'core' highlights (定义/核心考点, red/orange)."
  :group 'chai-faces)

(defface chai-highlight-detail
  '((((background dark))  :background "#1a3a28" :extend nil)
    (((background light)) :background "#DCEDC8" :foreground "#000000" :extend nil))
  "Face for 'detail' highlights (数据/细节, blue/green, underline)."
  :group 'chai-faces)

(defface chai-highlight-example
  '((((background dark))  :background "#1a2e3a" :extend nil)
    (((background light)) :background "#E3F2FD" :foreground "#000000" :extend nil))
  "Face for 'example' highlights (案例/辅助证据, blue/green, underline)."
  :group 'chai-faces)

(defface chai-highlight-hard
  '((((background dark))  :background "#2e1e3a" :extend nil)
    (((background light)) :background "#EDE7F6" :foreground "#000000" :extend nil))
  "Face for 'hard' highlights (难点/逻辑转折, purple, wavy underline)."
  :group 'chai-faces)

(defface chai-highlight-block
  '((((background dark))  :background "#1e1e2a" :extend nil)
    (((background light)) :background "#ECEFF1" :foreground "#000000" :extend nil))
  "Face for 'block' highlights (完整观点段落, grey, left border)."
  :group 'chai-faces)

(defface chai-highlight-view
  '((((background dark))  :background "#2a1e3a" :extend nil)
    (((background light)) :background "#F3E5F5" :foreground "#000000" :extend nil))
  "Face for 'view' highlights (作者观点/个人心得, purple/grey)."
  :group 'chai-faces)

(defface chai-highlight-outdated
  '((((background dark))  :foreground "#666666" :strike-through t :extend nil)
    (((background light)) :foreground "#9E9E9E" :strike-through t :extend nil))
  "Face for 'outdated' highlights (排除/过时信息, strikethrough)."
  :group 'chai-faces)

(defcustom chai-highlight-types
  '(;; 原有类型
    ("important" . chai-highlight-important)
    ("idea"      . chai-highlight-idea)
    ("question"  . chai-highlight-question)
    ("critical"  . chai-highlight-critical)
    ;; 新增类型
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

(org-link-set-parameters "chai"
                         :face #'chai-link-face
                         :follow #'chai-link-follow
                         :export #'chai-link-export)

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
(defun chai-remove-highlight ()
  "Remove the chai highlight at point, restoring the plain text.
Works whether the link has a note or not."
  (interactive)
  (let ((pos (point)))
    ;; org-element-context finds the link element at point
    (let ((elem (org-element-context)))
      (unless (and (eq (org-element-type elem) 'link)
                   (string= (org-element-property :type elem) "chai"))
        (user-error "No chai highlight at point"))
      (let* ((begin (org-element-property :begin elem))
             (end   (org-element-property :end elem))
             ;; The visible description text
             (text  (buffer-substring-no-properties
                     (org-element-property :contents-begin elem)
                     (org-element-property :contents-end elem))))
        ;; Remove note overlays attached to this link
        (chai-clear-annotations begin end)
        ;; Replace the full link with plain text
        (delete-region begin end)
        (insert text)
        (goto-char begin)))))

;;; Annotation Rendering

(defun chai-parse-link-path (path)
  "Parse a chai link PATH and return (TYPE . NOTE).
PATH format is either 'TYPE' or 'TYPE:NOTE'."
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
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\[\\[chai:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" end t)
      (let* ((link-end (match-end 0))
             (path     (match-string 1))
             (parsed   (chai-parse-link-path path))
             (type     (car parsed))
             (note     (cdr parsed))
             (face     (or (cdr (assoc type chai-highlight-types)) 'default)))
        (when note
          (let* ((clean-note (substring-no-properties note))
                 (body (propertize (concat " " clean-note)
                                   'face '(:foreground "#888888"
                                           :slant italic
                                           :height 0.85)))
                 (ov   (make-overlay link-end link-end)))
            (overlay-put ov 'after-string body)
            (overlay-put ov 'chai-note-ov t)
            (overlay-put ov 'priority 90)))))))

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
  (let ((results '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "\\[\\[chai:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" nil t)
        (let* ((path   (match-string 1))
               (text   (match-string 2))
               (parsed (chai-parse-link-path path))
               (type   (car parsed))
               (note   (cdr parsed))
               (lnum   (line-number-at-pos (match-beginning 0)))
               (mbeg   (match-beginning 0)))
          (push (list type note text lnum mbeg) results))))
    (nreverse results)))

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
        ;; Group: alist of (type . list-of-entries)
        (groups '()))
    ;; Build groups preserving type order from chai-highlight-types
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
                 ;; Highlight the group header if cursor is on this type
                 (header-face (if (string= type current-type)
                                  `(:inherit ,face :inverse-video t :weight bold)
                                `(:inherit ,face :weight bold))))
            ;; Group header
            (insert (propertize (format " %s (%d)\n" (upcase type) (length entries))
                                'face header-face))
            ;; Entries
            (dolist (entry entries)
              (let* ((note (nth 1 entry))
                     (text (nth 2 entry))
                     (lnum (nth 3 entry)))
                ;; Line number + highlighted text
                (insert (propertize (format "  L%-4d " lnum)
                                    'face '(:foreground "#666666")))
                (insert (propertize text 'face `(:inherit ,face)))
                (insert "\n")
                ;; Note (if present)
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
      ;; If panel window exists, preserve scroll position
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
  (let ((panel-buf  (get-buffer chai--context-panel-buf-name))
        (panel-win  (get-buffer-window chai--context-panel-buf-name)))
    (cond
     ;; Already visible: close it
     (panel-win
      (delete-window panel-win)
      (remove-hook 'post-command-hook #'chai--schedule-context-update t))
     ;; Open panel
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
      ;; Initial render
      (chai--update-context-panel)
      ;; Install auto-update hook
      (add-hook 'post-command-hook #'chai--schedule-context-update nil t)))))

;;; Annotation display in ordinary Org buffers

(defun chai--org-buffer-render-annotations ()
  "Render chai annotations in the current Org buffer if any chai links exist.
Called from `org-mode-hook' so annotations are visible without enabling
`chai-mode' (which is intended only for the refinery buffer)."
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "\\[\\[chai:" nil t))
    (chai--render-note-overlays (point-min) (point-max))))

(add-hook 'org-mode-hook #'chai--org-buffer-render-annotations)

;;; Integration with Chai Library

;; Autoload the library module commands so they are available
;; immediately after (require 'chai), without needing to manually
;; require 'chai-library.
(autoload 'chai-library-open "chai-library-table" "Open the Chai Library interface." t)
(autoload 'chai-library-open-book-by-id "chai-library" "Open book by ID." t)

(provide 'chai)
;;; chai.el ends here
