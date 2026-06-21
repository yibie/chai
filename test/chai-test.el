;;; chai-test.el --- ERT tests for chai.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'chai)
(require 'chai-library-table)

(defmacro chai-test--with-temp-org (contents &rest body)
  "Evaluate BODY in a temporary Org buffer with CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defmacro chai-test--with-export-env (&rest body)
  "Evaluate BODY with predictable export settings.
Binds a small `chai-highlight-types' and freezes the exported timestamp."
  (declare (indent 0))
  `(let ((chai-highlight-types '(("important" . chai-highlight-important)
                                ("idea"      . chai-highlight-idea)
                                ("key"       . chai-highlight-key))))
     (cl-letf (((symbol-function 'format-time-string)
                (lambda (&rest _) "2026-06-21 12:30")))
       ,@body)))

(defun chai-test--kill-file-buffers-under (dir)
  "Kill buffers visiting files under DIR."
  (dolist (buf (buffer-list))
    (when-let* ((file (buffer-file-name buf)))
      (when (file-in-directory-p file dir)
        (kill-buffer buf)))))

(ert-deftest chai-test-parse-link-path-plain ()
  "Parse a plain highlight path."
  (should (equal (chai-parse-link-path "important")
                 '("important" . nil))))

(ert-deftest chai-test-parse-link-path-with-note ()
  "Parse a highlight path with note containing colons."
  (should (equal (chai-parse-link-path "idea:note:with:colons")
                 '("idea" . "note:with:colons"))))

(ert-deftest chai-test-collect-plain-highlight ()
  "Collect a single plain highlight."
  (chai-test--with-temp-org "[[chai:important][highlight text]]"
    (let ((hl (car (chai--collect-highlights))))
      (should hl)
      (should (string= (nth 0 hl) "important"))
      (should (null (nth 1 hl)))
      (should (string= (nth 2 hl) "highlight text"))
      (should (= (nth 3 hl) 1))
      (should (= (nth 4 hl) 1)))))

(ert-deftest chai-test-collect-annotated-highlight ()
  "Collect a highlight with a note."
  (chai-test--with-temp-org "[[chai:idea:my note][highlight text]]"
    (let ((hl (car (chai--collect-highlights))))
      (should (string= (nth 0 hl) "idea"))
      (should (string= (nth 1 hl) "my note"))
      (should (string= (nth 2 hl) "highlight text")))))

(ert-deftest chai-test-collect-note-with-colons ()
  "Collect a highlight whose note contains colons."
  (chai-test--with-temp-org "[[chai:question:url: https://example.org][link]]"
    (let ((hl (car (chai--collect-highlights))))
      (should (string= (nth 0 hl) "question"))
      (should (string= (nth 1 hl) "url: https://example.org"))
      (should (string= (nth 2 hl) "link")))))

(ert-deftest chai-test-collect-ignores-non-chai-links ()
  "Non-chai Org links are ignored."
  (chai-test--with-temp-org "[[https://example.org][example]] [[chai:key][keep me]]"
    (let ((hls (chai--collect-highlights)))
      (should (= (length hls) 1))
      (should (string= (nth 2 (car hls)) "keep me")))))

(ert-deftest chai-test-collect-ignores-malformed-links ()
  "Malformed links are ignored without errors."
  (chai-test--with-temp-org "[[no closing bracket [[chai:ok][fine]]"
    (let ((hls (chai--collect-highlights)))
      (should (= (length hls) 1))
      (should (string= (nth 2 (car hls)) "fine")))))

(ert-deftest chai-test-collect-respects-narrowing ()
  "Collection respects buffer narrowing (used for region/subtree scopes)."
  (chai-test--with-temp-org "[[chai:important][before]] [[chai:key][after]]"
    (save-restriction
      (narrow-to-region 1 30)
      (let ((hls (chai--collect-highlights)))
        (should (= (length hls) 1))
        (should (string= (nth 2 (car hls)) "before"))))))

(ert-deftest chai-test-collect-in-subtree ()
  "Collection inside a narrowed subtree returns only that subtree's highlights."
  (chai-test--with-temp-org
      "* Section A\n[[chai:important][in A]]\n* Section B\n[[chai:key][in B]]"
    (goto-char 1)
    (save-restriction
      (org-narrow-to-subtree)
      (let ((hls (chai--collect-highlights)))
        (should (= (length hls) 1))
        (should (string= (nth 2 (car hls)) "in A"))))))

(ert-deftest chai-test-export-text-includes-file-path ()
  "Text export includes highlights and file path."
  (chai-test--with-temp-org "[[chai:important][keep me]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (let* ((items (chai--collect-items))
           (out (chai--export-items-as-text items "/tmp/source.org")))
      (should (= (length items) 1))
      (should (string-match-p "keep me" out))
      (should (string-match-p "/tmp/source.org" out)))))

(ert-deftest chai-test-export-org-plain-highlight ()
  "Org export emits a headline for a plain highlight."
  (chai-test--with-temp-org "[[chai:important][hi]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items "/tmp/source.org")))
        (should (string= out (concat "#+TITLE: source\n"
                                     "#+SOURCE: /tmp/source.org\n"
                                     "#+EXPORTED_AT: 2026-06-21 12:30\n"
                                     "#+SEQ_TODO: IMPORTANT IDEA KEY COMMENT\n\n"
                                     "* IMPORTANT hi\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n")))))))

(ert-deftest chai-test-export-org-annotated-highlight ()
  "Org export keeps annotation in a CHAI_ANNOTATION block under the headline."
  (chai-test--with-temp-org "[[chai:idea:my note][hi]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items "/tmp/source.org")))
        (should (string= out (concat "#+TITLE: source\n"
                                     "#+SOURCE: /tmp/source.org\n"
                                     "#+EXPORTED_AT: 2026-06-21 12:30\n"
                                     "#+SEQ_TODO: IMPORTANT IDEA KEY COMMENT\n\n"
                                     "* IDEA hi\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n"
                                     "#+BEGIN_CHAI_ANNOTATION\n"
                                     "my note\n"
                                     "#+END_CHAI_ANNOTATION\n")))))))

(ert-deftest chai-test-export-org-source-links ()
  "Org export puts line links in the :SOURCE: property."
  (chai-test--with-temp-org "[[chai:important][This is a long highlight text]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items "/tmp/source.org")))
        (should (string= out (concat "#+TITLE: source\n"
                                     "#+SOURCE: /tmp/source.org\n"
                                     "#+EXPORTED_AT: 2026-06-21 12:30\n"
                                     "#+SEQ_TODO: IMPORTANT IDEA KEY COMMENT\n\n"
                                     "* IMPORTANT This is a long highlight text\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n")))))))

(ert-deftest chai-test-export-org-comment-block ()
  "Org export emits a COMMENT headline for free-standing comments."
  (chai-test--with-temp-org "#+BEGIN_CHAI_COMMENT\ncomment text\n#+END_CHAI_COMMENT"
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items nil)))
        (should (string= out (concat "#+TITLE: Chai Export\n"
                                     "#+SOURCE: \n"
                                     "#+EXPORTED_AT: 2026-06-21 12:30\n"
                                     "#+SEQ_TODO: IMPORTANT IDEA KEY COMMENT\n\n"
                                     "* COMMENT comment text\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: \n"
                                     ":END:\n")))))))

(ert-deftest chai-test-export-org-mixed-order ()
  "Org export preserves source order across highlight and comment headlines."
  (chai-test--with-temp-org "[[chai:important][first]]\n#+BEGIN_CHAI_COMMENT\nmiddle\n#+END_CHAI_COMMENT\n[[chai:key][second]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items "/tmp/source.org")))
        (should (string= out (concat "#+TITLE: source\n"
                                     "#+SOURCE: /tmp/source.org\n"
                                     "#+EXPORTED_AT: 2026-06-21 12:30\n"
                                     "#+SEQ_TODO: IMPORTANT IDEA KEY COMMENT\n\n"
                                     "* IMPORTANT first\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n\n"
                                     "* COMMENT middle\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::2][L2]]\n"
                                     ":END:\n\n"
                                     "* KEY second\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::5][L5]]\n"
                                     ":END:\n")))))))

(ert-deftest chai-test-export-org-dynamic-todo ()
  "Org export #+SEQ_TODO line reflects the current `chai-highlight-types'."
  (chai-test--with-temp-org "[[chai:question][q]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (let ((chai-highlight-types '(("question" . chai-highlight-question))))
      (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "2026-06-21 12:30")))
        (let* ((items (chai--collect-items))
               (out (chai--export-items-as-org items "/tmp/source.org")))
          (should (string= out (concat "#+TITLE: source\n"
                                       "#+SOURCE: /tmp/source.org\n"
                                       "#+EXPORTED_AT: 2026-06-21 12:30\n"
                                       "#+SEQ_TODO: QUESTION COMMENT\n\n"
                                       "* QUESTION q\n"
                                       ":PROPERTIES:\n"
                                       ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                       ":END:\n"))))))))

(ert-deftest chai-test-export-org-title-from-source ()
  "Org export title follows the source Org #+TITLE."
  (chai-test--with-temp-org "#+TITLE: Real Book\n[[chai:important][hi]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items "/tmp/source.org")))
        (should (string-prefix-p "#+TITLE: Real Book\n" out))))))

(ert-deftest chai-test-export-org-same-line-order ()
  "Org export orders same-line highlights by buffer position."
  (chai-test--with-temp-org "[[chai:key][a]] [[chai:important][b]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items "/tmp/source.org")))
        (should (string= out (concat "#+TITLE: source\n"
                                     "#+SOURCE: /tmp/source.org\n"
                                     "#+EXPORTED_AT: 2026-06-21 12:30\n"
                                     "#+SEQ_TODO: IMPORTANT IDEA KEY COMMENT\n\n"
                                     "* KEY a\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n\n"
                                     "* IMPORTANT b\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n")))))))

(ert-deftest chai-test-export-org-multiline-highlight ()
  "Org export collapses multiline highlight titles and preserves body text."
  (chai-test--with-export-env
    (let* ((item (list :kind 'highlight :type "important" :text "line1\nline2"
                       :note nil :line 1 :beg 1))
           (out (chai--export-items-as-org (list item) "/tmp/source.org")))
      (should (string= out (concat "#+TITLE: source\n"
                                   "#+SOURCE: /tmp/source.org\n"
                                   "#+EXPORTED_AT: 2026-06-21 12:30\n"
                                   "#+SEQ_TODO: IMPORTANT IDEA KEY COMMENT\n\n"
                                   "* IMPORTANT line1 line2\n"
                                   ":PROPERTIES:\n"
                                   ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                   ":END:\n"
                                   "line1\n"
                                   "line2\n"))))))


(ert-deftest chai-test-export-org-multiline-comment ()
  "Org export collapses multiline comment titles and preserves body text."
  (chai-test--with-temp-org (concat "#+BEGIN_CHAI_COMMENT" "\n" "line1" "\n" "line2" "\n" "#+END_CHAI_COMMENT")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items nil)))
        (should (string= out (concat "#+TITLE: Chai Export\n"
                                     "#+SOURCE: \n"
                                     "#+EXPORTED_AT: 2026-06-21 12:30\n"
                                     "#+SEQ_TODO: IMPORTANT IDEA KEY COMMENT\n\n"
                                     "* COMMENT line1 line2\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: \n"
                                     ":END:\n"
                                     "line1\n"
                                     "line2\n")))))))

(ert-deftest chai-test-link-face-returns-face ()
  "chai-link-face returns the configured face for known types."
  (should (eq (chai-link-face "important") 'chai-highlight-important))
  (should (eq (chai-link-face "key") 'chai-highlight-key))
  (should (eq (chai-link-face "unknown") 'org-link)))

(ert-deftest chai-test-help-echo-plain ()
  "Help echo for a plain highlight shows its type."
  (chai-test--with-temp-org "[[chai:important][text]]"
    (let ((msg (chai-link-help-echo-at-point)))
      (should (string= msg "Chai: important")))))

(ert-deftest chai-test-help-echo-note ()
  "Help echo for an annotated highlight shows type and note."
  (chai-test--with-temp-org "[[chai:idea:my note][text]]"
    (let ((msg (chai-link-help-echo-at-point)))
      (should (string= msg "Chai note (idea): my note")))))

(ert-deftest chai-test-visible-face-property ()
  "Org fontification applies the chai highlight face to the link."
  (chai-test--with-temp-org "[[chai:important][text]]"
    (font-lock-ensure)
    (goto-char (point-min))
    (let ((face (get-text-property (point) 'face)))
      (should (or (eq face 'chai-highlight-important)
                  (and (listp face)
                       (memq 'chai-highlight-important face)))))))

;;; Chai Library keybindings

(defun chai-test-library-custom-command ()
  "Dummy command used by keybinding tests."
  (interactive))

(ert-deftest chai-test-library-keybindings-are-customizable ()
  "Library keybindings can be replaced through `chai-library-keybindings'."
  (let ((old-bindings chai-library-keybindings))
    (unwind-protect
        (progn
          (setq chai-library-keybindings
                '(("x" . chai-test-library-custom-command)
                  ("g" . chai-test-library-custom-command)))
          (chai-library-apply-keybindings)
          (should (eq (lookup-key chai-library-mode-map (kbd "x"))
                      'chai-test-library-custom-command))
          (should (eq (lookup-key chai-library-mode-map (kbd "g"))
                      'chai-test-library-custom-command))
          (should-not (lookup-key chai-library-mode-map (kbd "a"))))
      (setq chai-library-keybindings old-bindings)
      (chai-library-apply-keybindings))))

(ert-deftest chai-test-library-rename-unmanaged-without-metadata ()
  "Unmanaged org files without metadata are adopted using the filename as title."
  (let* ((dir (make-temp-file "chai-library-" t))
         (file (expand-file-name "Plain Book.org" dir)))
    (unwind-protect
        (let ((chai-library-directory dir))
          (with-temp-file file
            (insert "Body\n"))
          (let* ((result (chai-library--rename-to-managed file))
                 (new-path (cdr result))
                 (new-name (file-name-nondirectory new-path)))
            (should (eq (car result) 'success))
            (should (file-exists-p new-path))
            (should (string-match-p
                     "\\`[0-9]\\{8\\}T[0-9]\\{6\\}__Plain-Book\\.org\\'"
                     new-name))))
      (chai-test--kill-file-buffers-under dir)
      (delete-directory dir t))))

(ert-deftest chai-test-library-scan-auto-adopts-unmanaged-files ()
  "Scanning the library adopts unmanaged org files automatically."
  (let* ((dir (make-temp-file "chai-library-" t))
         (file (expand-file-name "Loose Note.org" dir)))
    (unwind-protect
        (let ((chai-library-directory dir)
              (chai-library--cache-books nil)
              (chai-library--cache-mtime nil)
              (chai-library--cache-hash (make-hash-table :test 'equal)))
          (with-temp-file file
            (insert "#+TITLE: Loose Note\n"))
          (let ((books (chai-library-scan t)))
            (should (= (length books) 1))
            (should (chai-book-id (car books)))
            (should-not (file-exists-p file))
            (should (string-match-p
                     "\\`[0-9]\\{8\\}T[0-9]\\{6\\}__Loose-Note\\.org\\'"
                     (file-name-nondirectory (chai-book-file-path (car books)))))))
      (chai-test--kill-file-buffers-under dir)
      (delete-directory dir t))))

(ert-deftest chai-test-library-refresh-preserves-current-book ()
  "Refreshing the library keeps point on the selected book."
  (let* ((book-a (chai-book-create :id "20260101T000001" :author "" :title "Alpha"
                                   :keywords nil :status nil :rating nil
                                   :file-path "/tmp/alpha.org" :modified (seconds-to-time 1)))
         (book-b (chai-book-create :id "20260101T000002" :author "" :title "Beta"
                                   :keywords nil :status nil :rating nil
                                   :file-path "/tmp/beta.org" :modified (seconds-to-time 2)))
         (book-c (chai-book-create :id "20260101T000003" :author "" :title "Gamma"
                                   :keywords nil :status nil :rating nil
                                   :file-path "/tmp/gamma.org" :modified (seconds-to-time 3)))
         (books (list book-a book-b book-c)))
    (cl-letf (((symbol-function 'chai-library-scan) (lambda (&optional _) books)))
      (with-temp-buffer
        (chai-library-mode)
        (chai-library-refresh)
        (goto-char (point-min))
        (search-forward "Beta")
        (let ((column (current-column)))
          (setq books (list book-c book-b book-a))
          (chai-library-refresh)
          (should (equal (chai-book-id (chai-library-get-book-at-point))
                         "20260101T000002"))
          (should (= (current-column) column)))))))

(ert-deftest chai-test-collect-comments ()
  "Collect CHAI_COMMENT blocks."
  (chai-test--with-temp-org "#+BEGIN_CHAI_COMMENT\nFirst comment.\n#+END_CHAI_COMMENT\n\n#+BEGIN_CHAI_COMMENT\nSecond.\n#+END_CHAI_COMMENT"
    (let ((comments (chai--collect-comments)))
      (should (= (length comments) 2))
      (should (string= (plist-get (nth 0 comments) :text) "First comment."))
      (should (string= (plist-get (nth 1 comments) :text) "Second.")))))

(ert-deftest chai-test-collect-comments-ignores-other-blocks ()
  "Only CHAI_COMMENT special blocks are collected."
  (chai-test--with-temp-org "#+BEGIN_EXAMPLE\nCode\n#+END_EXAMPLE\n#+BEGIN_CHAI_COMMENT\nNote\n#+END_CHAI_COMMENT"
    (let ((comments (chai--collect-comments)))
      (should (= (length comments) 1))
      (should (string= (plist-get (car comments) :text) "Note")))))

(ert-deftest chai-test-insert-comment-empty ()
  "chai-insert-comment creates an empty block at point."
  (chai-test--with-temp-org ""
    (chai-insert-comment)
    (should (string-search "#+BEGIN_CHAI_COMMENT" (buffer-string)))
    (should (string-search "#+END_CHAI_COMMENT" (buffer-string)))))

(ert-deftest chai-test-insert-comment-region ()
  "chai-insert-comment wraps the active region."
  (chai-test--with-temp-org "my note"
    (setq-local transient-mark-mode t)
    (goto-char 1)
    (set-mark (point))
    (goto-char 8)
    (activate-mark)
    (chai-insert-comment)
    (should (string-search "#+BEGIN_CHAI_COMMENT" (buffer-string)))
    (should (string-search "my note" (buffer-string)))))

(ert-deftest chai-test-add-comment ()
  "chai-add-comment inserts a complete comment block."
  (chai-test--with-temp-org ""
    (chai-add-comment "direct note")
    (should (string= (buffer-string)
                     "#+BEGIN_CHAI_COMMENT\ndirect note\n#+END_CHAI_COMMENT\n"))))

(ert-deftest chai-test-add-comment-empty-errors ()
  "chai-add-comment rejects empty comments."
  (chai-test--with-temp-org ""
    (should-error (chai-add-comment "  ") :type 'user-error)))

(ert-deftest chai-test-mouse-copy-text ()
  "chai-mouse-copy-text copies the highlighted text to the kill ring."
  (chai-test--with-temp-org "[[chai:important][copy me]]"
    (chai-mouse-copy-text)
    (should (string= (current-kill 0 t) "copy me"))))

(ert-deftest chai-test-mouse-copy-text-with-pos ()
  "chai-mouse-copy-text uses POS even when point is elsewhere."
  (chai-test--with-temp-org "[[chai:important][target]] other text"
    (goto-char (point-max))
    (chai-mouse-copy-text 1)
    (should (string= (current-kill 0 t) "target"))))

(ert-deftest chai-test-mouse-remove-highlight-with-pos ()
  "chai-mouse-remove-highlight uses POS even when point is elsewhere."
  (chai-test--with-temp-org "[[chai:important][target]] other text"
    (goto-char (point-max))
    (chai-mouse-remove-highlight 1)
    (should (string= (buffer-string) "target other text"))))

(ert-deftest chai-test-context-menu-uses-click-position ()
  "chai-context-menu produces commands that operate on the clicked position.
This verifies that the lambdas bound in the context menu capture the click
position and use it even when point has moved elsewhere."
  (chai-test--with-temp-org "[[chai:important][target]] other text"
    (goto-char (point-max))
    (let* ((menu (make-sparse-keymap))
           (click-event (list 'mouse-3 (posn-at-point 1 (selected-window))))
           (result-menu (chai-context-menu menu click-event))
           (cmd (lookup-key result-menu [chai-copy-text])))
      (should (commandp cmd))
      (funcall cmd)
      (should (string= (current-kill 0 t) "target")))))

(ert-deftest chai-test-mouse-remove-highlight ()
  "chai-mouse-remove-highlight restores the plain text."
  (chai-test--with-temp-org "[[chai:important][remove me]]"
    (chai-mouse-remove-highlight)
    (should (string= (buffer-string) "remove me"))))

(ert-deftest chai-test-mouse-change-type ()
  "chai-mouse-change-type updates the highlight type."
  (chai-test--with-temp-org "[[chai:important][text]]"
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "key")))
      (chai-mouse-change-type)
      (should (string= (buffer-string) "[[chai:key][text]]")))))

(ert-deftest chai-test-mouse-edit-annotation ()
  "chai-mouse-edit-annotation updates the highlight note."
  (chai-test--with-temp-org "[[chai:important][text]]"
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "new note")))
      (chai-mouse-edit-annotation)
      (should (string= (buffer-string) "[[chai:important:new note][text]]")))))

(ert-deftest chai-test-mouse-errors-at-non-highlight ()
  "Mouse commands signal user-error when not on a chai link."
  (chai-test--with-temp-org "plain text"
    (should-error (chai-mouse-copy-text) :type 'user-error)
    (should-error (chai-mouse-change-type) :type 'user-error)
    (should-error (chai-mouse-edit-annotation) :type 'user-error)))

(ert-deftest chai-test-mouse-region-highlight ()
  "chai-mouse-highlight-region highlights the active region."
  (chai-test--with-temp-org "select this text"
    (setq-local transient-mark-mode t)
    (goto-char 1)
    (set-mark (point))
    (goto-char 12)
    (activate-mark)
    (chai-mouse-highlight-region "important")
    (should (string= (buffer-string) "[[chai:important][select this]] text"))))

;;; Export preview

(defun chai-test--kill-preview-buffer ()
  "Kill the preview buffer if it exists."
  (when (get-buffer "*Chai Export Preview*")
    (kill-buffer "*Chai Export Preview*")))

(defun chai-test--y-or-n-p-nil (&rest _)
  "Mock `y-or-n-p' returning nil."
  nil)

(defun chai-test--y-or-n-p-t (&rest _)
  "Mock `y-or-n-p' returning t."
  t)

(ert-deftest chai-test-export-preview-file-name ()
  "Preview file name is derived from source base with _chai.org suffix."
  (let ((chai-export-preview-directory "/tmp/preview"))
    (should (string= (chai--export-preview-file-name "/path/book.org")
                     "/tmp/preview/book_chai.org"))))

(ert-deftest chai-test-export-preview-file-name-managed ()
  "Managed file names are only suffixed, not parsed."
  (let ((chai-export-preview-directory "/tmp/preview"))
    (should (string= (chai--export-preview-file-name "/path/20250101T120000--foo__bar.org")
                     "/tmp/preview/20250101T120000--foo__bar_chai.org"))))

(ert-deftest chai-test-export-preview-file-name-nil ()
  "Nil source file signals user-error."
  (should-error (chai--export-preview-file-name nil) :type 'user-error))

(ert-deftest chai-test-export-preview-buffer ()
  "Preview buffer contains rendered Org content and is editable."
  (chai-test--kill-preview-buffer)
  (let ((chai-export-preview-directory "/tmp/preview"))
    (chai-test--with-temp-org "[[chai:important][hi]]"
      (setq-local buffer-file-name "/tmp/source.org")
      (chai-test--with-export-env
        (chai-export-preview)
        (let ((buf (get-buffer "*Chai Export Preview*")))
          (should buf)
          (with-current-buffer buf
            (should (string= (buffer-string)
                             (concat "#+TITLE: source\n"
                                     "#+SOURCE: /tmp/source.org\n"
                                     "#+EXPORTED_AT: 2026-06-21 12:30\n"
                                     "#+SEQ_TODO: IMPORTANT IDEA KEY COMMENT\n\n"
                                     "* IMPORTANT hi\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n")))
            (should (eq major-mode 'org-mode))
            (should (not buffer-read-only))
            (should (string= buffer-file-name "/tmp/preview/source_chai.org"))
            (should (member "KEY" org-todo-keywords-1))
            (should (buffer-modified-p))))))))

(ert-deftest chai-test-export-preview-buffer-saves-file ()
  "Preview buffer can be saved to the associated preview file."
  (chai-test--kill-preview-buffer)
  (let ((chai-export-preview-directory (make-temp-file "chai-preview" t)))
    (chai-test--with-temp-org "#+TITLE: Source Title\n[[chai:important][hi]]"
      (setq-local buffer-file-name "/tmp/source.org")
      (chai-test--with-export-env
        (chai-export-preview)
        (with-current-buffer "*Chai Export Preview*"
          (basic-save-buffer)
          (let ((file buffer-file-name))
            (should (file-exists-p file))
            (should (string-match-p
                     "#\\+TITLE: Source Title"
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string)))))
          (should-not (buffer-modified-p)))))))

(ert-deftest chai-test-export-preview-save-writes-file ()
  "chai-export-preview-save writes the current export directly."
  (let ((chai-export-preview-directory (make-temp-file "chai-preview" t)))
    (chai-test--with-temp-org "#+TITLE: Source Title\n[[chai:important][hi]]"
      (setq-local buffer-file-name "/tmp/source.org")
      (chai-test--with-export-env
        (let ((file (chai-export-preview-save)))
          (should (file-exists-p file))
          (should (string= file (expand-file-name "source_chai.org" chai-export-preview-directory)))
          (should (string-match-p
                   "#\\+TITLE: Source Title"
                   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))))))))

(ert-deftest chai-test-export-preview-no-file-name ()
  "Preview command errors from a source buffer without file name."
  (chai-test--kill-preview-buffer)
  (chai-test--with-temp-org "[[chai:important][hi]]"
    (should-error (chai-export-preview) :type 'user-error)))

(ert-deftest chai-test-export-preview-source-links ()
  "Source links in the preview point to the original source file."
  (chai-test--kill-preview-buffer)
  (let ((chai-export-preview-directory "/tmp/preview"))
    (chai-test--with-temp-org "[[chai:important][hi]]"
      (setq-local buffer-file-name "/tmp/source.org")
      (chai-export-preview)
      (with-current-buffer "*Chai Export Preview*"
        (should (string-search "file:/tmp/source.org" (buffer-string)))
        (should-not (string-search "preview/source_chai.org" (buffer-string)))))))

(ert-deftest chai-test-export-preview-guards-modified-buffer ()
  "Rerunning the command on a modified preview buffer asks before replacing."
  (chai-test--kill-preview-buffer)
  (let ((chai-export-preview-directory "/tmp/preview"))
    (chai-test--with-temp-org "[[chai:important][hi]]"
      (setq-local buffer-file-name "/tmp/source.org")
      (chai-export-preview)
      (let ((buf (get-buffer "*Chai Export Preview*")))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert "\nuser edit")
          (set-buffer-modified-p t))
        (advice-add 'y-or-n-p :override #'chai-test--y-or-n-p-nil)
        (unwind-protect
            (should-error (chai-export-preview) :type 'user-error)
          (advice-remove 'y-or-n-p #'chai-test--y-or-n-p-nil))
        (with-current-buffer buf
          (should (string-search "user edit" (buffer-string)))
          (should (buffer-modified-p)))))))

(ert-deftest chai-test-export-preview-replaces-when-confirmed ()
  "Rerunning the command replaces a modified preview buffer when confirmed."
  (chai-test--kill-preview-buffer)
  (let ((chai-export-preview-directory "/tmp/preview"))
    (chai-test--with-temp-org "[[chai:important][hi]]"
      (setq-local buffer-file-name "/tmp/source.org")
      (chai-export-preview)
      (let ((buf (get-buffer "*Chai Export Preview*")))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert "\nuser edit")
          (set-buffer-modified-p t))
        (advice-add 'y-or-n-p :override #'chai-test--y-or-n-p-t)
        (unwind-protect
            (chai-export-preview)
          (advice-remove 'y-or-n-p #'chai-test--y-or-n-p-t))
        (with-current-buffer buf
          (should-not (string-search "user edit" (buffer-string)))
          (should (buffer-modified-p)))))))

(provide 'chai-test)
;;; chai-test.el ends here
