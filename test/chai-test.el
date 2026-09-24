;;; chai-test.el --- ERT tests for chai.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'chai)
(require 'chai-library-table)
(require 'chai-search)
(require 'chai-context)
(require 'chai-ask)
(require 'chai-superchat)
(require 'chai-vector)

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

(ert-deftest chai-test-chirp-capture-writes-normal-library-org-file ()
  "Capture a Chirp tweet as a saved, normal Org file in the library."
  (let ((library (make-temp-file "chai-chirp-capture" t))
        (entry '(:kind tweet
                 :id "123"
                 :text "First paragraph.\n\nSecond paragraph."
                 :author-name "Alice"
                 :author-handle "alice"
                 :created-at "2026-08-10T12:13:14Z"
                 :url "https://x.com/alice/status/123")))
    (unwind-protect
        (let ((chai-library-directory library))
          (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                    ((symbol-function 'chirp-entry-at-point) (lambda () entry))
                    ((symbol-function 'read-string)
                     (lambda (prompt initial &rest _)
                       (should (equal prompt "Chirp title: "))
                       (should (equal initial "First paragraph. Second paragraph."))
                       "A useful reading note"))
                    ((symbol-function 'format-time-string)
                     (lambda (&rest _) "20260810T121314")))
            (let ((file (chai-capture-chirp-entry)))
              (should (file-exists-p file))
              (should (string-match-p (rx "20260810T121314__alice__A-useful-reading-note==chirp.org" string-end)
                                      file))
              (let ((contents (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string))))
                (should (string-search "#+TITLE: A useful reading note" contents))
                (should (string-search "* A useful reading note" contents))
                (should (string-search "#+AUTHOR: Alice" contents))
                (should (string-search "#+FILETAGS: :chirp:" contents))
                (should (string-search ":ID: 20260810T121314" contents))
                (should (string-search ":CHIRP_ID: 123" contents))
                (should (string-search "https://x.com/alice/status/123" contents))
                (should (string-search
                         "#+BEGIN_QUOTE\nFirst paragraph.\n\nSecond paragraph.\n#+END_QUOTE"
                         contents))
                (should-not (string-search "#+BEGIN_CHAI" contents))))))
      (chai-test--kill-file-buffers-under library)
      (delete-directory library t))))

(ert-deftest chai-test-chirp-capture-rejects-unsaveable-entries ()
  "Reject non-tweets and tweets without stable content or provenance."
  (let ((library (make-temp-file "chai-chirp-capture" t)))
    (unwind-protect
        (let ((chai-library-directory library))
          (dolist (entry '((:kind user :id "123" :text "Profile" :url "https://x.com/alice")
                           (:kind tweet :id "123" :text "" :url "https://x.com/alice/status/123")
                           (:kind tweet :id "123" :text "Text")
                           (:kind tweet :text "Text" :url "https://x.com/alice/status/123")))
            (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                      ((symbol-function 'chirp-entry-at-point) (lambda () entry)))
              (should-error (chai-capture-chirp-entry) :type 'user-error)))
          (should-not (directory-files library nil "\\.org\\'")))
      (chai-test--kill-file-buffers-under library)
      (delete-directory library t))))

(ert-deftest chai-test-telega-capture-writes-normal-library-org-file ()
  "Capture a Telega message as a saved, normal Org file in the library."
  (let ((library (make-temp-file "chai-telega-capture" t))
        (message '(:id 123 :chat_id -10042 :date 1786501234)))
    (unwind-protect
        (let ((chai-library-directory library))
          (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                    ((symbol-function 'telega-msg-for-interactive) (lambda () message))
                    ((symbol-function 'telega-msg-content-text)
                     (lambda (&rest _) "First paragraph.\n\nSecond paragraph."))
                    ((symbol-function 'telega-msg-sender) (lambda (&rest _) 'sender))
                    ((symbol-function 'telega-msg-sender-title) (lambda (&rest _) "Alice"))
                    ((symbol-function 'telega-msg-sender-username) (lambda (&rest _) "alice"))
                    ((symbol-function 'telega-msg-chat) (lambda (&rest _) 'chat))
                    ((symbol-function 'telega-chat-title) (lambda (&rest _) "Emacs Group"))
                    ((symbol-function 'telega-tme-internal-link-to)
                     (lambda (&rest _) "telega:-10042#123"))
                    ((symbol-function 'format-time-string)
                     (lambda (&rest _) "20260811T102030")))
            (let ((file (chai-capture-telega-message)))
              (should (file-exists-p file))
              (should (string-match-p "20260811T102030__alice__" file))
              (should (string-suffix-p "-10042-123==telega.org" file))
              (let ((contents (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string))))
                (should (string-search "#+TITLE: First paragraph. Second paragraph." contents))
                (should (string-search "#+AUTHOR: Alice" contents))
                (should (string-search "#+FILETAGS: :telega:" contents))
                (should (string-search ":ID: 20260811T102030" contents))
                (should (string-search ":TELEGA_CHAT_ID: -10042" contents))
                (should (string-search ":TELEGA_MESSAGE_ID: 123" contents))
                (should (string-search ":TELEGA_CHAT: Emacs Group" contents))
                (should (string-search "telega:-10042#123" contents))
                (should (string-search
                         "#+BEGIN_QUOTE\nFirst paragraph.\n\nSecond paragraph.\n#+END_QUOTE"
                         contents))
                (should-not (string-search "#+BEGIN_CHAI" contents))))))
      (chai-test--kill-file-buffers-under library)
      (delete-directory library t))))

(ert-deftest chai-test-telega-capture-rejects-unsaveable-messages ()
  "Reject unavailable, textless, unidentifiable, or unlinked Telega messages."
  (let ((library (make-temp-file "chai-telega-capture" t)))
    (unwind-protect
        (let ((chai-library-directory library))
          (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil)))
            (should-error (chai-capture-telega-message) :type 'user-error))
          (dolist (message '((:id 123 :chat_id 42 :text "" :source "telega:42#123")
                             (:chat_id 42 :text "Text" :source "telega:42#123")
                             (:id 123 :text "Text" :source "telega:42#123")
                             (:id 123 :chat_id 42 :text "Text")))
            (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                      ((symbol-function 'telega-msg-for-interactive) (lambda () message))
                      ((symbol-function 'telega-msg-content-text)
                       (lambda (msg &rest _) (plist-get msg :text)))
                      ((symbol-function 'telega-msg-sender) (lambda (&rest _) 'sender))
                      ((symbol-function 'telega-msg-sender-title) (lambda (&rest _) "Alice"))
                      ((symbol-function 'telega-msg-sender-username) (lambda (&rest _) "alice"))
                      ((symbol-function 'telega-msg-chat) (lambda (&rest _) 'chat))
                      ((symbol-function 'telega-chat-title) (lambda (&rest _) "Emacs Group"))
                      ((symbol-function 'telega-tme-internal-link-to)
                       (lambda (msg &rest _) (plist-get msg :source))))
              (should-error (chai-capture-telega-message) :type 'user-error)))
          (should-not (directory-files library nil "\\.org\\'")))
      (chai-test--kill-file-buffers-under library)
      (delete-directory library t))))

(ert-deftest chai-test-eww-capture-writes-normal-library-org-file ()
  "Capture an EWW page region as a saved, normal Org file in the library."
  (let ((library (make-temp-file "chai-eww-capture" t)))
    (unwind-protect
        (with-temp-buffer
          (insert "Ignore\nFirst paragraph.\n\nSecond paragraph.\nIgnore")
          (goto-char (point-min))
          (search-forward "First paragraph.")
          (let ((beg (match-beginning 0)))
            (search-forward "Second paragraph.")
            (let ((end (match-end 0))
                  (chai-library-directory library))
              (cl-progv '(eww-current-url eww-current-title)
                  '("https://example.com/article" "Example Article")
                (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                          ((symbol-function 'use-region-p) (lambda () t))
                          ((symbol-function 'region-beginning) (lambda () beg))
                          ((symbol-function 'region-end) (lambda () end))
                          ((symbol-function 'format-time-string)
                           (lambda (&rest _) "20260811T102030")))
                  (let ((file (chai-capture-eww-region)))
                    (should (file-exists-p file))
                    (should (string-match-p "20260811T102030__web__" file))
                    (should (string-suffix-p "==eww.org" file))
                    (let ((contents (with-temp-buffer
                                      (insert-file-contents file)
                                      (buffer-string))))
                      (should (string-search "#+TITLE: Example Article" contents))
                      (should (string-search "#+AUTHOR: web" contents))
                      (should (string-search "#+FILETAGS: :eww:" contents))
                      (should (string-search ":ID: 20260811T102030" contents))
                      (should (string-search ":EWW_URL: https://example.com/article" contents))
                      (should (string-search ":EWW_TITLE: Example Article" contents))
                      (should (string-search
                               "#+BEGIN_QUOTE\nFirst paragraph.\n\nSecond paragraph.\n#+END_QUOTE"
                               contents))
                      (should (string-search "https://example.com/article" contents))
                      (should-not (string-search "#+BEGIN_CHAI" contents)))))))))
      (chai-test--kill-file-buffers-under library)
      (delete-directory library t))))

(ert-deftest chai-test-eww-capture-rejects-unsaveable-regions ()
  "Reject unavailable, unselected, empty, or unlinked EWW content."
  (let ((library (make-temp-file "chai-eww-capture" t)))
    (unwind-protect
        (let ((chai-library-directory library))
          (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil)))
            (should-error (chai-capture-eww-region) :type 'user-error))
          (with-temp-buffer
            (insert "Text")
            (cl-progv '(eww-current-url eww-current-title)
                '("https://example.com/article" "Example Article")
              (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                        ((symbol-function 'use-region-p) (lambda () nil)))
                (should-error (chai-capture-eww-region) :type 'user-error))
              (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                        ((symbol-function 'use-region-p) (lambda () t))
                        ((symbol-function 'region-beginning) (lambda () (point-min)))
                        ((symbol-function 'region-end) (lambda () (point-min))))
                (should-error (chai-capture-eww-region) :type 'user-error)))
            (cl-progv '(eww-current-url eww-current-title) '(nil "Example Article")
              (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                        ((symbol-function 'use-region-p) (lambda () t))
                        ((symbol-function 'region-beginning) (lambda () (point-min)))
                        ((symbol-function 'region-end) (lambda () (point-max))))
                (should-error (chai-capture-eww-region) :type 'user-error))))
          (should-not (directory-files library nil "\\.org\\'")))
      (chai-test--kill-file-buffers-under library)
      (delete-directory library t))))

(ert-deftest chai-test-collect-plain-highlight ()
  "Collect a single plain highlight."
  (chai-test--with-temp-org "[[chai:important][highlight text]]"
    (let ((hl (car (chai--collect-highlights))))
      (should hl)
      (should (string= (plist-get hl :type) "important"))
      (should (null (plist-get hl :note)))
      (should (string= (plist-get hl :text) "highlight text"))
      (should (= (plist-get hl :line) 1))
      (should (= (plist-get hl :beg) 1)))))

(ert-deftest chai-test-highlight-region-resets-org-element-cache ()
  "Highlighting resets Org's element cache after changing Org syntax."
  (chai-test--with-temp-org "highlight me"
    (let ((calls 0))
      (cl-letf (((symbol-function 'org-element-cache-reset)
                 (lambda (&rest _) (cl-incf calls))))
        (chai-highlight-region 1 13 "important")
        (should (= calls 1))))))

(ert-deftest chai-test-highlight-region-cross-paragraph-creates-source-block ()
  "Plain source blocks retain all paragraphs and remain collectable."
  (chai-test--with-temp-org "First paragraph.\n\nSecond paragraph."
    (chai-highlight-region (point-min) (point-max) "important")
    (should (string= (buffer-string)
                     "#+BEGIN_CHAI :type important\nFirst paragraph.\n\nSecond paragraph.\n#+END_CHAI"))
    (let ((highlight (car (chai--collect-highlights))))
      (should (string= (plist-get highlight :type) "important"))
      (should-not (plist-get highlight :note))
      (should (string= (plist-get highlight :text) "First paragraph.\n\nSecond paragraph."))
      (should (= (plist-get highlight :line) 2)))))

(ert-deftest chai-test-highlight-region-single-line-uses-chai-link ()
  "A selection without a newline uses the persistent Chai link syntax."
  (chai-test--with-temp-org "highlight me"
    (chai-highlight-region (point-min) (point-max) "important")
    (should (string= (buffer-string)
                     "[[chai:important][highlight me]]"))
    (let ((highlight (car (chai--collect-highlights))))
      (should (string= (plist-get highlight :type) "important"))
      (should-not (plist-get highlight :note))
      (should (string= (plist-get highlight :text) "highlight me")))))

(ert-deftest chai-test-highlight-annotate-single-line-uses-chai-link ()
  "An annotated selection without a newline keeps the link annotation syntax."
  (chai-test--with-temp-org "highlight me"
    (chai-highlight-annotate (point-min) (point-max) "idea" "note with : colon")
    (should (string= (buffer-string)
                     "[[chai:idea:note with : colon][highlight me]]"))
    (let ((highlight (car (chai--collect-highlights))))
      (should (string= (plist-get highlight :type) "idea"))
      (should (string= (plist-get highlight :note) "note with : colon"))
      (should (string= (plist-get highlight :text) "highlight me")))))

(ert-deftest chai-test-highlight-annotate-cross-paragraph-preserves-note ()
  "Annotated source blocks preserve notes, including colons."
  (chai-test--with-temp-org "First paragraph.\n\nSecond paragraph."
    (chai-highlight-annotate (point-min) (point-max) "idea" "note with : colon")
    (should (string= (buffer-string)
                     "#+BEGIN_CHAI :type idea :note \"note with : colon\"\nFirst paragraph.\n\nSecond paragraph.\n#+END_CHAI"))
    (let ((highlight (car (chai--collect-highlights))))
      (should (string= (plist-get highlight :type) "idea"))
      (should (string= (plist-get highlight :note) "note with : colon"))
      (should (string= (plist-get highlight :text) "First paragraph.\n\nSecond paragraph.")))))

(ert-deftest chai-test-source-block-overlay-covers-cross-paragraph-content ()
  "One face overlay spans all paragraphs in a Chai source block."
  (chai-test--with-temp-org "First paragraph.\n\nSecond paragraph."
    (chai-highlight-region (point-min) (point-max) "important")
    (let ((overlay (cl-find-if (lambda (candidate)
                                 (overlay-get candidate 'chai-block-ov))
                               (overlays-in (point-min) (point-max)))))
      (should overlay)
      (should (eq (overlay-get overlay 'face) 'org-quote))
      (goto-char (point-min))
      (search-forward "First paragraph.")
      (should (memq overlay (overlays-at (1- (point)))))
      (search-forward "Second paragraph.")
      (should (memq overlay (overlays-at (1- (point))))))))

(ert-deftest chai-test-source-block-uses-org-quote-face ()
  "Cross-line Chai blocks use Org's quote style for their contents."
  (chai-test--with-temp-org "First paragraph.\n\nSecond paragraph."
    (chai-highlight-region (point-min) (point-max) "important")
    (let ((overlay (cl-find-if (lambda (candidate)
                                 (overlay-get candidate 'chai-block-ov))
                               (overlays-in (point-min) (point-max)))))
      (should overlay)
      (should (eq (overlay-get overlay 'face) 'org-quote)))))

(ert-deftest chai-test-source-block-actions-preserve-cross-paragraph-text ()
  "Block actions work without flattening or losing cross-paragraph text."
  (chai-test--with-temp-org "First paragraph.\n\nSecond paragraph."
    (chai-highlight-region (point-min) (point-max) "important")
    (goto-char (point-min))
    (search-forward "First paragraph.")
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "idea")))
      (chai-mouse-change-type))
    (should (string-prefix-p "#+BEGIN_CHAI :type idea\n" (buffer-string)))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "saved note")))
      (chai-mouse-edit-annotation))
    (should (string-prefix-p "#+BEGIN_CHAI :type idea :note \"saved note\"\n"
                             (buffer-string)))
    (chai-mouse-copy-text)
    (should (string= (current-kill 0 t) "First paragraph.\n\nSecond paragraph."))
    (chai-remove-highlight)
    (should (string= (buffer-string) "First paragraph.\n\nSecond paragraph."))))

(ert-deftest chai-test-source-block-export-is-one-heading ()
  "Cross-paragraph source blocks export as one plain heading entry."
  (chai-test--with-temp-org "First paragraph.\n\nSecond paragraph."
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-highlight-region (point-min) (point-max) "important")
    (chai-test--with-export-env
      (should (string= (chai--export-items-as-org (chai--collect-items) buffer-file-name)
                       (concat "* [IMPORTANT] First paragraph. Second paragraph.\n"
                               ":PROPERTIES:\n"
                               ":SOURCE: [[file:/tmp/source.org::2][L2]]\n"
                               ":END:\n\n"
                               "First paragraph.\n\nSecond paragraph.\n"))))))

(ert-deftest chai-test-collect-annotated-highlight ()
  "Collect a highlight with a note."
  (chai-test--with-temp-org "[[chai:idea:my note][highlight text]]"
    (let ((hl (car (chai--collect-highlights))))
      (should (string= (plist-get hl :type) "idea"))
      (should (string= (plist-get hl :note) "my note"))
      (should (string= (plist-get hl :text) "highlight text")))))

(ert-deftest chai-test-collect-note-with-colons ()
  "Collect a highlight whose note contains colons."
  (chai-test--with-temp-org "[[chai:question:url: https://example.org][link]]"
    (let ((hl (car (chai--collect-highlights))))
      (should (string= (plist-get hl :type) "question"))
      (should (string= (plist-get hl :note) "url: https://example.org"))
      (should (string= (plist-get hl :text) "link")))))

(ert-deftest chai-test-collect-ignores-non-chai-links ()
  "Non-chai Org links are ignored."
  (chai-test--with-temp-org "[[https://example.org][example]] [[chai:key][keep me]]"
    (let ((hls (chai--collect-highlights)))
      (should (= (length hls) 1))
      (should (string= (plist-get (car hls) :text) "keep me")))))

(ert-deftest chai-test-collect-ignores-malformed-links ()
  "Malformed links are ignored without errors."
  (chai-test--with-temp-org "[[no closing bracket [[chai:ok][fine]]"
    (let ((hls (chai--collect-highlights)))
      (should (= (length hls) 1))
      (should (string= (plist-get (car hls) :text) "fine")))))

(ert-deftest chai-test-collect-respects-narrowing ()
  "Collection respects buffer narrowing (used for region/subtree scopes)."
  (chai-test--with-temp-org "[[chai:important][before]] [[chai:key][after]]"
    (save-restriction
      (narrow-to-region 1 30)
      (let ((hls (chai--collect-highlights)))
        (should (= (length hls) 1))
        (should (string= (plist-get (car hls) :text) "before"))))))

(ert-deftest chai-test-collect-in-subtree ()
  "Collection inside a narrowed subtree returns only that subtree's highlights."
  (chai-test--with-temp-org
      "* Section A\n[[chai:important][in A]]\n* Section B\n[[chai:key][in B]]"
    (goto-char 1)
    (save-restriction
      (org-narrow-to-subtree)
      (let ((hls (chai--collect-highlights)))
        (should (= (length hls) 1))
        (should (string= (plist-get (car hls) :text) "in A"))))))

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
  "Org export emits a plain Chai headline for a highlight."
  (chai-test--with-temp-org "[[chai:important][hi]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items "/tmp/source.org")))
        (should (string= out (concat "* [IMPORTANT] hi\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n")))))))

(ert-deftest chai-test-export-org-annotated-highlight ()
  "Org export keeps annotation as plain text under the headline."
  (chai-test--with-temp-org "[[chai:idea:my note][hi]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items "/tmp/source.org")))
        (should (string= out (concat "* [IDEA] hi\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n\n"
                                     "my note\n")))))))

(ert-deftest chai-test-export-org-source-links ()
  "Org export puts the source line link in the properties drawer."
  (chai-test--with-temp-org "[[chai:important][This is a long highlight text]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items "/tmp/source.org")))
        (should (string= out (concat "* [IMPORTANT] This is a long highlight text\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n")))))))

(ert-deftest chai-test-export-org-managed-source-links-use-chai-id ()
  "Managed Chai Library sources export stable ID links instead of file paths."
  (chai-test--with-temp-org "[[chai:important][hi]]"
    (setq-local buffer-file-name "/tmp/20260810T102030__Source.org")
    (let* ((items (chai--collect-items))
           (out (chai--export-items-as-org items buffer-file-name)))
      (should (string-search "[[chai:20260810T102030::1][L1]]" out))
      (should-not (string-search "file:/tmp/20260810T102030__Source.org" out)))))

(ert-deftest chai-test-export-org-comment-block ()
  "Org export emits a COMMENT headline for free-standing comments."
  (chai-test--with-temp-org "#+BEGIN_CHAI_COMMENT\ncomment text\n#+END_CHAI_COMMENT"
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items nil)))
        (should (string= out (concat "* [COMMENT] comment text\n"
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
        (should (string= out (concat "* [IMPORTANT] first\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n\n"
                                     "* [COMMENT] middle\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::2][L2]]\n"
                                     ":END:\n\n"
                                     "* [KEY] second\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::5][L5]]\n"
                                     ":END:\n")))))))

(ert-deftest chai-test-export-org-uses-prefixes-without-todo-keywords ()
  "Org export uses visible prefixes without adding TODO keywords."
  (chai-test--with-temp-org "[[chai:question][q]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (let ((chai-highlight-types '(("question" . chai-highlight-question))))
      (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "2026-06-21 12:30")))
        (let* ((items (chai--collect-items))
               (out (chai--export-items-as-org items "/tmp/source.org")))
          (should (string= out (concat "* [QUESTION] q\n"
                                       ":PROPERTIES:\n"
                                       ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                       ":END:\n")))
          (should-not (string-search "#+SEQ_TODO:" out)))))))

(ert-deftest chai-test-export-org-does-not-add-document-header ()
  "Org export does not add a document-level title or metadata header."
  (chai-test--with-temp-org "#+TITLE: Real Book\n[[chai:important][hi]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items "/tmp/source.org")))
        (should (string= out (concat "* [IMPORTANT] hi\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::2][L2]]\n"
                                     ":END:\n")))
        (should-not (string-search "#+TITLE:" out))
        (should-not (string-search "#+SOURCE:" out))
        (should-not (string-search "#+EXPORTED_AT:" out))))))

(ert-deftest chai-test-export-org-same-line-order ()
  "Org export orders same-line highlights by buffer position."
  (chai-test--with-temp-org "[[chai:key][a]] [[chai:important][b]]"
    (setq-local buffer-file-name "/tmp/source.org")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items "/tmp/source.org")))
        (should (string= out (concat "* [KEY] a\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n\n"
                                     "* [IMPORTANT] b\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n")))))))

(ert-deftest chai-test-export-org-multiline-highlight ()
  "Org export preserves multiline highlight text as plain body text."
  (chai-test--with-export-env
    (let* ((item (list :kind 'highlight :type "important" :text "line1\nline2"
                       :note nil :line 1 :beg 1))
           (out (chai--export-items-as-org (list item) "/tmp/source.org")))
      (should (string= out (concat "* [IMPORTANT] line1 line2\n"
                                   ":PROPERTIES:\n"
                                   ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                   ":END:\n\n"
                                   "line1\n"
                                   "line2\n"))))))


(ert-deftest chai-test-export-org-multiline-comment ()
  "Org export preserves multiline comments as plain body text."
  (chai-test--with-temp-org (concat "#+BEGIN_CHAI_COMMENT" "\n" "line1" "\n" "line2" "\n" "#+END_CHAI_COMMENT")
    (chai-test--with-export-env
      (let* ((items (chai--collect-items))
             (out (chai--export-items-as-org items nil)))
        (should (string= out (concat "* [COMMENT] line1 line2\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: \n"
                                     ":END:\n\n"
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

(ert-deftest chai-test-highlight-faces-have-visible-foreground ()
  "Configured highlight faces do not leave foreground color unspecified."
  (dolist (type chai-highlight-types)
    (let ((foreground (face-attribute (cdr type) :foreground nil t)))
      (should (and (stringp foreground)
                   (not (string= foreground "unspecified")))))))

(ert-deftest chai-test-refreshes-highlight-faces-for-current-frame ()
  "Refreshing annotations recalculates faces after a frame mode change."
  (let ((original-mode (frame-parameter nil 'background-mode)))
    (unwind-protect
        (progn
          (set-frame-parameter nil 'background-mode 'light)
          (chai-test--with-temp-org
              "#+BEGIN_CHAI :type key\ntext\n#+END_CHAI"
            (chai--render-annotations)
            (should (string= (face-attribute 'chai-highlight-key :background nil t)
                             "#FFE082"))))
      (set-frame-parameter nil 'background-mode original-mode)
      (chai--refresh-face-specs))))

(ert-deftest chai-test-refresh-face-specs-preserves-runtime-face-customization ()
  "Refreshing annotations does not overwrite a runtime face customization."
  (unwind-protect
      (progn
        (set-face-attribute 'chai-highlight-key nil
                            :background "#123456"
                            :foreground "#abcdef")
        (chai--refresh-face-specs)
        (should (string= (face-attribute 'chai-highlight-key :background nil t)
                         "#123456"))
        (should (string= (face-attribute 'chai-highlight-key :foreground nil t)
                         "#abcdef")))
    (face-spec-recalc 'chai-highlight-key nil)
    (put 'chai-highlight-key 'face-modified nil)))

(ert-deftest chai-test-generic-capture-writes-normal-library-org-file ()
  "Generic capture saves a region with file source metadata."
  (let ((library (make-temp-file "chai-generic-capture" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local buffer-file-name "/tmp/notes.org")
          (insert "Ignore\nFirst paragraph.\nSecond paragraph.\nIgnore")
          (goto-char (point-min))
          (search-forward "First paragraph.")
          (let ((beg (match-beginning 0)))
            (search-forward "Second paragraph.")
            (let ((end (match-end 0))
                  (chai-library-directory library))
              (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                        ((symbol-function 'region-beginning) (lambda () beg))
                        ((symbol-function 'region-end) (lambda () end))
                        ((symbol-function 'format-time-string)
                         (lambda (&rest _) "20260811T102030")))
                (let ((file (chai-capture)))
                  (should (file-exists-p file))
                  (should (string-prefix-p
                           "20260811T102030__notes.org__"
                           (file-name-nondirectory file)))
                  (should (string-suffix-p "==capture_org.org" file))
                  (let ((contents (with-temp-buffer
                                    (insert-file-contents file)
                                    (buffer-string))))
                    (should (string-search "#+TITLE: First paragraph. Second paragraph." contents))
                    (should (string-search "#+AUTHOR: notes.org" contents))
                    (should (string-search "#+FILETAGS: :capture:org:" contents))
                    (should (string-search ":ID: 20260811T102030" contents))
                    (should (string-search ":CHAI_SOURCE: /tmp/notes.org" contents))
                    (should (string-search ":CHAI_SOURCE_LINE: 2" contents))
                    (should (string-search ":CHAI_SOURCE_MODE: org-mode" contents))
                    (should (string-search
                             "#+BEGIN_QUOTE\nFirst paragraph.\nSecond paragraph.\n#+END_QUOTE"
                             contents))
                    (should (string-search "file:/tmp/notes.org::2" contents))
                    (should-not (string-search "#+BEGIN_CHAI" contents))))))))
      (chai-test--kill-file-buffers-under library)
      (delete-directory library t))))

(ert-deftest chai-test-generic-capture-falls-back-to-current-line ()
  "Without a region, generic capture saves the current line."
  (let ((library (make-temp-file "chai-generic-capture" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local buffer-file-name "/tmp/notes.org")
          (insert "Ignore\nKeep this line")
          (goto-char (point-max))
          (let ((chai-library-directory library))
            (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                      ((symbol-function 'format-time-string)
                       (lambda (&rest _) "20260811T102030")))
              (let ((file (chai-capture)))
                (should (file-exists-p file))
                (let ((contents (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string))))
                  (should (string-search
                           "#+BEGIN_QUOTE\nKeep this line\n#+END_QUOTE"
                           contents))
                  (should (string-search ":CHAI_SOURCE_LINE: 2" contents))
                  (should-not (string-search "Ignore" contents)))))))
      (chai-test--kill-file-buffers-under library)
      (delete-directory library t))))

(ert-deftest chai-test-generic-capture-non-file-buffer ()
  "Non-file buffers record the buffer name and mode as source."
  (let ((library (make-temp-file "chai-generic-capture" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "Ephemeral note")
          (goto-char (point-min))
          (let ((chai-library-directory library)
                (buf (buffer-name)))
            (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                      ((symbol-function 'format-time-string)
                       (lambda (&rest _) "20260811T102030")))
              (let ((file (chai-capture)))
                (should (file-exists-p file))
                (let ((contents (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string))))
                  (should (string-search
                           (format ":CHAI_SOURCE: %s" buf)
                           contents))
                  (should (string-search ":CHAI_SOURCE_MODE: org-mode" contents))
                  (should (string-search (format "%s (org-mode)" buf) contents))
                  (should-not (string-search "file:/" contents)))))))
      (chai-test--kill-file-buffers-under library)
      (delete-directory library t))))

(ert-deftest chai-test-generic-capture-rejects-empty-content ()
  "Generic capture refuses an empty selection or line."
  (let ((library (make-temp-file "chai-generic-capture" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "")
          (let ((chai-library-directory library))
            (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
              (should-error (chai-capture) :type 'user-error)))
          (should-not (directory-files library nil "\\.org\\'")))
      (chai-test--kill-file-buffers-under library)
      (delete-directory library t))))

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

(ert-deftest chai-test-library-menu-default-keybinding ()
  "Library exposes the transient menu on the default ? key."
  (should (eq (lookup-key chai-library-mode-map (kbd "?"))
              'chai-library-menu))
  (should (fboundp 'chai-library-menu)))

(ert-deftest chai-test-library-open-book-minibuffer-selection ()
  "Minibuffer selection opens the selected Chai Library book."
  (let* ((dir (make-temp-file "chai-library-" t))
         (file-a (expand-file-name
                  "20260720T120001__Author-A__Book-A.org" dir))
         (file-b (expand-file-name
                  "20260720T120002__Author-B__Book-B.org" dir))
         choices)
    (unwind-protect
        (let ((chai-library-directory dir))
          (with-temp-file file-a (insert "Book A\n"))
          (with-temp-file file-b (insert "Book B\n"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (setq choices collection)
                       (file-name-nondirectory file-b))))
            (chai-library-open-book))
          (should (= (length choices) 2))
          (should (member (file-name-nondirectory file-b) choices))
          (should (equal (file-truename (buffer-file-name))
                         (file-truename file-b))))
      (chai-test--kill-file-buffers-under dir)
      (delete-directory dir t))))

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

(ert-deftest chai-test-link-follow-finds-renamed-library-book-at-line ()
  "An ID source link finds a renamed Library file and moves to its line."
  (let* ((dir (make-temp-file "chai-library-" t))
         (id "20260810T102030")
         (old-path (expand-file-name (concat id "__Original.org") dir))
         (new-path (expand-file-name (concat id "__Renamed-Book.org") dir)))
    (unwind-protect
        (let ((chai-library-directory dir)
              (chai-library--cache-books nil)
              (chai-library--cache-mtime nil)
              (chai-library--cache-hash (make-hash-table :test 'equal)))
          (with-temp-file old-path
            (insert "* Book\n:PROPERTIES:\n:ID: " id
                    "\n:END:\n[[chai:important][highlight]]\n"))
          (rename-file old-path new-path)
          (chai-link-follow (concat id "::5"))
          (should (equal (file-truename (buffer-file-name))
                         (file-truename new-path)))
          (should (= (line-number-at-pos) 5)))
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

(ert-deftest chai-test-context-menu-shows-chai-action-on-plain-click ()
  "chai-context-menu exposes Chai even away from links or active regions."
  (chai-test--with-temp-org "plain text"
    (let* ((menu (make-sparse-keymap))
           (click-event (list 'mouse-3 (posn-at-point 1 (selected-window))))
           (result-menu (chai-context-menu menu click-event)))
      (should (commandp (lookup-key result-menu [chai-add-comment])))
      (should (commandp (lookup-key result-menu [chai-highlight-type-important])))
      (should (commandp (lookup-key result-menu [chai-highlight-region])))
      (should (commandp (lookup-key result-menu [chai-highlight-annotate]))))))

(ert-deftest chai-test-context-menu-respects-custom-highlight-types ()
  "Context menu quick highlight entries come from `chai-highlight-types'."
  (chai-test--with-temp-org "select this text"
    (let ((chai-highlight-types '(("custom" . chai-highlight-important)
                                  ("review" . chai-highlight-idea))))
      (setq-local transient-mark-mode t)
      (goto-char 1)
      (set-mark (point))
      (goto-char 12)
      (activate-mark)
      (let* ((menu (make-sparse-keymap))
             (click-event (list 'mouse-3 (posn-at-point 1 (selected-window))))
             (result-menu (chai-context-menu menu click-event))
             (cmd (lookup-key result-menu [chai-highlight-type-custom])))
        (should (commandp cmd))
        (should (commandp (lookup-key result-menu [chai-highlight-type-review])))
        (should-not (lookup-key result-menu [chai-highlight-type-important]))
        (deactivate-mark)
        (funcall cmd)
        (should (string= (buffer-string)
                         "[[chai:custom][select this]] text"))))))

(ert-deftest chai-test-context-menu-region-highlight-uses-captured-region ()
  "Context menu region highlight commands keep the selected bounds."
  (chai-test--with-temp-org "select this text"
    (setq-local transient-mark-mode t)
    (goto-char 1)
    (set-mark (point))
    (goto-char 12)
    (activate-mark)
    (let* ((menu (make-sparse-keymap))
           (click-event (list 'mouse-3 (posn-at-point 1 (selected-window))))
           (result-menu (chai-context-menu menu click-event))
           (cmd (lookup-key result-menu [chai-highlight-type-important])))
      (deactivate-mark)
      (goto-char (point-max))
      (should (commandp cmd))
      (funcall cmd)
      (should (string= (buffer-string)
                       "[[chai:important][select this]] text")))))

(ert-deftest chai-test-context-menu-region-annotate-uses-captured-region ()
  "Context menu annotate command keeps the selected bounds."
  (chai-test--with-temp-org "select this text"
    (setq-local transient-mark-mode t)
    (goto-char 1)
    (set-mark (point))
    (goto-char 12)
    (activate-mark)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "idea"))
              ((symbol-function 'read-string) (lambda (&rest _) "note")))
      (let* ((menu (make-sparse-keymap))
             (click-event (list 'mouse-3 (posn-at-point 1 (selected-window))))
             (result-menu (chai-context-menu menu click-event))
             (cmd (lookup-key result-menu [chai-highlight-annotate])))
        (deactivate-mark)
        (goto-char (point-max))
        (should (commandp cmd))
        (funcall cmd)
        (should (string= (buffer-string)
                         "[[chai:idea:note][select this]] text"))))))

(ert-deftest chai-test-context-menu-add-comment-uses-captured-region ()
  "Context menu comment command wraps the selected bounds."
  (chai-test--with-temp-org "my note"
    (setq-local transient-mark-mode t)
    (goto-char 1)
    (set-mark (point))
    (goto-char 8)
    (activate-mark)
    (let* ((menu (make-sparse-keymap))
           (click-event (list 'mouse-3 (posn-at-point 1 (selected-window))))
           (result-menu (chai-context-menu menu click-event))
           (cmd (lookup-key result-menu [chai-add-comment])))
      (deactivate-mark)
      (should (commandp cmd))
      (funcall cmd)
      (should (string= (buffer-string)
                       "#+BEGIN_CHAI_COMMENT\nmy note\n#+END_CHAI_COMMENT\n")))))

(ert-deftest chai-test-org-buffer-installs-context-menu-mouse-keys ()
  "Chai Org buffers bind right-click to the context menu locally."
  (chai-test--with-temp-org "select this text"
    (let ((down-binding (lookup-key (current-local-map) [down-mouse-3]))
          (up-binding (lookup-key (current-local-map) [mouse-3])))
      (should down-binding)
      (should-not (eq down-binding 'mouse-save-then-kill))
      (should (eq up-binding 'ignore)))))

(ert-deftest chai-test-load-installs-context-menu-in-current-org-buffer ()
  "Loading chai.el from an existing Org buffer installs the context menu there."
  (skip-unless (boundp 'context-menu-functions))
  (chai-test--with-temp-org "plain text"
    (setq-local context-menu-functions nil)
    (load (expand-file-name "chai.el" default-directory) nil t)
    (should (memq #'chai-context-menu context-menu-functions))))

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

;;; Export preview

(defun chai-test--kill-preview-buffer ()
  "Kill the preview buffer if it exists."
  (when (get-buffer "*Chai Export Preview*")
    (kill-buffer "*Chai Export Preview*")))

(defun chai-test--kill-heading-export-buffer ()
  "Kill the temporary Heading export buffer if it exists."
  (when (get-buffer "*Chai Heading Export*")
    (kill-buffer "*Chai Heading Export*")))

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

(ert-deftest chai-test-export-heading-buffer ()
  "Heading export uses the canonical plain Org entry renderer."
  (chai-test--kill-heading-export-buffer)
  (unwind-protect
      (chai-test--with-temp-org "#+TITLE: Source Title\n[[chai:important][hi]]"
        (setq-local buffer-file-name "/tmp/20260810T102030__Source.org")
        (chai-test--with-export-env
          (chai-export-heading)
          (with-current-buffer "*Chai Heading Export*"
            (should (string= (buffer-string)
                             (concat "* [IMPORTANT] hi\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[chai:20260810T102030::2][L2]]\n"
                                     ":END:\n")))
            (should (eq major-mode 'org-mode))
            (should-not buffer-file-name)
            (should (buffer-modified-p)))))
    (chai-test--kill-heading-export-buffer)))

(ert-deftest chai-test-export-heading-buffer-uses-configured-file ()
  "Heading export associates with its configured file for normal saving."
  (chai-test--kill-heading-export-buffer)
  (let* ((directory (make-temp-file "chai-heading" t))
         (file (expand-file-name "reading-note.org" directory)))
    (unwind-protect
        (let ((chai-export-heading-file file))
          (chai-test--with-temp-org "[[chai:key][hi]]"
            (setq-local buffer-file-name "/tmp/source.org")
            (chai-test--with-export-env
              (chai-export-heading)
              (with-current-buffer "*Chai Heading Export*"
                (should (string= buffer-file-name file))
                (should-not (file-exists-p file))
                (basic-save-buffer)
                (should (file-exists-p file))
                (should-not (buffer-modified-p)))
              (with-temp-buffer
                (insert-file-contents file)
                (should (string-search "* [KEY] hi" (buffer-string)))
                (should (string-search ":SOURCE:" (buffer-string)))))))
      (chai-test--kill-heading-export-buffer)
      (delete-directory directory t))))

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
                             (concat "* [IMPORTANT] hi\n"
                                     ":PROPERTIES:\n"
                                     ":SOURCE: [[file:/tmp/source.org::1][L1]]\n"
                                     ":END:\n")))
            (should (eq major-mode 'org-mode))
            (should (not buffer-read-only))
            (should (string= buffer-file-name "/tmp/preview/source_chai.org"))
            (should-not (string-search "#+SEQ_TODO:" (buffer-string)))
            (should-not (string-search "#+TITLE:" (buffer-string)))
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
                     "\\* \\[IMPORTANT\\] hi"
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
                   "\\* \\[IMPORTANT\\] hi"
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

;;; Search index

(defmacro chai-test--with-temp-library (&rest body)
  "Evaluate BODY against a throwaway Chai Library and search index.
Everything the index touches is redirected into a temporary directory, so
tests never see the user's real Library, database or scan cache."
  (declare (indent 0))
  `(let* ((chai-test--library-dir (make-temp-file "chai-search" t))
          (chai-library-directory chai-test--library-dir)
          (chai-index-file (expand-file-name "index.db" chai-test--library-dir))
          (chai-index--db nil)
          (chai-index--db-path nil)
          (chai-library--cache-hash (make-hash-table :test 'equal))
          (chai-library--cache-books nil)
          (chai-library--cache-mtime nil))
     (unwind-protect
         (progn ,@body)
       (chai-index-close)
       (chai-test--kill-file-buffers-under chai-test--library-dir)
       (delete-directory chai-test--library-dir t))))

(defun chai-test--write-book (name contents)
  "Write CONTENTS to book NAME inside the temporary Library.
NAME must already be in Chai's managed filename form so that the Library
scan does not try to rename it mid-test."
  (let ((path (expand-file-name name chai-library-directory)))
    (with-temp-file path (insert contents))
    path))

(defun chai-index-reset-for-test ()
  "Empty the test index so a rebuild has work to do."
  (let ((db (chai-index--db)))
    (sqlite-execute db "DELETE FROM fts_chunks")
    (sqlite-execute db "DELETE FROM chunks")
    (sqlite-execute db "DELETE FROM documents")))

(defun chai-test--rescan-library ()
  "Forget the Library scan cache so the next scan sees the current files."
  (setq chai-library--cache-books nil
        chai-library--cache-mtime nil)
  (clrhash chai-library--cache-hash))

(defun chai-test--hit-titles (hits)
  "Return the book titles of HITS, in order."
  (mapcar (lambda (hit) (plist-get hit :title)) hits))

(defconst chai-test--consensus-book
  "20260826T120000__Consensus==distsys_algorithms--done.org")

(defconst chai-test--cooking-book
  "20260826T120001__Cooking==food--reading.org")

(defun chai-test--build-two-book-library ()
  "Write one book about consensus and one about cooking, then index them."
  (chai-test--write-book chai-test--consensus-book
                         "* 分布式共识\n分布式共识的基本原理，以及 Raft 协议的选举过程。\n")
  (chai-test--write-book chai-test--cooking-book
                         "* 家常菜\n红烧肉的做法，先焯水再炒糖色。\n")
  (chai-index-rebuild))

(ert-deftest chai-test-search-finds-chinese-passage ()
  "A Chinese query reaches the book that discusses it and no other."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (let ((hits (chai-search-query "共识算法" 5)))
      (should (= (length hits) 1))
      (should (equal (chai-test--hit-titles hits) '("Consensus"))))))

(ert-deftest chai-test-search-tolerates-chinese-word-order ()
  "Querying a compound the source never spells out still reaches it.
The source says 分布式共识; the query says 共识算法.  Whole-run matching
would miss this, which is why the index is built from CJK bigrams."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (should (equal (chai-test--hit-titles (chai-search-query "共识算法" 5))
                   '("Consensus")))
    (should (equal (chai-test--hit-titles (chai-search-query "分布式" 5))
                   '("Consensus")))))

(ert-deftest chai-test-search-matches-mixed-language-book ()
  "A book mixing Chinese and English answers queries in either language."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (should (equal (chai-test--hit-titles (chai-search-query "Raft" 5)) '("Consensus")))
    (should (equal (chai-test--hit-titles (chai-search-query "选举" 5)) '("Consensus")))))

(ert-deftest chai-test-search-tolerates-fts5-special-characters ()
  "Punctuation in a query is literal text, not FTS5 query syntax."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (should (equal (chai-test--hit-titles (chai-search-query "共识: (算法) - raft" 5))
                   '("Consensus")))))

(ert-deftest chai-test-search-reports-a-position-inside-the-passage ()
  "A hit's position lands on the passage text it reported."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book
                           "* 分布式共识\nRaft 协议的选举过程。\n")
    (chai-index-rebuild)
    (let* ((hit (car (chai-search-query "Raft" 5)))
           (first-line (car (split-string (plist-get hit :text) "\n" t))))
      (should hit)
      (with-current-buffer (find-file-noselect (plist-get hit :file))
        (goto-char (plist-get hit :beg))
        (should (looking-at-p (regexp-quote first-line)))))))

(ert-deftest chai-test-search-filters-by-book-status ()
  "A status filter restricts the search to books in that reading state."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (should (chai-search-query "共识" 5 '(:status done)))
    (should-not (chai-search-query "共识" 5 '(:status reading)))))

(ert-deftest chai-test-search-filters-by-keyword ()
  "A keyword filter restricts the search to books carrying that keyword."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (should (chai-search-query "共识" 5 '(:keyword "distsys")))
    (should-not (chai-search-query "共识" 5 '(:keyword "food")))))

(ert-deftest chai-test-search-filters-to-annotated-passages ()
  "The annotated-only filter drops passages the reader never marked."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book
                           "* 共识\n[[chai:important][Raft 的选举过程]]很关键。\n")
    (chai-test--write-book chai-test--cooking-book
                           "* 共识\nRaft 的选举过程也出现在这本没有标注的书里。\n")
    (chai-index-rebuild)
    (should (= (length (chai-search-query "选举" 5)) 2))
    (should (equal (chai-test--hit-titles (chai-search-query "选举" 5 '(:highlighted t)))
                   '("Consensus")))))

(ert-deftest chai-test-search-filters-by-highlight-type ()
  "A type filter reaches only passages annotated with that highlight type."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book
                           "* 共识\n[[chai:important][Raft 的选举过程]]很关键。\n")
    (chai-index-rebuild)
    (should (chai-search-query "选举" 5 '(:type "important")))
    (should-not (chai-search-query "选举" 5 '(:type "question")))))

(ert-deftest chai-test-search-ranks-annotated-passages-first ()
  "Between two equally relevant passages, the annotated one wins.
The annotated passage is recalled by both channels, so fusion lifts it above
a passage that only the lexical channel returned."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--cooking-book
                           "* 共识\nRaft 的选举过程在这本书里只是顺带一提。\n")
    (chai-test--write-book chai-test--consensus-book
                           "* 共识\n[[chai:important][Raft 的选举过程]]是这本书的重点。\n")
    (chai-index-rebuild)
    (let ((hits (chai-search-query "选举" 5)))
      (should (= (length hits) 2))
      (should (equal (plist-get (car hits) :title) "Consensus"))
      (should (plist-get (plist-get (car hits) :explain) :highlight)))))

(ert-deftest chai-test-search-returns-nothing-without-a-match ()
  "Blank queries, unmatched queries and an empty Library all return no hits."
  (chai-test--with-temp-library
    (should-not (chai-search-query "共识" 5))
    (chai-test--build-two-book-library)
    (should-not (chai-search-query "" 5))
    (should-not (chai-search-query "   " 5))
    (should-not (chai-search-query "量子色动力学" 5))))

(ert-deftest chai-test-index-rebuild-is-idempotent ()
  "Re-indexing unchanged books neither duplicates nor loses passages."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (let ((before (chai-search-query "共识" 10)))
      (chai-test--rescan-library)
      (let ((result (chai-index-rebuild)))
        (should (= (plist-get result :indexed) 0))
        (should (= (plist-get result :skipped) 2)))
      (should (equal (chai-test--hit-titles (chai-search-query "共识" 10))
                     (chai-test--hit-titles before))))))

(ert-deftest chai-test-index-reindexes-changed-books ()
  "Editing a book replaces its passages instead of accumulating them."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (chai-test--write-book chai-test--consensus-book
                           "* 分布式共识\n这一版改成了 Paxos 的两阶段提交。\n")
    (chai-test--rescan-library)
    (chai-index-rebuild)
    (should (chai-search-query "Paxos" 5))
    (should-not (chai-search-query "Raft" 5))))

(ert-deftest chai-test-index-drops-deleted-books ()
  "Passages of a deleted book disappear from the results."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (should (chai-search-query "共识" 5))
    (delete-file (expand-file-name chai-test--consensus-book chai-library-directory))
    (chai-test--rescan-library)
    (chai-index-rebuild)
    (should-not (chai-search-query "共识" 5))
    (should (chai-search-query "红烧肉" 5))))

(ert-deftest chai-test-index-does-not-multiply-passages-without-blank-lines ()
  "A book whose paragraphs are plain lines indexes in proportion to its size.
Books converted from PDF or EPUB often contain no blank line at all.  A scan
that treats \"no blank line ahead\" as ordinary progress restarts one character
later and emits another passage reaching to the end of the section, producing
one passage per character and an index orders of magnitude larger than the
source text."
  (chai-test--with-temp-library
    (let ((body (mapconcat (lambda (i) (format "第 %d 行，分布式共识算法的正文内容。" i))
                           (number-sequence 1 400)
                           "\n")))
      (chai-test--write-book chai-test--consensus-book (concat "* 共识\n" body "\n"))
      (chai-index-rebuild)
      (let ((chunks (plist-get (chai-index-status) :chunks)))
        (should (> chunks 0))
        ;; ~800 characters per chunk over ~8000 characters of text: a few dozen.
        ;; The quadratic scan produced one per character instead.
        (should (< chunks 60)))
      (should (chai-search-query "共识算法" 5)))))

(ert-deftest chai-test-index-handles-a-book-with-no-headline ()
  "A book that is one unbroken block of text still indexes and is searchable."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book
                           (mapconcat (lambda (i) (format "第 %d 行，Raft 协议的选举过程。" i))
                                      (number-sequence 1 200)
                                      "\n"))
    (chai-index-rebuild)
    (let ((chunks (plist-get (chai-index-status) :chunks)))
      (should (> chunks 0))
      (should (< chunks 40)))
    (should (chai-search-query "选举" 5))))

(ert-deftest chai-test-index-passages-do-not-overlap ()
  "Passages of one book cover distinct, non-overlapping regions of it."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book
                           (concat "* 共识\n"
                                   (mapconcat (lambda (i) (format "第 %d 段，分布式共识与 Raft 选举。" i))
                                              (number-sequence 1 120)
                                              "\n")
                                   "\n"))
    (chai-index-rebuild)
    (let* ((hits (chai-search-query "共识" 200))
           (sorted (sort (copy-sequence hits)
                         (lambda (a b) (< (plist-get a :beg) (plist-get b :beg))))))
      (should (> (length sorted) 1))
      (cl-loop for (this next) on sorted
               while next
               do (should (<= (plist-get this :end) (plist-get next :beg)))))))

(ert-deftest chai-test-index-runs-the-rebuild-in-a-separate-emacs ()
  "An interactive rebuild hands the work to a batch Emacs and watches it.
The point of the separate process is that this session keeps its own database
connection closed and does no indexing work itself."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (chai-index-reset-for-test)
    (unwind-protect
        (progn
          (call-interactively 'chai-index-rebuild)
          (should (process-live-p chai-index--process))
          (let ((waited 0))
            (while (and (process-live-p chai-index--process) (< waited 60))
              (accept-process-output chai-index--process 0.2)
              (setq waited (+ waited 0.2))))
          (accept-process-output nil 0.5)
          (should-not chai-index--process)
          (should (= (plist-get (chai-index-status) :documents) 2))
          (should (chai-search-query "共识" 5)))
      (when (process-live-p chai-index--process)
        (delete-process chai-index--process)))))

(ert-deftest chai-test-index-batch-entry-point-indexes-and-reports ()
  "The batch entry point indexes a Library and prints progress then a result.
This is what the separate Emacs actually runs, so it carries the real work."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (chai-index-reset-for-test)
    (chai-index-close)
    (let* ((output "")
           (settings (list :library chai-library-directory :index chai-index-file)))
      (cl-letf (((symbol-function 'external-debugging-output)
                 (lambda (char) (setq output (concat output (string char))))))
        (chai-index-batch-run settings))
      (should (string-match-p "CHAI-PROGRESS 0 2 0" output))
      (should (string-match-p "CHAI-PROGRESS 2 2 " output))
      (should (string-match-p "CHAI-RESULT " output))
      (let ((result (car (read-from-string
                          (substring output (+ (string-match "CHAI-RESULT " output)
                                               (length "CHAI-RESULT ")))))))
        (should (= (plist-get result :indexed) 2))
        (should (> (plist-get result :chunks) 0))))))

(ert-deftest chai-test-index-turns-process-output-into-progress ()
  "Progress lines arriving in fragments still produce whole-line updates."
  (let ((chai-index--partial "")
        (chai-index--progress nil)
        (seen nil))
    (cl-letf (((symbol-function 'chai-index--report)
               (lambda (result) (push result seen))))
      ;; A line split across two reads must not be acted on twice or early.
      (chai-index--filter nil "CHAI-PROGRESS 3 10 4")
      (should-not chai-index--progress)
      (chai-index--filter nil "2
CHAI-PROG")
      (should (equal chai-index--progress " Chai indexing 3/10 (42)"))
      (chai-index--filter nil "RESS 10 10 90
CHAI-RESULT (:indexed 10 :skipped 0 :chunks 90 :seconds 1.0)
")
      (should (equal chai-index--progress " Chai indexing 10/10 (90)"))
      (should (= (plist-get (car seen) :indexed) 10)))))

(ert-deftest chai-test-index-resumes-after-being-stopped ()
  "Books indexed before a stop are kept, and the next run finishes the rest."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (chai-index-reset-for-test)
    ;; Index one book, as an interrupted rebuild would have.
    (let ((db (chai-index--db))
          (book (car (chai-library-scan))))
      (with-sqlite-transaction db
        (chai-index--index-book db book (chai-index--file-hash (chai-book-file-path book)))))
    (should (= (plist-get (chai-index-status) :documents) 1))
    (let ((result (chai-index-rebuild)))
      (should (= (plist-get result :indexed) 1))
      (should (= (plist-get result :skipped) 1)))
    (should (= (plist-get (chai-index-status) :documents) 2))))

(ert-deftest chai-test-index-refuses-to-run-two-rebuilds-at-once ()
  "Starting a rebuild while one is running is refused rather than interleaved."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (unwind-protect
        (progn
          (call-interactively 'chai-index-rebuild)
          (should-error (call-interactively 'chai-index-rebuild) :type 'user-error))
      (when (process-live-p chai-index--process)
        (delete-process chai-index--process))
      (accept-process-output nil 0.3))))

(ert-deftest chai-test-search-reports-the-full-headline-path ()
  "A hit names every heading enclosing it, not just the nearest one."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book
                           (concat "* 第五章 分布式系统\n"
                                   "** 第三节 共识协议\n"
                                   "*** Raft\n"
                                   "Raft 通过选举产生 leader。\n"))
    (chai-index-rebuild)
    (let ((hit (car (chai-search-query "选举" 5))))
      (should hit)
      (should (equal (plist-get hit :headline) "Raft"))
      (should (equal (plist-get hit :outline)
                     "第五章 分布式系统 › 第三节 共识协议 › Raft")))))

(ert-deftest chai-test-search-closes-headline-path-at-a-sibling ()
  "Moving to a sibling heading drops the previous branch from the path."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book
                           (concat "* 第一章\n"
                                   "** 甲节\n"
                                   "关于 Paxos 的两阶段提交。\n"
                                   "** 乙节\n"
                                   "关于 Raft 的选举过程。\n"))
    (chai-index-rebuild)
    (should (equal (plist-get (car (chai-search-query "两阶段" 5)) :outline)
                   "第一章 › 甲节"))
    (should (equal (plist-get (car (chai-search-query "选举" 5)) :outline)
                   "第一章 › 乙节"))))

(ert-deftest chai-test-search-leaves-the-path-empty-before-any-heading ()
  "Text ahead of the first heading has no enclosing path."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book
                           "开篇的一段话，讲 Raft 的选举过程。\n\n* 第一章\n正文。\n")
    (chai-index-rebuild)
    (let ((hit (car (chai-search-query "选举" 5))))
      (should hit)
      (should-not (plist-get hit :outline))
      (should-not (plist-get hit :headline)))))

(ert-deftest chai-test-search-does-not-index-ancestor-headings ()
  "A chapter title matches its own heading, not every passage beneath it.
Indexing the whole path would make a broad query return an entire chapter,
trading away precision exactly where it matters most."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book
                           (concat "* 经济学原理\n"
                                   "** 供给\n"
                                   "生产者愿意出售的数量随价格上升而增加。\n"
                                   "** 需求\n"
                                   "消费者愿意购买的数量随价格上升而减少。\n"
                                   "** 弹性\n"
                                   "衡量数量对价格变化的敏感程度。\n"))
    (chai-index-rebuild)
    ;; Three sections sit under the chapter; the chapter title must not drag
    ;; all three into the results.
    (should (< (length (chai-search-query "经济学" 10)) 3))))

(ert-deftest chai-test-search-cleans-up-headline-titles ()
  "Tags, Org line-break markers and blank levels stay out of the path.
The blank level here is padded with the ideographic space U+3000, which is
what Chinese ebook conversions produce and what `string-blank-p' misses."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book
                           (concat "* 第10章\\\\    :draft:noexport:\n"
                                   "** \u3000\u3000\n"
                                   "*** \u3000共识\u3000\n"
                                   "Raft 的选举过程。\n"))
    (chai-index-rebuild)
    (let ((hit (car (chai-search-query "选举" 5))))
      (should hit)
      (should (equal (plist-get hit :outline) "第10章 › 共识")))))

(ert-deftest chai-test-index-keeps-the-write-ahead-log-bounded ()
  "Finishing a rebuild folds the write-ahead log back into the database.
Left alone across a large rebuild the log outgrows the database itself."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (let ((wal (concat chai-index-file "-wal")))
      (should (file-exists-p chai-index-file))
      ;; A finished rebuild truncates the log rather than leaving it behind.
      (should (or (not (file-exists-p wal))
                  (< (file-attribute-size (file-attributes wal)) 4096))))))

(ert-deftest chai-test-search-stores-a-preview-and-reads-the-rest-on-demand ()
  "The index keeps a bounded preview; the full passage comes from the file."
  (chai-test--with-temp-library
    (let ((body (mapconcat (lambda (i) (format "第 %d 句，讲的是分布式共识算法。" i))
                           (number-sequence 1 40) "")))
      (chai-test--write-book chai-test--consensus-book (concat "* 共识\n" body "\n"))
      (let ((chai-index-preview-chars 40))
        (chai-index-rebuild))
      (let ((hit (car (chai-search-query "共识算法" 5))))
        (should hit)
        ;; the stored preview is bounded, plus the ellipsis marking the cut
        (should (<= (length (plist-get hit :text)) 41))
        ;; the whole passage is still reachable, and is longer
        (let ((full (chai-search-passage-text hit)))
          (should (> (length full) (length (plist-get hit :text))))
          (should (string-prefix-p (substring (plist-get hit :text) 0 20) full)))))))

(ert-deftest chai-test-search-reads-the-passage-as-the-file-now-reads ()
  "The full passage reflects the file's current content, not the indexed copy."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book "* 共识\nRaft 的选举过程。\n")
    (chai-index-rebuild)
    (let ((hit (car (chai-search-query "选举" 5))))
      (should (string-match-p "Raft" (chai-search-passage-text hit)))
      (chai-test--write-book chai-test--consensus-book "* 共识\nPaxos 的选举过程。\n")
      ;; without re-indexing, the passage text follows the file
      (should (string-match-p "Paxos" (chai-search-passage-text hit))))))

(ert-deftest chai-test-index-reports-progress-in-the-echo-area ()
  "Progress reaches the echo area, not only the mode line.
Many configurations replace the mode line without rendering
`global-mode-string', and then the echo area is the only place a rebuild is
visible at all."
  (let ((chai-index--partial "")
        (chai-index--progress nil)
        (chai-index--last-echo 0)
        (chai-index--noise nil)
        (chai-index-echo-interval 0)
        (echoed nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) echoed))))
      (chai-index--filter nil "CHAI-PROGRESS 7 90 300\n"))
    (should (equal chai-index--progress " Chai indexing 7/90 (300)"))
    (should (= (length echoed) 1))
    (should (string-match-p "7/90" (car echoed)))))

(ert-deftest chai-test-index-rate-limits-progress-messages ()
  "Progress messages are throttled so a rebuild cannot flood the echo area."
  (let ((chai-index--partial "")
        (chai-index--progress nil)
        (chai-index--last-echo 0)
        (chai-index--noise nil)
        (chai-index-echo-interval 3600)
        (echoed 0))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) (setq echoed (1+ echoed)))))
      (chai-index--filter nil "CHAI-PROGRESS 1 90 10\nCHAI-PROGRESS 2 90 20\nCHAI-PROGRESS 3 90 30\n"))
    ;; The mode line still tracks every update; only the echo area is throttled.
    (should (equal chai-index--progress " Chai indexing 3/90 (30)"))
    (should (= echoed 1))))

(ert-deftest chai-test-index-keeps-unexpected-child-output ()
  "Output that is not progress is kept so a failed rebuild can explain itself."
  (let ((chai-index--partial "")
        (chai-index--progress nil)
        (chai-index--noise nil))
    (chai-index--filter nil "Symbol's function definition is void: nonesuch\n")
    (should (equal chai-index--noise '("Symbol's function definition is void: nonesuch")))))

;;; Asking

(ert-deftest chai-test-ask-estimates-tokens-conservatively ()
  "Token estimates count CJK by character and western text by word-ish runs."
  (should (= (chai-estimate-tokens "共识算法") 4))
  (should (= (chai-estimate-tokens "") 0))
  ;; Western text costs less per character, but never zero.
  (should (< (chai-estimate-tokens "consensus algorithm")
             (chai-estimate-tokens "共识算法共识算法共识算法共识算法")))
  (should (> (chai-estimate-tokens "consensus") 0)))

(ert-deftest chai-test-ask-keeps-only-citations-that-exist ()
  "Citation markers are validated against the passages actually supplied.
A marker outside that range names a source the model was never given."
  (should (equal (chai-context-citations "答案是 [1]，因为 [2] 这样说。" 2) '(1 2)))
  (should (equal (chai-context-citations "见 [1] 和 [5]。" 2) '(1)))
  (should (equal (chai-context-citations "如 [1] 所述，再看 [1]。" 1) '(1)))
  (should (equal (chai-context-citations "没有引用。" 3) nil))
  (should (equal (chai-context-citations "[0] 不算" 3) nil))
  ;; Order follows first appearance, not numeric order.
  (should (equal (chai-context-citations "先 [3] 再 [1]" 3) '(3 1))))

(ert-deftest chai-test-ask-stops-adding-passages-at-the-budget ()
  "Passages are added while they fit and the surplus is dropped from the end."
  (let* ((long (make-string 400 ?共))
         (hits (mapcar (lambda (i)
                         (list :title (format "书%d" i) :file "/tmp/x.org"
                               :beg 1 :end 2 :text long))
                       (number-sequence 1 6))))
    (cl-letf (((symbol-function 'chai-search-passage-text) (lambda (hit) (plist-get hit :text))))
      ;; Each passage costs about 400 tokens, so a 1000-token budget takes two.
      (let ((chai-context-tokens 1000)
            (chai-context-passages 6))
        (should (= (length (chai-context-select hits)) 2)))
      ;; The first passage is kept even when it alone exceeds the budget.
      (let ((chai-context-tokens 10)
            (chai-context-passages 6))
        (should (= (length (chai-context-select hits)) 1)))
      ;; Passages are numbered from one, in rank order.
      (let ((chai-context-tokens 100000)
            (chai-context-passages 3))
        (let ((selected (chai-context-select hits)))
          (should (equal (mapcar #'car selected) '(1 2 3)))
          (should (equal (plist-get (nth 1 (car selected)) :title) "书1")))))))

(ert-deftest chai-test-ask-refuses-to-answer-without-material ()
  "With nothing retrieved, no request is made at all."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (let ((called nil))
      (cl-letf (((symbol-function 'chai-ask--start)
                 (lambda (&rest _) (setq called t))))
        (chai-ask "量子色动力学的重整化")
        (should-not called)
        (chai-ask "共识")
        (should called)))))

(ert-deftest chai-test-ask-request-turns-off-model-reasoning ()
  "The request asks the model not to reason at length before answering."
  (let ((chai-ask-think nil))
    (should (string-match-p "\"think\":false" (chai-ask--request-body "问题"))))
  (let ((chai-ask-think t))
    (should (string-match-p "\"think\":true" (chai-ask--request-body "问题")))))

;;; Semantic recall

(ert-deftest chai-test-vector-formats-vectors-for-sqlite ()
  "Vectors reach sqlite-vec as the JSON array it accepts."
  (should (equal (chai-vector--json '(1.0 -0.5 0.25)) "[1,-0.5,0.25]"))
  (should (equal (chai-vector--json '()) "[]")))

(ert-deftest chai-test-vector-batches-by-token-budget ()
  "Requests are filled by estimated size, not by a count of sections.
The server applies its context window to a whole request, so a batch sized by
count is refused outright once the sections are long."
  (let ((chai-vector-request-tokens 1000)
        (texts (make-hash-table :test 'eql)))
    (puthash 1 (make-string 600 ?共) texts)
    (puthash 2 (make-string 600 ?识) texts)
    (puthash 3 (make-string 100 ?算) texts)
    (cl-letf (((symbol-function 'chai-vector--section-text)
               (lambda (_db id) (gethash id texts))))
      (let ((batches (chai-vector--batches nil '(1 2 3))))
        ;; 600 + 600 exceeds the budget, so the second section opens a batch.
        (should (equal (mapcar #'car batches) '((1) (2 3))))))))

(ert-deftest chai-test-vector-batches-keep-an-oversized-section ()
  "A section too large for any batch is still sent, alone."
  (let ((chai-vector-request-tokens 10))
    (cl-letf (((symbol-function 'chai-vector--section-text)
               (lambda (_db _id) (make-string 500 ?共))))
      (should (equal (mapcar #'car (chai-vector--batches nil '(1 2))) '((1) (2)))))))

(ert-deftest chai-test-search-is-unchanged-without-semantic-recall ()
  "Search behaves exactly as before when the semantic channel is unavailable.
An absent channel contributes nothing to the fusion, so nothing needs a branch."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (let ((baseline (chai-test--hit-titles (chai-search-query "共识" 5))))
      (cl-letf (((symbol-function 'chai-vector-available-p) (lambda (&rest _) nil))
                ((symbol-function 'chai-vector-recall-chunks) (lambda (&rest _) nil)))
        (let ((chai-search-semantic t))
          (should (equal (chai-test--hit-titles (chai-search-query "共识" 5)) baseline)))))))

(ert-deftest chai-test-search-lets-semantic-recall-add-passages ()
  "The semantic channel can raise a passage the lexical query never matched."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book "* 共识\nRaft 的选举过程。\n")
    (chai-test--write-book chai-test--cooking-book "* 烹饪\n红烧肉先焯水再炒糖色。\n")
    (chai-index-rebuild)
    ;; The cooking passage shares no wording with the query at all.
    (should (equal (chai-test--hit-titles (chai-search-query "选举" 5)) '("Consensus")))
    (let* ((db (chai-index--db))
           (cooking (caar (sqlite-select db "SELECT c.id FROM chunks c JOIN documents d ON d.id = c.doc
                                             WHERE d.title = 'Cooking'"))))
      (cl-letf (((symbol-function 'chai-vector-recall-chunks) (lambda (&rest _) (list cooking))))
        (let ((chai-search-semantic t))
          (should (member "Cooking" (chai-test--hit-titles (chai-search-query "选举" 5)))))))))

(ert-deftest chai-test-vector-recall-is-silent-when-unavailable ()
  "With no extension configured, semantic recall simply returns nothing."
  (let ((chai-vector-extension "/nonexistent/vec0.dylib")
        (chai-vector--extension-cache 'unset))
    (should-not (chai-vector-extension-path))))

;;; Handing passages to a chat client

(ert-deftest chai-test-context-renders-numbered-material ()
  "Passages reach a model numbered and named, in rank order."
  (let ((passages (list (list 1 '(:title "共识" :outline "第五章 › 选举") "正文甲")
                        (list 2 '(:title "烹饪" :file "/tmp/x.org") "正文乙"))))
    (let ((rendered (chai-context-render passages)))
      (should (string-match-p "\\[1\\] 《共识》 › 第五章 › 选举\n正文甲" rendered))
      (should (string-match-p "\\[2\\] 《烹饪》\n正文乙" rendered))
      ;; The first passage is presented first: rank order is the contract.
      (should (< (string-match "\\[1\\]" rendered) (string-match "\\[2\\]" rendered))))))

(ert-deftest chai-test-context-names-a-source-without-an-outline ()
  "A passage above the first heading still names its book."
  (should (equal (chai-context-source-name '(:title "共识")) "《共识》"))
  (should (equal (chai-context-source-name '(:file "/tmp/某书.org")) "《某书》")))

(ert-deftest chai-test-context-resolves-a-citation-to-its-passage ()
  "A citation number maps back to the passage it was given for."
  (let ((passages (list (list 1 '(:title "甲") "x") (list 2 '(:title "乙") "y"))))
    (should (equal (plist-get (chai-context-hit passages 2) :title) "乙"))
    (should (equal (mapcar (lambda (s) (nth 0 s))
                           (chai-context-sources passages "见 [2] 与 [7]。"))
                   '(2)))))

(ert-deftest chai-test-superchat-attaches-passages-to-a-turn ()
  "A superchat turn gains numbered passages and keeps its own question."
  (let ((chai-superchat--passages nil)
        (chai-superchat-scope 'always)
        (turn (record 'superchat-turn)))
    (cl-letf (((symbol-function 'superchat-turn-clean-input) (lambda (_) "共识算法"))
              ((symbol-function 'chai-superchat--prompt) (lambda (_) "共识算法"))
              ((symbol-function 'chai-superchat--set-prompt)
               (lambda (_turn value) (setq turn value)))
              ((symbol-function 'chai-context-for)
               (lambda (&rest _) (list (list 1 '(:title "共识") "Raft 的选举过程。")))))
      (chai-superchat-attach nil)
      (should (string-match-p "\\[1\\] 《共识》" turn))
      (should (string-suffix-p "共识算法" turn))
      (should (= (length chai-superchat--passages) 1)))))

(ert-deftest chai-test-superchat-leaves-an-unrelated-turn-alone ()
  "A turn the Library cannot answer reaches the model exactly as written."
  (let ((chai-superchat--passages '(stale))
        (chai-superchat-scope 'always)
        (set nil))
    (cl-letf (((symbol-function 'superchat-turn-clean-input) (lambda (_) "为什么"))
              ((symbol-function 'chai-superchat--set-prompt) (lambda (&rest _) (setq set t)))
              ((symbol-function 'chai-context-for) (lambda (&rest _) nil)))
      (chai-superchat-attach nil)
      (should-not set)
      ;; Passages from an earlier turn must not linger into this one.
      (should-not chai-superchat--passages))))

(ert-deftest chai-test-superchat-skips-very-short-turns ()
  "Asides are not worth a Library search."
  (let ((chai-superchat--passages nil)
        (chai-superchat-scope 'always)
        (searched nil))
    (cl-letf (((symbol-function 'superchat-turn-clean-input) (lambda (_) "好"))
              ((symbol-function 'chai-context-for) (lambda (&rest _) (setq searched t) nil)))
      (chai-superchat-attach nil)
      (should-not searched))))

(ert-deftest chai-test-superchat-mode-registers-and-removes-its-hooks ()
  "Turning the integration off leaves superchat's pipeline as it was.
The hooks are saved and restored rather than let-bound: `add-hook' writes
through to the default value, which a dynamic binding would not capture."
  (let ((build (default-value 'superchat-build-prompt-functions))
        (post (default-value 'superchat-post-turn-functions))
        (was chai-superchat-mode))
    (unwind-protect
        (progn
          (chai-superchat-mode 1)
          (should (memq 'chai-superchat-attach superchat-build-prompt-functions))
          (should (memq 'chai-superchat-linkify superchat-post-turn-functions))
          (chai-superchat-mode -1)
          (should-not (memq 'chai-superchat-attach superchat-build-prompt-functions))
          (should-not (memq 'chai-superchat-linkify superchat-post-turn-functions)))
      (setq-default superchat-build-prompt-functions build
                    superchat-post-turn-functions post)
      (setq chai-superchat-mode was))))

(ert-deftest chai-test-superchat-reads-only-a-marked-conversation ()
  "The Library is consulted for a marked conversation and no other.
superchat is a general chat client; attaching chapters of a book to a question
about something else helps nobody."
  (let ((chai-superchat--sessions (make-hash-table :test 'equal))
        (chai-superchat-scope 'session)
        (chai-superchat--passages nil)
        (searched nil))
    (puthash "reading" t chai-superchat--sessions)
    (cl-letf (((symbol-function 'superchat-turn-clean-input) (lambda (_) "共识算法是什么"))
              ((symbol-function 'chai-superchat--set-prompt) (lambda (&rest _)))
              ((symbol-function 'chai-context-for) (lambda (&rest _) (setq searched t) nil)))
      (cl-letf (((symbol-function 'chai-superchat--slot) (lambda (_turn _slot) "other")))
        (chai-superchat-attach nil)
        (should-not searched))
      (cl-letf (((symbol-function 'chai-superchat--slot) (lambda (_turn _slot) "reading")))
        (chai-superchat-attach nil)
        (should searched)))))

(ert-deftest chai-test-superchat-always-scope-reads-every-conversation ()
  "Setting the scope to `always' drops the per-conversation gate."
  (let ((chai-superchat--sessions (make-hash-table :test 'equal))
        (chai-superchat-scope 'always)
        (chai-superchat--passages nil)
        (searched nil))
    (cl-letf (((symbol-function 'superchat-turn-clean-input) (lambda (_) "共识算法是什么"))
              ((symbol-function 'chai-superchat--slot) (lambda (_turn _slot) "unmarked"))
              ((symbol-function 'chai-superchat--set-prompt) (lambda (&rest _)))
              ((symbol-function 'chai-context-for) (lambda (&rest _) (setq searched t) nil)))
      (chai-superchat-attach nil)
      (should searched))))

(ert-deftest chai-test-superchat-cowork-needs-a-conversation ()
  "Marking a conversation outside superchat says so instead of doing nothing."
  (let ((superchat--session-id nil))
    (should-error (chai-superchat-cowork) :type 'user-error)))

(ert-deftest chai-test-superchat-command-answers-one-question ()
  "`/chai QUESTION' hands superchat a request carrying the passages."
  (let ((chai-superchat--passages nil))
    (cl-letf (((symbol-function 'chai-context-for)
               (lambda (&rest _) (list (list 1 '(:title "共识") "Raft 的选举过程。")))))
      (let ((result (chai-superchat-command "chai" "共识算法是什么" nil nil nil)))
        (should (eq (plist-get result :type) :llm-query))
        (should (string-match-p "\\[1\\] 《共识》" (plist-get result :prompt)))
        ;; The question is what the conversation shows; the passages are not.
        (should (equal (plist-get result :user-message) "共识算法是什么"))
        (should (string-suffix-p "共识算法是什么" (plist-get result :prompt)))
        (should (= (length chai-superchat--passages) 1))))))

(ert-deftest chai-test-superchat-command-says-when-nothing-was-found ()
  "With no passages the command reports it rather than asking anyway.
Sending the question on without material would only fetch the model's own
recollection, which is what a Library answer must never be."
  (let ((chai-superchat--passages '(stale)))
    (cl-letf (((symbol-function 'chai-context-for) (lambda (&rest _) nil)))
      (let ((result (chai-superchat-command "chai" "量子色动力学" nil nil nil)))
        (should (eq (plist-get result :type) :echo))
        (should-not chai-superchat--passages)))))

(ert-deftest chai-test-superchat-command-ignores-other-commands ()
  "Another command passes through to whoever handles it."
  (should-not (chai-superchat-command "recall" "foo" nil nil nil))
  ;; An empty question explains itself instead of searching for nothing.
  (should (eq (plist-get (chai-superchat-command "chai" "" "" nil nil) :type) :echo)))

(ert-deftest chai-test-superchat-command-falls-back-to-the-whole-line ()
  "The question may arrive as the args or as the whole input line."
  (cl-letf (((symbol-function 'chai-context-for)
             (lambda (query) (list (list 1 (list :title query) "x")))))
    (let ((result (chai-superchat-command "chai" "" "共识算法" nil nil)))
      (should (string-match-p "《共识算法》" (plist-get result :prompt))))))

;;; Keeping the index up to date

(ert-deftest chai-test-index-recognises-a-book-by-its-path ()
  "A Library path resolves to a book without scanning the whole Library."
  (chai-test--with-temp-library
    (let ((path (chai-test--write-book chai-test--consensus-book "* 共识\n正文。\n")))
      (should (equal (chai-book-title (chai-index-book-at path)) "Consensus"))
      (should-not (chai-index-book-at "/tmp/not-in-the-library.org"))
      (should-not (chai-index-book-at (expand-file-name "missing.org" chai-library-directory))))))

(ert-deftest chai-test-index-auto-mode-reindexes-a-saved-book ()
  "A saved book is re-indexed once Emacs falls idle, not while saving."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (should (chai-search-query "共识" 5))
    ;; Edit a book behind the index's back, as saving from a buffer would.
    (chai-test--write-book chai-test--consensus-book "* 共识\n这一版改成了 Paxos。\n")
    (should-not (chai-search-query "Paxos" 5))
    (let ((chai-index--dirty (list (expand-file-name chai-test--consensus-book
                                                     chai-library-directory))))
      (chai-index--catch-up)
      (should (chai-search-query "Paxos" 5))
      ;; The queue is emptied, so an idle Emacs does no further work.
      (should-not chai-index--dirty))))

(ert-deftest chai-test-index-auto-mode-ignores-files-outside-the-library ()
  "Saving any other file records nothing."
  (chai-test--with-temp-library
    (let ((chai-index--dirty nil))
      (with-temp-buffer
        (cl-letf (((symbol-function 'buffer-file-name) (lambda (&rest _) "/tmp/elsewhere.org")))
          (chai-index--note-save)))
      (should-not chai-index--dirty))))

(ert-deftest chai-test-index-auto-mode-survives-a-book-deleted-after-saving ()
  "A book removed between the save and the catch-up is simply skipped."
  (chai-test--with-temp-library
    (chai-test--build-two-book-library)
    (let ((path (expand-file-name chai-test--consensus-book chai-library-directory)))
      (delete-file path)
      (let ((chai-index--dirty (list path)))
        (chai-index--catch-up)
        (should-not chai-index--dirty)))))

(ert-deftest chai-test-search-can-be-limited-to-one-book ()
  "A file filter restricts the search to a single book."
  (chai-test--with-temp-library
    (chai-test--write-book chai-test--consensus-book "* 共识\nRaft 的选举过程。\n")
    (chai-test--write-book chai-test--cooking-book "* 烹饪\n关于选举的一句闲话。\n")
    (chai-index-rebuild)
    (should (= (length (chai-search-query "选举" 10)) 2))
    (let ((only (chai-search-query "选举" 10
                                   (list :file (expand-file-name chai-test--cooking-book
                                                                 chai-library-directory)))))
      (should (equal (chai-test--hit-titles only) '("Cooking"))))))

(ert-deftest chai-test-superchat-integration-is-wired-from-the-core ()
  "Loading Chai arranges for the superchat integration to load with superchat.

The registration cannot ride on an autoload cookie — Chai is installed by
adding a directory to `load-path', where cookies never fire — and putting the
`with-eval-after-load' inside `chai-superchat.el' would make it dead code,
since nothing loads that file until one of its commands runs.  So `chai.el'
must carry it."
  (should (assq 'superchat after-load-alist))
  ;; And the command it registers is reachable once that has happened.
  (should (fboundp 'chai-superchat-command))
  (should (commandp 'chai-superchat-cowork)))

(ert-deftest chai-test-superchat-command-is-registered-by-the-mode ()
  "Turning the mode on is what puts `/chai' in superchat's command chain."
  (let ((build (default-value 'superchat-build-prompt-functions))
        (post (default-value 'superchat-post-turn-functions))
        (commands (default-value 'superchat-command-hooks))
        (was chai-superchat-mode))
    (unwind-protect
        (progn
          (chai-superchat-mode -1)
          (should-not (memq 'chai-superchat-command superchat-command-hooks))
          (chai-superchat-mode 1)
          (should (memq 'chai-superchat-command superchat-command-hooks)))
      (setq-default superchat-build-prompt-functions build
                    superchat-post-turn-functions post
                    superchat-command-hooks commands)
      (setq chai-superchat-mode was))))

(defun chai-test--export-headlines (output)
  "Return the headline lines of Org OUTPUT."
  (cl-remove-if-not (lambda (line) (string-match-p "^\\*+ " line))
                    (split-string output "\n")))

(ert-deftest chai-test-export-outline-nested-and-siblings ()
  "Export only relevant ancestors, preserving levels and source order."
  (chai-test--with-temp-org
      (concat "[[chai:key][Preface note]]\n"
              "* Chapter\n[[chai:key][Chapter note]]\n"
              "** Section\n[[chai:idea:annotation][First]]\n"
              "#+BEGIN_CHAI_COMMENT\nComment\n#+END_CHAI_COMMENT\n"
              "** Empty\nUnmarked text\n"
              "** Section\n#+BEGIN_CHAI :type key\nBlock\n#+END_CHAI\n"
              "* Empty chapter\n")
    (let ((out (chai--export-items-as-org (chai--collect-items) "/tmp/source.org")))
      (should (equal (chai-test--export-headlines out)
                     '("* [KEY] Preface note" "* Chapter" "** [KEY] Chapter note"
                       "** Section" "*** [IDEA] First" "*** [COMMENT] Comment"
                       "** Section" "*** [KEY] Block")))
      (should (string-search "annotation" out))
      (should (string-search "[[file:/tmp/source.org::5][L5]]" out)))))

(ert-deftest chai-test-export-outline-narrowed-keeps-ancestors ()
  "A note-only restriction still exports its full source path and line."
  (chai-test--with-temp-org
      "* Chapter\n** Section\n[[chai:key][Selected]]\n** Other\n[[chai:key][Excluded]]\n"
    (forward-line 2)
    (let ((start (point))
          (before (buffer-string)))
      (forward-line 1)
      (narrow-to-region start (point))
      (let ((out (chai--export-items-as-org (chai--collect-items) "/tmp/source.org")))
        (should (equal (chai-test--export-headlines out)
                       '("* Chapter" "** Section" "*** [KEY] Selected")))
        (should (string-search "[[file:/tmp/source.org::3][L3]]" out))
        (should (= (point-min) start)))
      (widen)
      (should (equal before (buffer-string))))))

(ert-deftest chai-test-export-outline-subtree-and-region ()
  "Scope selects notes, while their ancestors remain available."
  (dolist (scope '(subtree region))
    (chai-test--with-temp-org
        "* Chapter\n** Section\n[[chai:key][Selected]]\n** Other\n[[chai:key][Excluded]]\n"
      (forward-line 1)
      (when (eq scope 'region)
        (forward-line 1)
        (set-mark (point))
        (forward-line 1)
        (setq mark-active t transient-mark-mode t))
      (let ((out (chai--export-items-as-org
                  (chai--collect-items-in-scope scope))))
        (should (equal (chai-test--export-headlines out)
                       '("* Chapter" "** Section" "*** [KEY] Selected")))))))

(ert-deftest chai-test-export-outline-skipped-levels-and-same-titles ()
  "Do not merge different chapters by title or renumber skipped levels."
  (chai-test--with-temp-org
      "* Same\n*** Deep\n[[chai:key][One]]\n* Same\n[[chai:key][Two]]\n"
    (should (equal (chai-test--export-headlines
                    (chai--export-items-as-org (chai--collect-items)))
                   '("* Same" "*** Deep" "**** [KEY] One"
                     "* Same" "** [KEY] Two")))))

(ert-deftest chai-test-export-outline-empty-comment-has-no-ancestors ()
  "Empty comments must not introduce otherwise unused headings."
  (chai-test--with-temp-org
      "* Empty\n#+BEGIN_CHAI_COMMENT\n\n#+END_CHAI_COMMENT\n"
    (should (equal "" (chai--export-items-as-org (chai--collect-items))))))

(ert-deftest chai-test-export-outline-preview-copy-and-save-agree ()
  "Preview, Org copy and direct save share the same chapter structure."
  (let ((chai-export-preview-directory (make-temp-file "chai-outline" t))
        (kill-ring nil))
    (chai-test--kill-preview-buffer)
    (unwind-protect
        (chai-test--with-temp-org "* Chapter\n** Section\n[[chai:key][Note]]\n"
          (setq-local buffer-file-name "/tmp/source.org")
          (chai-export-highlights-copy-org)
          (let ((expected (current-kill 0)))
            (should (equal (chai-test--export-headlines expected)
                           '("* Chapter" "** Section" "*** [KEY] Note")))
            (save-window-excursion
              (chai-export-preview))
            (with-current-buffer "*Chai Export Preview*"
              (should (equal (buffer-string) expected))
              (should-not buffer-read-only))
            (let ((file (chai-export-preview-save)))
              (with-temp-buffer
                (insert-file-contents file)
                (should (equal (buffer-string) expected))))))
      (chai-test--kill-preview-buffer)
      (delete-directory chai-export-preview-directory t))))

;;; Store and insert notes

(ert-deftest chai-test-store-notes-wraps-snapshot-without-kill-ring ()
  "Storing snapshots the notes under a title heading and leaves the kill ring."
  (let ((chai-stored-notes nil)
        (kill-ring nil))
    (chai-test--with-temp-org
        "#+TITLE: Real Book\n** Orphan\n[[chai:key][One]]\n* Chapter\n[[chai:key][Two]]\n"
      (setq-local buffer-file-name "/tmp/20240101T120000__book.org")
      (chai-store-notes 'buffer)
      (erase-buffer))
    (should-not kill-ring)
    (should (= 1 (length chai-stored-notes)))
    (let ((entry (car chai-stored-notes)))
      (should (= 2 (plist-get entry :count)))
      (should (string-search ":SOURCE: [[chai:20240101T120000][Real Book]]"
                             (plist-get entry :text)))
      (should (equal (chai-test--export-headlines (plist-get entry :text))
                     '("* Real Book" "*** Orphan" "**** [KEY] One"
                       "** Chapter" "*** [KEY] Two")))
      (with-temp-buffer
        (org-mode)
        (insert (plist-get entry :text))
        (should (org-kill-is-subtree-p (buffer-string)))))))

(ert-deftest chai-test-store-notes-requires-notes ()
  "Storing a document without Chai notes is an error, not an empty entry."
  (let ((chai-stored-notes nil))
    (chai-test--with-temp-org "* Chapter\nplain text\n"
      (should-error (chai-store-notes 'buffer) :type 'user-error))
    (should-not chai-stored-notes)))

(ert-deftest chai-test-store-notes-buffer-replaces-same-source ()
  "A whole-buffer store replaces earlier notes from the same source."
  (let ((chai-stored-notes nil))
    (chai-test--with-temp-org "* A\n[[chai:key][One]]\n* B\n[[chai:key][Two]]\n"
      (setq-local buffer-file-name "/tmp/source.org")
      (chai-store-notes 'region (point-min) (line-end-position 2))
      (chai-store-notes 'region (point-min) (line-end-position 2))
      (should (= 1 (length chai-stored-notes)))
      (chai-store-notes 'buffer)
      (should (= 1 (length chai-stored-notes)))
      (should (= 2 (plist-get (car chai-stored-notes) :count))))))

(ert-deftest chai-test-insert-stored-notes-as-last-child ()
  "Insertion puts the notes after the current heading's subtree, one level down."
  (let ((chai-stored-notes nil))
    (chai-test--with-temp-org "* Chapter\n[[chai:key][One]]\n"
      (setq-local buffer-file-name "/tmp/source.org")
      (chai-store-notes 'buffer))
    (chai-test--with-temp-org "* A\nmy thoughts\nmore\n** B\n* Z\n"
      (search-forward "my th")
      (chai-insert-stored-notes (car chai-stored-notes))
      (should (looking-at-p "\\*\\* source$"))
      (should (equal (chai-test--export-headlines (buffer-string))
                     '("* A" "** B" "** source" "*** Chapter"
                       "**** [KEY] One" "* Z")))
      (should (string-search "my thoughts\nmore\n** B" (buffer-string))))
    (should-not chai-stored-notes)))

(ert-deftest chai-test-insert-stored-notes-before-first-heading-and-keep ()
  "Before any heading, insert at top level; the option keeps the entry."
  (let ((chai-stored-notes nil)
        (chai-stored-notes-keep-after-insertion t))
    (chai-test--with-temp-org "[[chai:key][One]]\n"
      (setq-local buffer-file-name "/tmp/source.org")
      (chai-store-notes 'buffer))
    (chai-test--with-temp-org "preamble\n* A\n"
      (chai-insert-stored-notes (car chai-stored-notes))
      (should (equal (chai-test--export-headlines (buffer-string))
                     '("* source" "** [KEY] One" "* A")))
      (should (string-prefix-p "preamble\n" (buffer-string))))
    (should (= 1 (length chai-stored-notes)))))

(ert-deftest chai-test-store-notes-title-from-managed-file-name ()
  "Without #+TITLE, stored notes use the title part of a Library file name."
  (let ((chai-stored-notes nil))
    (chai-test--with-temp-org "[[chai:key][One]]\n"
      (setq-local buffer-file-name
                  "/tmp/20240101T120000__Some-Author__Book-Title--done-5.org")
      (chai-store-notes 'buffer))
    (should (equal "Book-Title" (plist-get (car chai-stored-notes) :title)))
    (should (string-prefix-p "* Book-Title\n"
                             (plist-get (car chai-stored-notes) :text)))))

(ert-deftest chai-test-insert-stored-notes-as-sibling ()
  "With a prefix, insert after the heading's subtree at the same level."
  (let ((chai-stored-notes nil))
    (chai-test--with-temp-org "[[chai:key][One]]\n"
      (setq-local buffer-file-name "/tmp/source.org")
      (chai-store-notes 'buffer))
    (chai-test--with-temp-org "* Book 3\nbody\n** 3.1\n"
      (search-forward "body")
      (chai-insert-stored-notes (car chai-stored-notes) t)
      (should (equal (chai-test--export-headlines (buffer-string))
                     '("* Book 3" "** 3.1" "* source" "** [KEY] One"))))))
