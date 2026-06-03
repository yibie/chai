;;; chai-library-batch-rename.el --- Batch auto-rename unmanaged library files -*- lexical-binding: t; -*-
;;
;; Usage (from terminal, run inside the chai directory):
;;   emacs --batch -L . -l chai-library.el -l chai-library-batch-rename.el \
;;         --eval '(chai-library-batch-rename)'
;;
;; Or from any directory (replace /path/to/chai):
;;   emacs --batch -L /path/to/chai -l chai-library.el -l chai-library-batch-rename.el \
;;         --eval '(chai-library-batch-rename)'
;;
;; With a custom library directory:
;;   emacs --batch -L . -l chai-library.el -l chai-library-batch-rename.el \
;;         --eval '(setq chai-library-directory "~/my-lib/")' \
;;         --eval '(chai-library-batch-rename)'
;;
;; Dry-run mode (preview only, no actual renames):
;;   emacs --batch -L . -l chai-library.el -l chai-library-batch-rename.el \
;;         --eval '(chai-library-batch-rename t)'

(require 'chai-library)

(defun chai-library-batch-rename (&optional dry-run)
  "Batch rename all unmanaged .org files in `chai-library-directory'.
With optional DRY-RUN non-nil, only print what would be renamed.
Designed to run in `emacs --batch' mode without UI interaction."
  (unless (file-exists-p chai-library-directory)
    (message "ERROR: Library directory does not exist: %s" chai-library-directory)
    (kill-emacs 1))

  (let ((files (directory-files chai-library-directory nil "\\.org\\'"))
        (renamed 0)
        (skipped 0)
        (errors 0))

    (princ (format "Scanning %d .org files in %s...\n"
                   (length files) chai-library-directory))

    (dolist (file files)
      (princ (format "  %s ... " file))

      ;; Skip already-managed files
      (if (chai-library--parse-managed-filename file)
          (progn
            (princ "SKIP (already managed)\n")
            (setq skipped (1+ skipped)))

        (let* ((file-path (expand-file-name file chai-library-directory))
               (meta (chai-library--read-file-metadata file-path))
               (title (plist-get meta :title))
               (author (plist-get meta :author)))

          (if (not (or title author))
              (progn
                (princ "SKIP (no #+TITLE or #+AUTHOR)\n")
                (setq skipped (1+ skipped)))

            ;; Has metadata - generate new filename
            (let* ((id (or (plist-get meta :id)
                           (chai-library--generate-id)))
                   (book (chai-book-create
                          :id id
                          :author (or author "")
                          :title (or title (file-name-base file-path))
                          :keywords (plist-get meta :keywords)
                          :status nil
                          :rating nil
                          :file-path file-path))
                   (new-filename (chai-library--generate-filename book))
                   (new-path (expand-file-name new-filename chai-library-directory)))

              (if (and (not dry-run) (file-exists-p new-path))
                  (progn
                    (princ (format "ERROR (target exists: %s)\n" new-filename))
                    (setq errors (1+ errors)))

                (if dry-run
                    (progn
                      (princ (format "WOULD RENAME -> %s\n" new-filename))
                      (setq renamed (1+ renamed)))
                  ;; Ensure file has ID in PROPERTIES drawer
                  (condition-case err
                      (progn
                        (chai-library--ensure-file-id file-path)
                        (rename-file file-path new-path)
                        (princ (format "OK -> %s\n" new-filename))
                        (setq renamed (1+ renamed)))
                    (error
                     (princ (format "ERROR: %s\n" (error-message-string err)))
                     (setq errors (1+ errors)))))))))))

    (princ (format "\nDone: %d renamed, %d skipped, %d errors\n"
                   renamed skipped errors))
    (kill-emacs (if (> errors 0) 1 0))))

(provide 'chai-library-batch-rename)
;;; chai-library-batch-rename.el ends here
