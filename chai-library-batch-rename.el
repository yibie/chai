;;; chai-library-batch-rename.el --- Batch auto-rename unmanaged library files -*- lexical-binding: t -*-
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
      (let* ((file-path (expand-file-name file chai-library-directory))
             (result (chai-library--rename-to-managed file-path dry-run))
             (status (car result))
             (value (cdr result)))
        (pcase status
          ('managed
           (princ "SKIP (already managed)\n")
           (cl-incf skipped))
          ('exists
           (princ (format "ERROR (target exists: %s)\n" value))
           (cl-incf errors))
          ('error
           (princ (format "ERROR: %s\n" value))
           (cl-incf errors))
          ('success
           (princ (format "%s -> %s\n"
                          (if dry-run "WOULD RENAME" "OK")
                          (file-name-nondirectory value)))
           (cl-incf renamed))
          (_
           (princ "ERROR (unknown status)\n")
           (cl-incf errors)))))

    (princ (format "\nDone: %d renamed, %d skipped, %d errors\n"
                   renamed skipped errors))
    (kill-emacs (if (> errors 0) 1 0))))

(provide 'chai-library-batch-rename)
;;; chai-library-batch-rename.el ends here
