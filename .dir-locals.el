;; Support for compiling in subdirectories from Emacs. Adapted from Coq source.
((nil
  . ((eval
      . (progn
          ;; root directory (ending with slash)
          (let ((terminus-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (terminus-project-find-file
                 (and (boundp 'terminus-project-find-file) terminus-project-find-file)))

            ;; terminus tags file
            (when terminus-root-directory
              (setq tags-file-name (concat terminus-root-directory "TAGS"))
              (add-to-list 'compilation-search-path terminus-root-directory)
              ;; Setting the compilation directory to terminus root. This is
              ;; mutually exclusive with the setting of default-directory
              ;; below.
              (if (not terminus-project-find-file)
                  (setq compile-command (concat "make -C " terminus-root-directory)))
              )
            (setq terminus-executable (concat terminus-root-directory "terminus.native")))))))
 (tuareg-mode
  (show-trailing-whitespace . t))
 (terminus-mode
  (show-trailing-whitespace . t)))
