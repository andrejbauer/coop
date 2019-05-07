;; Support for compiling in subdirectories from Emacs. Adapted from Coq source.
((nil
  . ((eval
      . (progn
          ;; root directory (ending with slash)
          (let ((coop-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (coop-project-find-file
                 (and (boundp 'coop-project-find-file) coop-project-find-file)))

            ;; coop tags file
            (when coop-root-directory
              (setq tags-file-name (concat coop-root-directory "TAGS"))
              (add-to-list 'compilation-search-path coop-root-directory)
              ;; Setting the compilation directory to coop root. This is
              ;; mutually exclusive with the setting of default-directory
              ;; below.
              (if (not coop-project-find-file)
                  (setq compile-command (concat "make -C " coop-root-directory)))
              )
            (setq coop-executable (concat coop-root-directory "coop.native")))))))
 (tuareg-mode
  (show-trailing-whitespace . t))
 (coop-mode
  (show-trailing-whitespace . t)))
