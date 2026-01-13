;;; -*- lexical-binding: t; -*-

(require 'org)

(let ((init-org (expand-file-name "init.org" user-emacs-directory))
      (init-el (expand-file-name "init-tangled.el" user-emacs-directory)))
  (when (or (not (file-exists-p init-el))
            (file-newer-than-file-p init-org init-el))
    (message "Tangling init.org -> init-tangled.el")
    (org-babel-tangle-file init-org init-el))
  (load-file init-el))
