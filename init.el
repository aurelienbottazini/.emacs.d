;;; -*- lexical-binding: t; -*-

(let ((init-org (expand-file-name "init.org" user-emacs-directory))
      (init-el (expand-file-name "init-tangled.el" user-emacs-directory)))
  (unless (file-exists-p init-el)
    (require 'org)
    (message "Tangling init.org -> init-tangled.el")
    (org-babel-tangle-file init-org init-el))
  (when (file-newer-than-file-p init-org init-el)
    (message "init-tangled.el is older than init.org; save init.org to re-tangle."))
  (load init-el nil 'nomessage))
