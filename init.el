;;; -*- lexical-binding: t; -*-

(let* ((init-org (expand-file-name "init.org" user-emacs-directory))
       (init-base (expand-file-name "init-tangled" user-emacs-directory))
       (init-el (concat init-base ".el")))
  (unless (file-exists-p init-el)
    (require 'org)
    (message "Tangling init.org -> init-tangled.el")
    (org-babel-tangle-file init-org init-el))
  (when (file-newer-than-file-p init-org init-el)
    (message "init-tangled.el is older than init.org; save init.org to re-tangle."))
  (load init-base nil 'nomessage))
