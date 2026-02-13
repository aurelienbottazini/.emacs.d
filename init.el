;;; -*- lexical-binding: t; -*-

;; Ultra-fast startup: load pre-tangled config directly.
;; To retangle manually after editing init.org, run:
;;   M-x abo/tangle-init-org
(let ((init-el (expand-file-name "init-tangled.el" user-emacs-directory)))
  (unless (file-exists-p init-el)
    (require 'org)
    (org-babel-tangle-file (expand-file-name "init.org" user-emacs-directory) init-el))
  (load-file init-el))
