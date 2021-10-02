;; -*- lexical-binding: t; -*-
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold 16777216
                                      gc-cons-percentage 0.1
                                      file-name-handler-alist last-file-name-handler-alist)))

(setq package-archives
      '(("melpa"       . "https://melpa.org/packages/")
        ("org"         . "https://orgmode.org/elpa/")
       ("gnu"         . "http://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq package-user-dir (concat user-emacs-directory "elpa"))

(require 'use-package)

(use-package paredit-mode
  :hook emacs-lisp-mode)

(setq use-package-compute-statistics t) ;(use-package-report) to show  which package is slow to start.
(setq use-package-always-ensure t) ; Install package if it is missing

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))
