(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
             :custom (straight-use-package-by-default t))

(menu-bar-mode -1)
(tool-bar-mode -1)

(unless (display-graphic-p)
  (load-theme 'tango-dark))


(use-package paredit
  :hook
  (paredit-mode . emacs-lisp-mode-hook)
  (paredit-mode . ielm-mode-hook)
  (paredit-mode . lisp-mode-hook)
  (paredit-mode . lisp-interaction-mode-hook)
  (paredit-mode . scheme-mode-hook))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(recentf-mode 1)

(global-set-key (kbd "C-c r") 'recentf-open-files)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Operator Mono SSm AB")))))
