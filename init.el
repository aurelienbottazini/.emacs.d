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

(load-theme 'tango-dark)

(use-package paredit
             :config
             (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
             (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
             (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
             (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
             (add-hook 'scheme-mode-hook           #'enable-paredit-mode))
