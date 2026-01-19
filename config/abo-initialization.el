(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 100000000
      gc-cons-percentage 0.6
      file-name-handler-alist nil)


(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold 16777216
                                      gc-cons-percentage 0.1
                                      file-name-handler-alist last-file-name-handler-alist)))

(setq byte-compile-warnings '(cl-functions))

(setq package-archives
      '(("melpa"       . "https://melpa.org/packages/")
        ("gnu"         . "https://elpa.gnu.org/packages/")
        ("nongnu"      . "https://elpa.nongnu.org/nongnu/")))

(setq package-user-dir (concat user-emacs-directory "elpa"))

(setq package--init-file-ensured t) ; do not add things at the end of init.el
(setq package-check-signature nil)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun require-package (package &optional min-version)
  "Ask elpa to install given PACKAGE. You can specify a MIN-VERSION for your PACKAGE."
  (unless (package-installed-p package min-version)
    (package-install package)))

(require-package 'use-package)
(require 'use-package)

(require-package 'diminish)
(require 'diminish)

(setq use-package-compute-statistics t) ;(use-package-report)
(setq use-package-always-ensure t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(if (file-exists-p "~/.emacs.d/.emacs-local")
    (load "~/.emacs.d/.emacs-local"))

(let ((default-directory  "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(provide 'abo-initialization)
