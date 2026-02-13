;; Startup perf: avoid double package initialization and expensive handlers early.
(setq package-enable-at-startup nil)

(setenv "LSP_USE_PLISTS" "true")

(defvar abo/early-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist abo/early-file-name-handler-alist
                  gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)))
