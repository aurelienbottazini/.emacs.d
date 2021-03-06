;; -*- lexical-binding: t; -*-
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold 16777216
                                      gc-cons-percentage 0.1
                                      file-name-handler-alist last-file-name-handler-alist)))

(setq lsp-headerline-arrow ">")

(setq package-archives
      '(("melpa"       . "https://melpa.org/packages/")
        ("org"         . "https://orgmode.org/elpa/")
       ("gnu"         . "http://elpa.gnu.org/packages/")))

(use-package org)

(setq package-user-dir (concat user-emacs-directory "elpa"))

;; this tells package.el not to add those pesky customized variable settings at
;; the end of your init.el
(setq package--init-file-ensured t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun require-package (package &optional min-version)
  "Ask elpa to install given PACKAGE. You can specify a MIN-VERSION for your PACKAGE."
  (unless (package-installed-p package min-version)
    (package-install package)))

(require-package 'use-package)
(require 'use-package)

(setq use-package-compute-statistics t) ;(use-package-report) to show  which package is slow to start.
(setq use-package-always-ensure t) ; Install package if it is missing

(use-package solarized-theme
 :config
(setq solarized-distinct-fringe-background t)
(setq solarized-high-contrast-mode-line t)
 (setq abott/default-color '("#eee8d5" "#657b83" . "#0087ff"))
 (load-theme 'solarized-dark t))

(add-hook 'post-command-hook '(lambda ()
  (let* (
         (color (cond ((minibufferp) abott/default-color)
                      ((evil-emacs-state-p)  '("#4c7073" "#dcdccc" . "#f0dfaf"))
                      ((evil-visual-state-p) '("#adcff1" "#4c4e56" . "#4c4e56"))
                      ((evil-insert-state-p)  '("#97d88a" "#4c4e56" . "#4c4e56"))
                      (t abott/default-color)))
         )
    (set-face-attribute 'mode-line nil :box `(:line-width 2 :color ,(car color)))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line-buffer-id (cddr color))
    (set-face-foreground 'mode-line (cadr color)))))

(use-package evil
  :config
  (setq evil-insert-state-cursor '(bar "#97d88a")
        evil-visual-state-cursor '(box "#adcff1")
        evil-emacs-state-cursor '(box "#ffa2cb")
        evil-normal-state-cursor '(box "#d33682")))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(if (file-exists-p "~/.emacs.d/.emacs-local")
  (load "~/.emacs.d/.emacs-local"))

(let ((default-directory  "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;; (define-key my-keys-minor-mode-map (kbd "C-z") 'suspend-frame)

(defun my-reload-dir-locals-for-current-buffer ()
  "Reloads dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(setq select-enable-clipboard t)
(use-package osx-clipboard
  :diminish osx-clipboard-mode
  :config
  (osx-clipboard-mode t))

(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/adoptopenjdk-12.0.2.jdk/Contents/Home")
(let* ((home-folder (getenv "HOME"))
      (my-paths `("/home/linuxbrew/.linuxbrew/bin/"
                  ,(concat home-folder "/.config/yarn/global/node_modules/.bin/")
                  ,(concat home-folder "/.local/share/n/bin")
                  ,(concat home-folder "/work/dox-compose/bin/")
                  ,(concat home-folder "/.rbenv/bin/")
                  ,(concat home-folder "/.rbenv/shims/")
                  ,(concat home-folder "/dotfiles/bin/")
                  ,(concat home-folder "/.fzf/bin")
                  ,(concat home-folder "/.local/bin")
                  ,(concat home-folder "/.local/share/npm/bin/")
                  ,(concat home-folder "/bin")
                  "/snap/bin"
                  "/usr/local/bin"
                  "/bin/"
                  "/usr/local/sbin/"
                  "/usr/bin/"))
      )
  (setenv "PATH" (mapconcat 'identity my-paths ":" ))
  (setq exec-path my-paths))

(defun check-large-file-hook ()
  "If a file is over a given size, turn off minor modes."
  (when (> (buffer-size) (* 1024 100)) ; 100K
    (fundamental-mode)
    (font-lock-mode -1)
    (setq buffer-read-only t)
    (buffer-disable-undo)))
(add-hook 'find-file-hooks 'check-large-file-hook)

(setq help-window-select t ; if an help window appears, give it focus
      inhibit-startup-message t
      default-indicate-empty-lines nil ; show end of buffer on left fringe
      tab-always-indent 'complete ; try to indent first, if already indented try to complete
)

(make-variable-buffer-local 'compile-command) ; makes the compile command be buffer specific.
(defalias 'yes-or-no-p 'y-or-n-p) ; instead of typing yes or no, type y or n
(setq ring-bell-function 'ignore) ; please don't startle me with a bell!

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '((".*" . "~/.local/share/emacs-saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      delete-by-moving-to-trash t
      auto-save-default t ;files starting with # are generated by autosave
      auto-save-timeout 60 ; number of seconds before auto-saving
      auto-save-interval 200 ; number of keystrokes before auto-saves
      version-control t ; use versioned backups
      create-lockfiles nil
      auto-save-file-name-transforms `((".*" ,"~/.local/share/emacs-saves" t))
)

(setq global-auto-revert-non-file-buffers t) ; also auto-revert dired buffers and other special buffers

;; if file has no change, just load any changes
;; coming from an external process
(global-auto-revert-mode 1)

;; replace selected text when typing. Not very useful as I use vim keybindings.
;; Still nice to have as a default
(pending-delete-mode 1)

(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'process "\\*compilation\\*\\'"   'utf-8)

(set-default 'truncate-lines t) ; gives each line only one visual line and don't show a continuation on next line

(setq sentence-end-double-space nil)

(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(require 're-builder)
(setq reb-re-syntax 'string)

(setq-default
 indent-tabs-mode nil    ; no tabs
 c-basic-offset 2)

(setq-default whitespace-style '(face trailing tabs tab-mark))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'whitespace-mode)
(eval-after-load "whitespace"
  '(diminish 'whitespace-mode))

(recentf-mode 1)
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200)

(defun sudo ()
  "Use TRAMP to `sudo' the file for current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp. MY-PAIR is a
cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(defun filepath-with-line-number-for-current-buffer ()
  "Return a string with Buffer-file-name:line-number.
             Make it easier to prepare commands for tools like rspec"
  (interactive)
  (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos))))

(defun abott/today ()
  "Today's date as a string."
  (format-time-string "%Y-%m-%d"))

(defun add-date-to-filename ()
  "Add current date in front of filename for current buffer. This is useful with some
        Blog tools like Jekyll to publish new articles."
  (interactive)
  (let* ((date (abott/today))
         (buffer-file (buffer-file-name))
         (new-file-name (concat (file-name-directory buffer-file)
                                date
                                "-"
                                (file-name-nondirectory buffer-file)))
         )
    (rename-file buffer-file new-file-name)
    (set-visited-file-name new-file-name)
    (save-buffer)))

(defun abott/insert-date ()
  "Insert today's date in current buffer"
  (interactive)
  (insert (abott/today)))

(defun toggle-html-export-on-save ()
  "Enable or disable HTML export when saving current org buffer."
  (interactive)
  (when (not (eq major-mode 'org-mode))
    (error "Not an org-mode file!"))
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn (remove-hook 'after-save-hook 'org-html-export-to-html t)
             (message "Disabled org html export on save"))
    (add-hook 'after-save-hook 'org-publish-current-file nil t)
    (set-buffer-modified-p t)
    (message "Enabled org html export on save")))


(defun abo-jump-to-note-file ()
  "Jump to org note file for current buffer"
  (interactive)
  (find-file **local-note-file**))
(define-key my-keys-minor-mode-map "\C-cn" 'abo-jump-to-note-file)

(defun abo-change-line-endings-to-unix ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

(if (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode t))

(blink-cursor-mode 0)
(column-number-mode) ; column number in the mode line

(electric-indent-mode t)
(define-key my-keys-minor-mode-map (kbd "C-c oi") 'electric-indent-mode)

(electric-pair-mode t)
(defun inhibit-electric-pair-mode-in-minibuffer (char)
  (minibufferp))
(setq electric-pair-inhibit-predicate #'inhibit-electric-pair-mode-in-minibuffer)

(setq frame-title-format "emacs")

(setq blink-matching-paren 'jump-offscreen)
(show-paren-mode 1)

;; makes fringe big enough with HDPI
(when (boundp 'fringe-mode)
  (fringe-mode 20))

(use-package diminish
  :config
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode))
    (eval-after-load "subword"
    '(diminish 'subword-mode))
  (diminish 'auto-fill-function)
  (diminish 'my-keys-minor-mode)
  (diminish 'eldoc-mode))

(defun tmux-socket-command-string ()
  (interactive)
  (concat "tmux -S "
          (replace-regexp-in-string "\n\\'" ""
                                    (shell-command-to-string "echo $TMUX | sed -e 's/,.*//g'"))))

(defun tmux-move-right ()
  (interactive)
  (condition-case nil
      (evil-window-right 1)
    (error (unless window-system (shell-command (concat
                                                 (tmux-socket-command-string) " select-pane -R") nil)))))

(defun tmux-move-left ()
  (interactive)
  (condition-case nil
      (evil-window-left 1)
    (error (unless window-system (shell-command (concat
                                                 (tmux-socket-command-string) " select-pane -L") nil)))))

(defun tmux-move-up ()
  (interactive)
  (condition-case nil
      (evil-window-up 1)
    (error (unless window-system (shell-command (concat
                                                 (tmux-socket-command-string) " select-pane -U") nil)))))

(defun tmux-move-down ()
  (interactive)
  (condition-case nil
      (evil-window-down 1)
    (error (unless window-system (shell-command (concat
                                                 (tmux-socket-command-string) " select-pane -D") nil)))))

(define-key my-keys-minor-mode-map (kbd "C-h") 'tmux-move-left)
(define-key my-keys-minor-mode-map (kbd "C-j") 'tmux-move-down)
(define-key my-keys-minor-mode-map (kbd "C-k") 'tmux-move-up)
(define-key my-keys-minor-mode-map (kbd "C-l") 'tmux-move-right)

(defun my-prog-mode-auto-fill-hook ()
  (setq fill-column 80)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))
(add-hook 'prog-mode-hook 'my-prog-mode-auto-fill-hook)

;; First install the package:
(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :mode "\\.clj\\'"
  :after evil
  :config
  (require 'flycheck-clj-kondo)
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package cider
  :after evil
  :config
  (setq cider-repl-display-help-banner nil)
(defadvice cider--debug-mode ( after activate-emacs-state activate)
  (evil-make-intercept-map cider--debug-mode-map)
  (evil-normal-state))
  ;;(evil-make-intercept-map cider--debug-mode-map 'normal)
  )

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package ruby-mode
  :mode "\\.rake\\'"
  :mode "Rakefile\\'"
  :mode "\\.gemspec\\'"
  :mode "\\.ru\\'"
  :mode "Gemfile\\'"
  :mode "Guardfile\\'"
  :mode "Capfile\\'"
  :mode "\\.cap\\'"
  :mode "\\.thor\\'"
  :mode "\\.rabl\\'"
  :mode "Thorfile\\'"
  :mode "Vagrantfile\\'"
  :mode "\\.jbuilder\\'"
  :mode "Podfile\\'"
  :mode "\\.podspec\\'"
  :mode "Puppetfile\\'"
  :mode "Berksfile\\'"
  :mode "Appraisals\\'"
  :mode "\\.rb$"
  :mode "ruby"
  :config

  (add-hook 'ruby-mode-hook 'subword-mode)

  (define-key ruby-mode-map (kbd "C-c C-c") 'xmp)
  (use-package ruby-interpolation
    :diminish ruby-interpolation-mode)
  (use-package ruby-end
    :diminish ruby-end-mode
    :config
    (defun ruby-end-insert-end ()
      "Closes block by inserting end."
      (save-excursion
        (newline)
        (insert "end")
        (indent-according-to-mode)))
    )
  (use-package rspec-mode))

(require 'rcodetools)
(defadvice comment-dwim (around rct-hack activate)
    "If comment-dwim is successively called, add => mark."
    (if (and (or (eq major-mode 'enh-ruby-mode)
                 (eq major-mode 'ruby-mode))
             (eq last-command 'comment-dwim))
        (progn
          (if (eq major-mode 'enh-ruby-mode)
              (end-of-line))
          (insert "=>"))
      ad-do-it))

(use-package go-mode
  :mode "\\.go\\'")

(use-package web-mode
  :mode "\\.html\\'"
  :mode "\\.gohtml\\'"
  :config
  (setq web-mode-enable-auto-closing t))

(use-package emmet-mode
  :hook (css-mode sgml-mode web-mode)
  :after evil
  :diminish emmet-mode
  :config
  (progn
    (evil-define-key 'insert emmet-mode-keymap (kbd "C-j") 'emmet-expand-line)
    (evil-define-key 'emacs emmet-mode-keymap (kbd "C-j") 'emmet-expand-line))

  (add-hook 'css-mode-hook
            (lambda ()
              (emmet-mode)
              (setq emmet-expand-jsx-className? nil)))

  (add-hook 'sgml-mode-hook
            (lambda ()
              (emmet-mode)
              (setq emmet-expand-jsx-className? nil))))

(use-package scss-mode :mode "\\.scss\\'")

(use-package sass-mode :mode "\\.sass\\'")

(use-package less-css-mode :mode "\\.less\\'")

(require 'compile)
(setq compilation-error-regexp-alist-alist
      (cons '(node "^\\([a-zA-Z\.0-9\/-]+\\):\\([0-9]+\\)$"
                   1 ;; file
                   2 ;; line
                   )
            compilation-error-regexp-alist-alist))
(setq compilation-error-regexp-alist
      (cons 'node compilation-error-regexp-alist))

(add-hook 'js2-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "node %s" (file-name-nondirectory buffer-file-name)))))

(setq js-indent-level 2)

(use-package js2-mode
  :mode "\\.js\\'"
  :mode "\\.jsx\\'"
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2
        js2-highlight-level 3
        css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-script-padding 0
        web-mode-css-indent-offset 2
        web-mode-style-padding 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2)
  :config
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook (lambda() (subword-mode t)))

  ;; (use-package xref-js2
  ;;   :init
  ;;   (setq xref-js2-search-program 'rg)
  ;;   :config
  ;;   (add-hook 'js2-mode-hook (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package json-mode
  :mode "\\.json\\'"
  :mode "\\.eslintrc\\'")

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :config
  (use-package highlight-indentation)
  (add-hook 'coffee-mode-hook '(lambda () (highlight-indentation-mode)))
  (add-hook 'coffee-mode-hook '(lambda () (subword-mode +1)))
  (custom-set-variables '(coffee-tab-width 2)))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package prettier-js
  :diminish prettier-js-mode
  :config
  (setq prettier-args '(
                        "--trailing-comma" "es5"
                        "--single-quote" "true"
                        )
        prettier-js-command "prettier")
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package context-coloring
  :ensure t
  :diminish context-coloring-mode
  :bind (:map my-keys-minor-mode-map ("C-c oc" . context-coloring-mode))
  :config
  (add-hook 'js2-mode-hook 'context-coloring-mode))

(add-to-list 'magic-mode-alist '("^import.*React.* from 'react'" . my-jsx-hook) )
(defun my-jsx-hook ()
  "Set web mode with adjustments for JSX"
  (interactive)
  (web-mode)
  (web-mode-set-content-type "jsx")
  (setq emmet-expand-jsx-className? t)
  (emmet-mode)))

(use-package web-mode
  :mode "\\.vue\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 0)
  (defun jjpandari/merge-imenu (index-fun)
    (interactive)
    (let ((mode-imenu (funcall index-fun))
          (custom-imenu (imenu--generic-function imenu-generic-expression)))
      (append custom-imenu mode-imenu)))

  (use-package prettier-js
    :config
    (add-hook 'web-mode-hook #'(lambda ()
                                 (enable-minor-mode
                                  '("\\.vue?\\'" . prettier-js-mode)))))

  (add-hook 'web-mode-hook
            (lambda ()
              (setq imenu-create-index-function (lambda () (jjpandari/merge-imenu 'web-mode-imenu-index))))))

(require 'aurayb-narrow-indirect-vue)
;; (define-key my-keys-minor-mode-map (kbd "nj") (aurayb-make-narrow-indirect-vue "script" 'js2-mode))
;; (define-key my-keys-minor-mode-map (kbd "nh") (aurayb-make-narrow-indirect-vue "template" 'html-mode))
;; (define-key my-keys-minor-mode-map (kbd "ns") (aurayb-make-narrow-indirect-vue "style" 'scss-mode))
;; (define-key my-keys-minor-mode-map (kbd "nn") '(lambda () (interactive) (pop-to-buffer-same-window (buffer-base-buffer))))

(use-package flycheck
  :diminish flycheck-mode
  :init
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'cfn-mode-hook 'flycheck-mode)
  (add-hook 'ruby-mode-hook 'flycheck-mode)
  :config
(advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  (define-key evil-normal-state-map (kbd "[f") 'flycheck-previous-error)
  (define-key evil-normal-state-map (kbd "]f") 'flycheck-next-error)

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(define-derived-mode cfn-mode yaml-mode
  "Cloudformation"
  "Cloudformation template mode.")
(add-to-list 'auto-mode-alist '(".template.yaml\\'" . cfn-mode))
(flycheck-define-checker cfn-lint
  "A Cloudformation linter using cfn-python-lint.
            See URL 'https://github.com/awslabs/cfn-python-lint'."
  :command ("cfn-lint" "-f" "parseable" source)
  :error-patterns (
                   (warning line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "W" (one-or-more digit)) ":" (message) line-end)
                   (error line-start (file-name) ":" line ":" column
                          ":" (one-or-more digit) ":" (one-or-more digit) ":"
                          (id "E" (one-or-more digit)) ":" (message) line-end)
                   )
  :modes (cfn-mode))
(add-to-list 'flycheck-checkers 'cfn-lint))

(use-package evil
  :config
  (setq evil-want-C-i-jump nil)
  (evil-define-key 'insert lisp-interaction-mode-map (kbd "C-j") 'eval-print-last-sexp))

(use-package key-chord
  :defer 2
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state))

(use-package which-key
  :diminish which-key-mode
  :after magit
  :config
  (which-key-mode))

(define-key my-keys-minor-mode-map (kbd "C-M-e") 'recursive-edit)

;; makes grep buffers writable and apply the changes to files.
(use-package wgrep :defer t)

(use-package paredit
  :diminish paredit-mode
  :bind (:map my-keys-minor-mode-map
         ("C-c 0" . paredit-forward-slurp-sexp)
         ("C-c 9" . paredit-backward-slurp-sexp)
         ("C-c ]" . paredit-forward-barf-sexp)
         ("C-c [" . paredit-backward-barf-sexp))
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package expand-region
  :bind (:map my-keys-minor-mode-map ("C-c w" . er/expand-region)))

(define-key my-keys-minor-mode-map (kbd "C-c a") 'org-agenda)
(define-key my-keys-minor-mode-map (kbd "C-c R") 'revert-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c jc") 'org-clock-jump-to-current-clock)
(define-key my-keys-minor-mode-map (kbd "C-c je") '(lambda () (interactive) (find-file "~/.emacs.d/init.org")))
(define-key my-keys-minor-mode-map (kbd "C-c jp") '(lambda () (interactive) (find-file "~/projects/")))
(define-key my-keys-minor-mode-map (kbd "C-c jw") '(lambda () (interactive) (find-file "~/work")))
(define-key my-keys-minor-mode-map (kbd "C-c jn") '(lambda () (interactive) (find-file (concat **local-dropbox-folder** "/org/notes.org"))))
(define-key my-keys-minor-mode-map (kbd "C-c jr") '(lambda () (interactive) (find-file (concat **local-dropbox-folder** "org/references-notes"))))
(define-key my-keys-minor-mode-map (kbd "C-c jj") 'dired-jump)
(define-key my-keys-minor-mode-map (kbd "C-c k") 'recompile)
(define-key my-keys-minor-mode-map (kbd "C-c K") 'compile)

(define-key my-keys-minor-mode-map (kbd "<f5>") 'ispell-buffer)

(define-key my-keys-minor-mode-map (kbd "C-c h") 'highlight-symbol-at-point)
(define-key my-keys-minor-mode-map (kbd "C-c H") 'unhighlight-regexp)

(defun hide-line-numbers ()
  (interactive)
  (setq display-line-numbers (quote nil)))
(define-key my-keys-minor-mode-map (kbd "C-c olh") 'hide-line-numbers)

(defun show-line-numbers ()
  (interactive)
  (setq display-line-numbers (quote absolute)))
(define-key my-keys-minor-mode-map (kbd "C-c oll") 'show-line-numbers)
(define-key my-keys-minor-mode-map (kbd "C-c ow") 'visual-line-mode)
(define-key my-keys-minor-mode-map (kbd "C-c of") 'auto-fill-mode)
(global-hl-line-mode t)
(define-key my-keys-minor-mode-map (kbd "C-c og") 'global-hl-line-mode)
(define-key my-keys-minor-mode-map (kbd "C-c op") 'show-paren-mode)

(use-package rainbow-mode
  :diminish rainbow-mode
  :bind (:map my-keys-minor-mode-map
              ("C-c or" . rainbow-mode)))

(define-key my-keys-minor-mode-map (kbd "C-c ot") 'toggle-truncate-lines)

(use-package evil
  :config
   (define-key evil-normal-state-map (kbd "[b") 'previous-buffer)
   (define-key evil-normal-state-map (kbd "]b") 'next-buffer)
   (define-key evil-normal-state-map (kbd "]e") 'next-error)
   (define-key evil-normal-state-map (kbd "[e") 'previous-error))

(use-package windresize
  :bind (:map evil-normal-state-map
              ("C-w r" . windresize)))

(use-package drag-stuff
  :diminish t
  :bind (:map my-keys-minor-mode-map
         ("C-M-<up>" . drag-stuff-up)
         ("C-M-<down>" . drag-stuff-down))
  :config
  (drag-stuff-global-mode t))

(setq org-directory **local-dropbox-folder**)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(use-package evil
  :init
  (setq org-use-speed-commands nil) ; they don't work well with Evil.
  :config
  (evil-define-key 'normal org-mode-map
    (kbd "M-l") 'org-shiftmetaright
    (kbd "M-h") 'org-shiftmetaleft
    (kbd "M-k") 'org-move-subtree-up
    (kbd "M-j") 'org-move-subtree-down
    (kbd "M-p") 'org-publish-current-project
    (kbd "TAB") 'org-cycle)
  )

(require 'org-habit)
(add-to-list 'org-modules "org-habit")
(add-to-list 'org-modules "org-git-link")
(setq org-log-into-drawer t)
(define-key my-keys-minor-mode-map "\C-ci" 'counsel-org-goto)
(define-key org-mode-map "\C-c\C-x\C-t" 'counsel-org-tag)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(canceled@)")))

(use-package deft
 :bind (("<f9>" . deft))
 :commands (deft)
 :init
 (setq deft-extensions '("org" "md")
       deft-recursive t
       deft-directory (concat **local-dropbox-folder** "org/")))

(use-package markdown-mode
 :mode "\\.md\\'")

(define-key my-keys-minor-mode-map "\C-cl" 'org-store-link)

(use-package palimpsest
  :diminish palimpsest-mode
  :config
  (add-hook 'org-mode-hook 'palimpsest-mode))

(setq org-capture-templates
      '(("n" "Notes" entry (file+headline **local-note-file** "Inbox") "* %?\n")
        ("t" "todo" entry (file+headline **local-note-file** "Inbox")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))
      )

(define-key my-keys-minor-mode-map (kbd "C-c n") '(lambda () (interactive) (org-capture nil "n")))
(add-hook 'org-capture-mode-hook 'evil-insert-state)

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "global-org-capture" (frame-parameter nil 'name))
      (progn
        (delete-frame))))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "global-org-capture" (frame-parameter nil 'name))
      (progn
        (delete-frame))))

;; make the frame contain a single window. by default org-capture
;; splits the window.
(add-hook 'org-capture-mode-hook
          'delete-other-windows)

(require 'ob-clojure) ;; run cider-jack-in from org buffer to be able to run
                      ;; clojure code
(use-package ob-clojurescript) ;; requires [[https://github.com/anmonteiro/lumo][lumo]]
(setq org-babel-clojure-backend 'cider)
(require 'ob-js)
(setq org-babel-js-function-wrapper "require('util').log(require('util').inspect(function(){%s}()));")
(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)
                               (ditaa . t)))
(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar")

(use-package ob-graphql)

(setq
 time-stamp-active t
 time-stamp-line-limit 30     ; check first 30 buffer lines for Time-stamp:
 time-stamp-format "%04y-%02m-%02d") ;

(use-package writeroom-mode
  :bind (:map my-keys-minor-mode-map
              ("C-c z" . writeroom-mode)))

(use-package htmlize) ; for org html export
(setq system-time-locale "C") ; make sure time local is in english when exporting
(setq org-html-validation-link nil)
(setq org-publish-project-alist
      `(
        ("blog-files"
         :base-directory ,(concat **local-dropbox-folder** "/org/blog")
         :base-extension "org"
         :publishing-directory ,(concat **local-dropbox-folder** "/org/blog_published")
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         :html-head-extra nil
         ;; :body-only nil
         )
        ;; ... add all the components here (see below)...
        ;; ("wiki" :components ("wiki-files"))
        )
      user-full-name "Aurélien Bottazini"
      org-export-with-toc t
      org-html-doctype "html5"
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/main.css\" />"
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-html5-fancy t
      org-html-postamble nil
      org-src-preserve-indentation nil
      org-html-htmlize-output-type "css"
      org-html-indent nil               ; a value other than nil will screw up src block indentation
      org-edit-src-content-indentation 0)

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local time-stamp-start "Updated on[ 	]+\\\\?[\"<]+")
            (org-indent-mode t)
            (add-hook 'before-save-hook 'time-stamp nil 'local)))

(add-hook 'write-file-hooks 'time-stamp) ; update time-stamp on save
(require 'ox-publish)
(setq system-time-locale "C") ;; make sure time local is in english when exporting
(setq org-html-validation-link nil)

(add-to-list 'org-latex-packages-alist '("" "listings" nil))
(setq org-latex-listings t)
(setq org-latex-listings-options '(("breaklines" "true")
                                   ("literate" "{0}{0}{1}%
           {1}{1}{1}%
           {2}{2}{1}%
           {3}{3}{1}%
           {4}{4}{1}%
           {5}{5}{1}%
           {6}{6}{1}%
           {7}{7}{1}%
           {8}{8}{1}%
           {9}{9}{1}%
    ")))

(use-package define-word
  :bind (:map evil-normal-state-map
          ("zw" . define-word-at-point)))

(use-package powerthesaurus
  :bind (:map evil-normal-state-map
          ("zs" . powerthesaurus-lookup-word-dwim)))

(use-package writegood-mode
 :bind (:map evil-normal-state-map
 (
         ("zgg" . writegood-mode)
         ("zgr" . writegood-reading-ease)
         ("zgl" . writegood-grade-level)
  )))

(require 'browse-url) ; part of gnu emacs

(defun my-lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
If there is a text selection (a phrase), use that.

This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url (concat "http://en.wikipedia.org/wiki/" word))
    ;; (eww myUrl) ; emacs's own browser
    ))

(use-package evil
  :config
  (defun my-evil-record-macro ()
    (interactive)
    (if buffer-read-only
        (quit-window)
      (call-interactively 'evil-record-macro)))

  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map (kbd "q") 'my-evil-record-macro)))

(use-package evil
  :config
  (evil-set-initial-state 'deft-mode 'insert)
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'use-package-statistics 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'ert-results-mode 'emacs))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode t))

(use-package evil-matchit
  :defer 2
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-search-highlight-persist
  :bind  (:map my-keys-minor-mode-map
              ("C-c oh" . (lambda ()
                            (interactive)
                            (hi-lock-mode -1) (evil-search-highlight-persist-remove-all))
               )
              )
  :config
  (global-evil-search-highlight-persist t))

(use-package evil
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "W" 'save-buffer))

(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings))

(use-package ivy
  :diminish ivy-mode
  :bind (:map my-keys-minor-mode-map
         ("C-c v" . ivy-switch-view)
         ("C-c V" . ivy-push-view)
         :map ivy-minibuffer-map
         ("C-c C-c" . ivy-restrict-to-matches))
  :init
  (setq ivy-display-style 'fancy)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t) ; enable bookmarks and recent-f
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
    '((t      . ivy--regex-plus)))
  :config
  (use-package ivy-hydra)
  (ivy-mode 1))

(use-package avy
  :bind (:map my-keys-minor-mode-map
         ("C-c ;" . avy-goto-char-timer)))

(use-package counsel
  :bind (:map my-keys-minor-mode-map ("C-c f" . counsel-rg)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("C-c C-m" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ido-switch-buffer)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)
         :map my-keys-minor-mode-map
         ("C-c r" . counsel-buffer-or-recentf))
  :init
  (setq counsel-git-cmd "rg --files")
  (setq counsel-rg-base-command
        "rg --smart-case -M 120 --hidden --no-heading --line-number --color never %s .")

  :config
  (eval-after-load "counsel" '(progn
                                (defun counsel-imenu-categorize-functions (items)
                                  "Categorize all the functions of imenu."
                                  (let ((fns (cl-remove-if #'listp items :key #'cdr)))
                                    (if fns
                                        (nconc (cl-remove-if #'nlistp items :key #'cdr)
                                               `((":" ,@fns)))
                                      items))))))

(use-package iedit)

(use-package evil-iedit-state
  :bind (:map my-keys-minor-mode-map ("<f6>" . evil-iedit-state/iedit-mode)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
(setq ediff-split-window-function 'split-window-vertically)

(use-package git-link
  :bind (:map my-keys-minor-mode-map
              ("C-c gl" . git-link))
  :config

  ;; (defun abott/git-link-advice (orig-fun url)
  ;;   "For use with wsl. Copies git-link to windows clipboard."
  ;;   (shell-command (concat "echo "
  ;;                          (shell-quote-argument url)
  ;;                          " | clip.exe") url)
  ;;   (funcall orig-fun url))

  ;; (advice-add 'git-link--new :around #'abott/git-link-advice)
)

(use-package git-timemachine
  :bind (:map my-keys-minor-mode-map
              ("C-c gt" . git-timemachine-toggle))
  :config
  (defadvice git-timemachine-mode (after git-timemachine-change-to-emacs-state activate compile)
    "when entering git-timemachine mode, change evil normal state to emacs state"
    (if (evil-normal-state-p)
        (evil-emacs-state)
      (evil-normal-state)))

  (ad-activate 'git-timemachine-mode))

(use-package fullframe
  :config
  (fullframe vc-annotate quit-window))

(eval-after-load "vc-annotate"
     '(progn
      (define-key vc-annotate-mode-map "j" 'evil-next-line)
      (define-key vc-annotate-mode-map "k" 'evil-previous-line)))

(use-package evil
  :config
  (evil-define-key 'normal diff-mode-map (kbd "q") 'quit-window))

(use-package magit
  :demand true
  :bind (:map my-keys-minor-mode-map
              ("C-c gs" . magit-status)
              ("C-c gc" . magit-commit)
              ("C-c gp" . magit-push-current)
              ("C-c gf" . magit-file-dispatch))
  :init
  (setq magit-commit-show-diff nil
        magit-revert-buffers 1))

(use-package fullframe
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window))

(use-package evil
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package diff-hl
  :after magit
  :config
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package projectile
:bind-keymap
("C-c p" . projectile-command-map)
:config
(projectile-mode +1))

(use-package find-file-in-project
  :ensure t
  :bind (:map  my-keys-minor-mode-map
               ("C-c T" . find-file-in-project-by-selected)
               ("M-p" . ffip)
               :map evil-normal-state-map
               ("gf" . find-file-in-project-at-point))
  :config

  (setq ffip-ignore-filenames (seq-remove (lambda (astring) (string= astring "*.png")) ffip-ignore-filenames))
  (setq ffip-ignore-filenames (seq-remove (lambda (astring) (string= astring "*.jpg")) ffip-ignore-filenames))
  (setq ffip-ignore-filenames (seq-remove (lambda (astring) (string= astring "*.jpeg")) ffip-ignore-filenames))
  (setq ffip-ignore-filenames (seq-remove (lambda (astring) (string= astring "*.gif")) ffip-ignore-filenames))
  (setq ffip-ignore-filenames (seq-remove (lambda (astring) (string= astring "*.bmp")) ffip-ignore-filenames))
  (setq ffip-ignore-filenames (seq-remove (lambda (astring) (string= astring "*.ico")) ffip-ignore-filenames))
  (setq ffip-prefer-ido-mode nil)
  (setq ffip-use-rust-fd t)
  (setq ffip-strip-file-name-regex "\\(\\.mock\\|_test\\|\\.test\\|\\.mockup\\|\\.spec\\)")
  (add-to-list 'ffip-prune-patterns "*/.git/*")
  (add-to-list 'ffip-prune-patterns "*/dist/*")
  (add-to-list 'ffip-prune-patterns "*/.emacs.d/elpa/*")
  (add-to-list 'ffip-prune-patterns "*/.nuxt/*")
  (add-to-list 'ffip-prune-patterns "*/spec/coverage/*")
  (add-to-list 'ffip-prune-patterns "*/public/*")
  (add-to-list 'ffip-prune-patterns "*/.shadow-cljs/*")
  (add-to-list 'ffip-prune-patterns "*/vendor/*")
  (add-to-list 'ffip-prune-patterns "node_modules/*"))

(require 'abott-find-in-project)
(define-key my-keys-minor-mode-map (kbd "C-c s") 'abott-find-file-with-similar-name)

(use-package projectile
  :bind (:map my-keys-minor-mode-map
              ("C-c t" . projectile-find-file)))

(use-package dumb-jump
  :init
  (setq dumb-jump-selector 'ivy)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(setq speedbar-directory-unshown-regexp "^$")
(define-key my-keys-minor-mode-map (kbd "C-c b") 'speedbar-get-focus)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-from-kill try-expand-all-abbrevs try-expand-list))
(require 'mode-local)
(setq-mode-local elisp-mode hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-from-kill try-expand-list try-complete-lisp-symbol-partially try-complete-lisp-symbol))
(setq-mode-local elisp-mode hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-from-kill try-expand-all-abbrevs try-complete-lisp-symbol-partially try-complete-lisp-symbol))
(define-key evil-insert-state-map (kbd "s-/") 'hippie-expand)
(define-key evil-insert-state-map (kbd "M-/") 'hippie-expand)

(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0.2
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '((company-files company-capf))
        company-transformers '(company-sort-by-occurrence))

  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  (setq company-show-numbers t)
  (define-key evil-insert-state-map (kbd "C-x C-o") 'company-complete)

  (use-package company-statistics
    :after company
    :config
    (setq company-statistics-file "~/.emacs.d/company-stats-cache.el")
    (company-statistics-mode +1))
      ;;; Prevent suggestions from being triggered automatically. In particular,
  ;;; this makes it so that:
  ;;; - TAB will always complete the current selection.
  ;;; - RET will only complete the current selection if the user has explicitly
  ;;;   interacted with Company.
  ;;; - SPC will never complete the current selection.
  ;;;
  (dolist (key '("<return>" "RET"))
    ;; Here we are using an advanced feature of define-key that lets
    ;; us pass an "extended menu item" instead of an interactive
    ;; function. Doing this allows RET to regain its usual
    ;; functionality when the user has not explicitly interacted with
    ;; Company.
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                               cmd)))))
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "SPC") nil)
  (autoload 'company-capf "company-capf")
  (autoload 'company-yasnippet "company-yasnippet")
  (autoload 'company-elisp "company-elisp")
  (autoload 'company-files "company-files"))

(use-package yasnippet
  :defer 3
  :commands yas-expand-snippet
  :bind (:map my-keys-minor-mode-map
              ("C-c y" . yas-insert-snippet))
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1)
  (add-hook 'term-mode-hook (lambda()
                              (yas-minor-mode -1))))

(setq ls-lisp-use-insert-directory-program t) ;same ls-lisp for Dired regardless of the platform
(setq dired-listing-switches "-alh")
;; on mac there is some weird prefixing going on for GNU Tools like ls.
;; I favor GNU ls over MacOSX default ls
(when (string-equal system-type "darwin")
  (setq insert-directory-program "gls"))

(require 'dired )
(defun my-dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'my-dired-mode-setup)

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            (setq dired-recursive-copies (quote always)) ; “always” means no asking
            (setq dired-recursive-deletes (quote top)) ; “top” means ask once
            ))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "-" 'dired-up-directory)
     (define-key dired-mode-map (kbd "/") 'evil-search-forward)
     (define-key dired-mode-map (kbd "j") 'dired-next-line)
     (define-key dired-mode-map (kbd "k") 'dired-previous-line)
     (define-key dired-mode-map (kbd "[b") 'previous-buffer)
     (define-key dired-mode-map (kbd "]b") 'next-buffer)
     (define-key dired-mode-map (kbd "C-d") 'evil-scroll-page-down)
     (evil-define-key 'normal dired-mode-map
       "gg" 'evil-goto-first-line
       "^" '(lambda () (interactive) (find-alternate-file "..")))))

(define-key package-menu-mode-map (kbd "/") 'evil-search-forward)

(use-package dired-rsync
:bind (:map dired-mode-map ("p" . dired-rsync)))

(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)))

(setq vc-follow-symlinks t)
(put 'magit-edit-line-commit 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq tags-add-tables 'nil) ; always start a new table don't ask the user

(require 'wat-mode)

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-run)))

(use-package engine-mode
  :bind (:map my-keys-minor-mode-map
              ("C-c d c" . engine/search-caniuse)
              ("C-c d m" . engine/search-mdn)
              ("C-c d ra" . engine/search-rails)
              ("C-c d rr" . engine/search-ruby))
  :config
  (defengine ruby "https://apidock.com/ruby/search?query=%s")
  (defengine rails "https://api.rubyonrails.org/?q=%s")
  (defengine mdn "https://developer.mozilla.org/en-US/search?q=%s")
  (defengine caniuse "https://caniuse.com/#search=%s")
  )

(define-key my-keys-minor-mode-map (kbd "C-c u") 'universal-argument)
(define-key my-keys-minor-mode-map (kbd "C-u") 'evil-scroll-up)

(use-package restclient
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package peep-dired
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package origami
  :config
  (global-origami-mode))

(use-package emamux
  :config
  (define-key my-keys-minor-mode-map (kbd "C-c x") '(lambda () (interactive) (emamux:send-command "!!"))))

(use-package hydra
  :config
  (defhydra hydra-utils (global-map "<f8>")
    "drag"
    ("j" drag-stuff-down "down")
    ("k" drag-stuff-up "up")))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)

(use-package default-text-scale
  :config
  (define-key my-keys-minor-mode-map (kbd "C-=") 'default-text-scale-reset)
  (define-key my-keys-minor-mode-map (kbd "C-+") 'default-text-scale-increase)
  (define-key my-keys-minor-mode-map (kbd "C-M-+") 'default-text-scale-decrease))

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

(setq org-refile-targets '((nil :maxlevel . 3)
                                (org-agenda-files :maxlevel . 3)))
(advice-add 'org-refile :after
        (lambda (&rest _)
        (org-save-all-org-buffers)))

(add-to-list 'auto-mode-alist '("\\aliases\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\exports\\'" . shell-script-mode))

(setq default-frame-alist '((font . "Jetbrains Mono-14")))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(defun abott/org-tree-slide-play ()
  (writeroom-mode 1)
  (default-text-scale-increment 40))
(defun abott/org-tree-slide-stop ()
  (writeroom-mode -1)
  (default-text-scale-reset))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . abott/org-tree-slide-play)
         (org-tree-slide-stop . abott/org-tree-slide-stop))
  :bind (("<f7>" . org-tree-slide-mode)
         ("S-<f7>" . org-tree-slide-skip-done-toggle))
  :config
  (with-eval-after-load "org-tree-slide"
    (define-key org-tree-slide-mode-map (kbd "<f8>") 'org-tree-slide-move-previous-tree)
    (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-next-tree)))

(use-package ox-reveal
  :config
  (setq org-reveal-root "file:///Users/auray/.emacs.d/site-lisp/reveal.js-4.1.0"))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")

  (setq read-process-output-max (* 1024 1024))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (js2-mode . lsp)
         (web-mode . lsp)
         (clojure-mode . lsp)
         (json-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
