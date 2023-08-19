;; -*- lexical-binding: t; -*-
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
        ("org"         . "https://orgmode.org/elpa/")
        ("gnu"         . "http://elpa.gnu.org/packages/")))

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

(use-package diminish
  :config
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode))
  (eval-after-load "subword"
    '(diminish 'subword-mode))
  (diminish 'auto-fill-function)
  (diminish 'org-indent-mode)
  (diminish 'visual-line-mode)
  (diminish 'eldoc-mode))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(if (file-exists-p "~/.emacs.d/.emacs-local")
    (load "~/.emacs.d/.emacs-local"))

(let ((default-directory  "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'org)

(defun my-reload-dir-locals-for-current-buffer ()
  "Reloads dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the current buffer's, reload dir-locals."
  (interactive)
  (let ((dirdefault-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(setq vc-follow-symlinks t)
(put 'magit-edit-line-commit 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq tags-add-tables 'nil) ; always start a new TAGS table don't ask the user

(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/adoptopenjdk-12.0.2.jdk/Contents/Home")
(let* ((home-folder (getenv "HOME"))
       (my-paths `("/opt/homebrew/bin"
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
                   "/usr/bin/"
                   "/usr/local/sbin/"
                   "/opt/homebrew/bin"
                   "/usr/bin/")) ;; /usr/bin/ is repeated because eshell does not consider last entry. Bug?
       )

  (setenv "PATH" (concat (mapconcat 'identity my-paths ":" ) ":"))
  (setq eshell-path-env (concat (mapconcat 'identity my-paths ":" ) ":"))
  (setq exec-path my-paths))

(defun check-large-file-hook ()
  "If a file is over a given size, turn off minor modes."
  (when (> (buffer-size) (* 1024 100)) ; 100K
    (fundamental-mode)
    (font-lock-mode -1)
    (setq buffer-read-only t)
    (buffer-disable-undo)))
(add-hook 'find-file-hooks 'check-large-file-hook)


;; only support left to right languages.
;; this makes long lines in files not a problem anymore.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(global-so-long-mode 1) ;; helps when visiting files with long lines.

(setq help-window-select t ; if an help window appears, give it focus
      inhibit-startup-message t
      default-indicate-empty-lines nil ; show end of buffer on left fringe
      )

(make-variable-buffer-local 'compile-command) ; makes the compile command buffer specific.
(defalias 'yes-or-no-p 'y-or-n-p) ; instead of typing yes or no, type y or n
(setq ring-bell-function 'ignore) ; please don't startle me with a bell!

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '((".*" . "~/.local/share/emacs-saves"))    ; don't litter my filesystem with saves
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

;; replace selected text when typing.
(pending-delete-mode 1)

(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'process "\\*compilation\\*\\'"   'utf-8)

(setq-default truncate-lines nil)

(setq sentence-end-double-space nil)

(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(setq-default
 indent-tabs-mode nil    ; no tabs
 c-basic-offset 2)

(setq-default whitespace-style '(face trailing))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'whitespace-mode)
(eval-after-load "whitespace"
  '(diminish 'whitespace-mode))

(recentf-mode 1)
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200)

(defun auray/tmux-active-session ()
  (interactive)
  ;; (substring-no-properties (shell-command-to-string "tmux ls | grep \\\(attached\\\) | cut -d':' -f1") 0 -1))
 (substring-no-properties (shell-command-to-string "tmux list-clients | grep 'attached,focused,' | cut -d' ' -f2") 0 -1))

(defun auray/tmux-select-pane (direction)
  (shell-command (concat  "tmux select-pane -t " (auray/tmux-active-session) " -" direction)))

(defun auray/tmux-move (direction)
  (condition-case nil
      (cond
       ((string= "R" direction) (windmove-right))
       ((string= "L" direction) (windmove-left))
       ((string= "U" direction) (windmove-up))
       ((string= "D" direction) (windmove-down)))
    (error (unless window-system (auray/tmux-select-pane direction)))))

(defun tmux-move-right ()
  (interactive)
  (auray/tmux-move "R"))

(defun tmux-move-left ()
  (interactive)
  (auray/tmux-move "L"))

(defun tmux-move-up ()
  (interactive)
  (auray/tmux-move "U"))

(defun tmux-move-down ()
  (interactive)
  (auray/tmux-move "D"))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium)
  )

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

(defun auray/today ()
  "Today's date as a string."
  (format-time-string "%Y-%m-%d"))

(defun auray/add-date-to-filename ()
  "Add current date in front of filename for current buffer. This is useful with some Blog tools like Jekyll to publish new articles."
  (interactive)
  (let* ((date (abott/today))
         (buffer-file (buffer-file-name))
         (new-file-name (concat (file-name-directory buffer-file)
                                date
                                "-"
                                (file-name-nondirectory buffer-file)))
         )
    (save-buffer)
    (rename-file buffer-file new-file-name)
    (set-visited-file-name new-file-name)
    (save-buffer)))

(defun auray/insert-date ()
  "Insert today's date in current buffer"
  (interactive)
  (insert (abott/today)))

(defun auray/toggle-html-export-on-save ()
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

(defun auray/change-line-endings-to-unix ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode t))

(blink-cursor-mode 0)
(column-number-mode) ; column number in the mode line

(electric-indent-mode t)

(electric-pair-mode t)
(defun inhibit-electric-pair-mode-in-minibuffer (char)
  (minibufferp))
(setq electric-pair-inhibit-predicate #'inhibit-electric-pair-mode-in-minibuffer)

(setq frame-title-format "emacs")

;; makes fringe big enough with HDPI
(when (boundp 'fringe-mode)
  (fringe-mode 20))

(setq blink-matching-paren 'jump-offscreen)
(show-paren-mode 1)

(require 're-builder)
(setq reb-re-syntax 'string)

(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))
(advice-add 'org-refile :after
            (lambda (&rest _)
              (org-save-all-org-buffers)))

(defun my-prog-mode-auto-fill-hook ()
  (setq fill-column 100)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))
(add-hook 'prog-mode-hook 'my-prog-mode-auto-fill-hook)

;; First install the package:
(use-package clojure-mode
  :mode "\\.clj\\'"
  :config
  (add-hook 'clojure-mode-hook #'subword-mode)

  (use-package rainbow-blocks)
  (add-hook 'clojure-mode-hook #'rainbow-blocks-mode))


(use-package cider
  :after clojure-mode
  :config

  (define-key cider-mode-map (kbd "C-c C-c") 'cider-eval-list-at-point)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (add-hook 'before-save-hook 'cider-format-buffer t t)
        ))

  (add-hook 'edn-mode-hook (lambda ()
                                 (add-hook 'before-save-hook 'cider-format-edn-buffer t t)
        ))
  (setq cider-repl-display-help-banner nil))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; (require 'ruby-ts-mode)
;;(define-key ruby-mode-map (kbd "C-c C-c") 'xmp)

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

  ;; (add-hook 'ruby-mode-hook 'subword-mode)


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
               (eq major-mode 'ruby-mode)
               (eq major-mode 'ruby-ts-mode)
               )
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
  :mode "\\.erb\\'"
  :config
  (setq web-mode-enable-auto-closing t)
  (define-key web-mode-map (kbd "C-c C-c e") 'emmet-expand-line)
  (add-hook
   'web-mode-hook
   (lambda ()
     (setq-local
      electric-pair-pairs
      (append electric-pair-pairs '((?< . ?>))))))
  )

(use-package emmet-mode
  :hook (css-mode sgml-mode web-mode)
  :diminish emmet-mode
  :config
  (define-key css-mode-map (kbd "C-c C-c e") 'emmet-expand-line)
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

(add-hook 'js-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "node %s" (file-name-nondirectory buffer-file-name)))))

(setq js-indent-level 2)

(add-hook 'js-mode-hook (lambda () (subword-mode t)))

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

(use-package js2-mode
  :mode "\\.js\\'"
  :mode "\\.mjs\\'"
  :mode "\\.jsx\\'")

(use-package json-mode
  :mode "\\.json\\'"
  :mode "\\.eslintrc\\'")

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :config
  (add-hook 'coffee-mode-hook (lambda () (subword-mode +1)))
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
        prettier-js-command "/home/auray/.local/share/npm/bin/prettier"))

(use-package context-coloring
  :ensure t
  :hook ((js2-mode . context-coloring-mode))
  :bind (("C-c oc" . context-coloring-mode)))

(add-to-list 'magic-mode-alist '("^import.*React.* from 'react'" . my-jsx-hook) )
(defun my-jsx-hook ()
  "Set web mode with adjustments for JSX"
  (interactive)
  (web-mode)
  (web-mode-set-content-type "jsx")
  (setq emmet-expand-jsx-className? t)
  (emmet-mode))

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
    (add-hook 'web-mode-hook (lambda ()
                               (enable-minor-mode
                                '("\\.vue?\\'" . prettier-js-mode)))))

  (add-hook 'web-mode-hook
            (lambda ()
              (setq imenu-create-index-function (lambda () (jjpandari/merge-imenu 'web-mode-imenu-index))))))

(require 'aurayb-narrow-indirect-vue)

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-run)))

(require 'wat-mode)

(add-to-list 'auto-mode-alist '("\\aliases\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\exports\\'" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)

(use-package elm-mode)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; makes grep buffers writable and apply the changes to files.
(use-package wgrep :defer t)

(use-package paredit
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package expand-region)

(global-display-line-numbers-mode -1)
(defun show-line-numbers ()
  (interactive)
  (setq display-line-numbers 'absolute))
(defun hide-line-numbers ()
  (interactive)
  (setq display-line-numbers 'nil))
(defun show-relative-line-numbers ()
  (interactive)
  (setq display-line-numbers 'relative))

(global-hl-line-mode -1)

(use-package rainbow-mode :diminish rainbow-mode)
(use-package windresize)

(use-package ivy-hydra)
(use-package general
  :config

  (general-create-definer my-leader-def
    :prefix "SPC")

  (my-leader-def
    :states 'normal
    :keymaps 'override
    "b" 'counsel-switch-buffer
    "c" (lambda () (interactive) (org-capture nil "n"))
    "d" 'flymake-show-buffer-diagnostics
    "D" 'flymake-show-project-diagnostics
    "e" 'recentf
    "f" 'counsel-rg
    "F" 'rg-project
    "g" 'magit-status
    "G" 'magit-file-dispatch
    "h" 'highlight-symbol-at-point
    "H" 'unhighlight-regexp
    "i" 'counsel-imenu
    "p" 'project-find-file
    "s" 'auray/find-file-with-similar-name
    "t" 'tab-switch
    "rr" 'eglot-code-actions
    "rq" 'eglot-code-action-quickfix
    "rn" 'eglot-rename
    "rf" 'eglot-format
    "x" 'emamux:run-last-command
    "X" 'emamux:send-command
    )

  (my-leader-def
    :states 'visual
    :keymaps 'override
    "x" 'emamux:send-region)

  (winner-mode 1)
  (general-define-key
   :states 'normal
   "-" 'dired-jump
   "gf" 'auray/project-guess-file
   "gr" 'er/expand-region
   "[ [" 'previous-buffer
   "] ]" 'next-buffer
   "[ q" 'previous-error
   "] q" 'next-error
   "]w" 'winner-redo
   "[w" 'winner-undo
   )

  (general-define-key
   :states 'insert
   "s-/" 'hippie-expand
   "M-/" 'hippie-expand)

  (general-define-key
   :keymaps 'override

   "s-t" 'project-find-file

   "<f5>" 'ispell-buffer
   "<f6>" 'iedit-mode
   "<f7>" 'org-tree-slide-mode
   "S-<f7>" 'org-tree-slide-skip-done-toggle
   ;; Hydra on F8
   "<f9>" 'deft

   "M-." 'xref-find-definitions
   "M-c" 'kill-ring-save ; ⌘-c = Copy
   "M-v" 'yank ; ⌘-v = Paste
   "M-x" 'counsel-M-x
   "C-=" 'er/expand-region
   "C-+" 'default-text-scale-increase
   "C-M-+" 'default-text-scale-decrease

   "C-h" 'tmux-move-left
   "C-j" 'tmux-move-down
   "C-l" 'tmux-move-right
   "C-k" 'tmux-move-up

   "C-r" 'isearch-backward
   "C-s" 'swiper

   "C-c C-m" 'execute-extended-command ; Another =M-x= without leaving the home row

   "C-c 9" 'paredit-backward-slurp-sexp
   "C-c 0" 'paredit-forward-slurp-sexp
   "C-c [" 'paredit-backward-barf-sexp
   "C-c ]" 'paredit-forward-barf-sexp
   "C-c a" 'org-agenda
   ;; C-c C-c "runs" what makes sense for a particular mode
   "C-c gg" 'magit-status
   "C-c gl" 'git-link
   "C-c gt" 'git-timemachine-toggle
   "C-c jc" 'org-clock-jump-to-current-clock
   "C-c je" (lambda () (interactive) (find-file "~/.emacs.d/init.org"))
   "C-c jg" (lambda () (interactive) (find-file "~/Dropbox/notes/gtd.org"))
   "C-c jp" (lambda () (interactive) (find-file "~/projects/"))
   "C-c jw" (lambda () (interactive) (find-file "~/work"))
   "C-c jj" 'dired-jump
   "C-c k" 'recompile
   "C-c K" 'compile
   "C-c l" 'org-store-link

   "C-c of" 'auto-fill-mode
   "C-c og" 'global-hl-line-mode
   "C-c oi" 'electric-indent-mode
   "C-c olh" 'hide-line-numbers
   "C-c oll" 'show-line-numbers
   "C-c olr" 'show-relative-line-numbers
   "C-c op" 'show-paren-mode
   "C-c or" 'rainbow-mode
   "C-c ot" 'toggle-truncate-lines
   "C-c ow" 'visual-line-mode

   "C-c p" 'project-find-file
   "C-c q" 'speedbar-get-focus
   "C-c R" 'revert-buffer
   "C-c w r" 'windresize

   "C-x C-m" 'counsel-M-x ; Another =M-x= without leaving the home row
   "C-x C-o" 'company-complete
   "C-x b" 'switch-to-buffer
   "C-x C-f" 'counsel-find-file
   "C-x B" 'project-switch-to-buffer
   "C-x m" 'execute-extended-command ; Another =M-x= without leaving the home row
   "C-x o" 'other-window)
  )

(use-package hydra
  :config
  (defhydra hydra-utils (global-map "<f8>")
    "drag"
    ("j" drag-stuff-down "down")
    ("k" drag-stuff-up "up")))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :config
  (drag-stuff-global-mode t))

(setq org-directory "~/Dropbox/org")

(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; (require 'org-habit)
;; (add-to-list 'org-modules "org-habit")
;; (add-to-list 'org-modules "org-git-link")
(setq org-log-into-drawer t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(canceled@)")))

(use-package deft
  :commands (deft)
  :init
  (setq deft-extensions '("org" "md")
        deft-recursive t
        deft-directory "~/Dropbox/notes/"))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package palimpsest
  :diminish palimpsest-mode
  :config
  (add-hook 'org-mode-hook 'palimpsest-mode))

(setq org-capture-templates
      '(("n" "Notes" entry (file+headline "~/Dropbox/org/inbox.org" "Inbox") "* %?\n")
        ("t" "todo" entry (file+headline "~/Dropbox/org/inbox.org" "Inbox")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))

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

(setq
 time-stamp-active t
 time-stamp-line-limit 30     ; check first 30 buffer lines for Time-stamp:
 time-stamp-format "%04y-%02m-%02d") ;

(use-package writeroom-mode
  :bind (("C-c w w" . writeroom-mode)))

(use-package htmlize) ; for org html export
(setq system-time-locale "C") ; make sure time local is in english when exporting
(setq org-html-validation-link nil)
(setq org-publish-project-alist
      `(
        ("blog-files"
         :base-directory "~/project/blog"
         :base-extension "org"
         :publishing-directory "~/project/blog_published"
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

(use-package writegood-mode)

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
    (xwidget-webkit-browse-url (concat "http://en.wikipedia.org/wiki/" word))
    ;; (eww myUrl) ; emacs's own browser
    ))

(require 'auray/find-in-project)

(use-package iedit)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
(setq ediff-split-window-function 'split-window-vertically)

(use-package git-link)

(use-package fullframe
  :config
  (fullframe vc-annotate quit-window))

(use-package magit
  :init
  (setq magit-commit-show-diff nil
        magit-auto-revert-mode nil
        magit-commit-show-diff nil))

(setq auth-sources '("~/.authinfo"))

(use-package forge
  :after magit)

(use-package fullframe
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window))

(require 'project)

(use-package dumb-jump
  :init
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(setq speedbar-directory-unshown-regexp "^$")

(setq project-switch-commands 'project-dired)

(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'js2-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-from-kill try-expand-all-abbrevs try-expand-list try-complete-file-name))
(require 'mode-local)
(setq-mode-local elisp-mode hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-from-kill try-expand-list try-complete-lisp-symbol-partially try-complete-lisp-symbol try-complete-file-name))

(use-package company
  :demand t
  :diminish company-mode
  :config
  (setq company-idle-delay nil
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

  (use-package company-statistics
    :after company
    :config
    (setq company-statistics-file "~/.emacs.d/company-stats-cache.el")
    (company-statistics-mode +1))

  (autoload 'company-capf "company-capf")
  (autoload 'company-yasnippet "company-yasnippet")
  (autoload 'company-elisp "company-elisp")
  (autoload 'company-files "company-files"))

(use-package yasnippet
  :defer 3
  :commands yas-expand-snippet
  :bind (("C-c y" . yas-insert-snippet))
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
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

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
     ))

(use-package dired-rsync
  :bind (:map dired-mode-map ("p" . dired-rsync)))

(use-package restclient
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(defun abott/org-tree-slide-play ()
  (writeroom-mode 1)
  (default-text-scale-increment 40))
(defun abott/org-tree-slide-stop ()
  (writeroom-mode -1)
  (default-text-scale-reset))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . abott/org-tree-slide-play)
         (org-tree-slide-stop . abott/org-tree-slide-stop))
  :config
  (with-eval-after-load "org-tree-slide"
    (define-key org-tree-slide-mode-map (kbd "<f8>") 'org-tree-slide-move-previous-tree)
    (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-next-tree)))

(use-package ox-reveal
  :config
  (setq org-reveal-root "file:///Users/auray/.emacs.d/site-lisp/reveal.js-4.1.0"))

(pixel-scroll-precision-mode)

(use-package deadgrep)

(use-package paredit-everywhere
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(use-package emamux
  :commands (emamux:run-last-command emamux:send-command emamux:send-region)
  :init
  (setq emamux:use-nearest-pane 1))

(setq visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)

(desktop-save-mode -1)

(use-package evil
  :init (setq evil-want-C-i-jump nil)
  :config
  (define-key evil-normal-state-map (kbd "C-r") 'isearch-backward)
  (define-key evil-normal-state-map (kbd "C-n") 'next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'previous-line)
  (define-key evil-normal-state-map (kbd "M-,") 'xref-pop-marker-stack)
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
  (evil-mode 1))

(use-package evil
  :init
  :config
  (defun my-evil-record-macro ()
    (interactive)
    (if buffer-read-only
        (quit-window)
      (call-interactively 'evil-record-macro)))

  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map (kbd "q") 'my-evil-record-macro)))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil
  :config
  (evil-set-initial-state 'deadgrep-mode 'emacs)
  (evil-set-initial-state 'rg-mode 'emacs)
  (evil-set-initial-state 'deft-mode 'insert)
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'use-package-statistics 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'ert-results-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'ivy-occur-mode 'emacs)

  ;; magit commit
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode))

(use-package evil-visualstar
  :after evil
  :config
  (evil-define-key nil evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (evil-define-key nil evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (global-evil-visualstar-mode t))

(use-package evil-matchit
  :defer 2
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil
  :init (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "W" 'save-buffer))

(use-package evil
  :config
  (setq evil-want-C-i-jump nil)
  (evil-define-key 'insert lisp-interaction-mode-map (kbd "C-c C-c") 'eval-print-last-sexp))

(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state))

(use-package haskell-mode)

(use-package rg)

(use-package sqlite3)

(use-package exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(use-package counsel
  :diminish counsel-mode ivy-mode
  :config
  (ivy-mode t)
  (counsel-mode t))

(require 'tramp)
(add-to-list 'tramp-remote-path "~/.local/share/npm/bin/")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(use-package graphql-mode)

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package highlight-blocks)
;; (add-hook 'prog-mode-hook 'highlight-blocks-mode)
