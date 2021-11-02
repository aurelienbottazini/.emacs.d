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

(use-package zenburn-theme
 :config
 (setq auray/default-color '("#2b2b2b" "#8fb28f" . "#f0dfaf"))
 (load-theme 'zenburn t))

(use-package evil
  :init
  (setq evil-respect-visual-line-mode t)
  :config
  (setq evil-insert-state-cursor '(bar "#97d88a")
        evil-visual-state-cursor '(box "#adcff1")
        evil-emacs-state-cursor '(box "#ffa2cb")
        evil-normal-state-cursor '(box "#f0dfaf")))

(add-hook 'post-command-hook '(lambda ()
  (let* (
         (color (cond ((minibufferp) auray/default-color)
                      ((evil-emacs-state-p)  '("#4c7073" "#dcdccc" . "#f0dfaf"))
                      ((evil-visual-state-p) '("#adcff1" "#4c4e56" . "#4c4e56"))
                      ((evil-insert-state-p)  '("#97d88a" "#4c4e56" . "#4c4e56"))
                      (t auray/default-color)))
         )
    (set-face-attribute 'mode-line nil :box `(:line-width 2 :color ,(car color)))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line-buffer-id (cddr color))
    (set-face-foreground 'mode-line (cadr color)))))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(if (file-exists-p "~/.emacs.d/.emacs-local")
  (load "~/.emacs.d/.emacs-local"))

(let ((default-directory  "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

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

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

(setq vc-follow-symlinks t)
(put 'magit-edit-line-commit 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq tags-add-tables 'nil) ; always start a new TAGS table don't ask the user

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

(global-so-long-mode 1) ;; helps when visiting files with long lines

;; only support left to right languages.
;; this makes long lines in files not slow anymore.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq help-window-select t ; if an help window appears, give it focus
      inhibit-startup-message t
      default-indicate-empty-lines nil ; show end of buffer on left fringe
      tab-always-indent 'complete ; try to indent first, if already indented try to complete
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

(set-default 'truncate-lines nil) ; when true gives each line only one visual line and don't show a continuation on next line
(global-visual-line-mode)

(setq sentence-end-double-space nil)

(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

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

(defun abo-change-line-endings-to-unix ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

(if (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode t))

(blink-cursor-mode 0)
(column-number-mode) ; column number in the mode line

(electric-indent-mode t)
(global-set-key (kbd "C-c oi") 'electric-indent-mode)

(electric-pair-mode t)
(defun inhibit-electric-pair-mode-in-minibuffer (char)
  (minibufferp))
(setq electric-pair-inhibit-predicate #'inhibit-electric-pair-mode-in-minibuffer)

(setq frame-title-format "emacs")

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
  (diminish 'eldoc-mode))

(setq blink-matching-paren 'jump-offscreen)
(show-paren-mode 1)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package default-text-scale
  :config
  :bind (("C-=" . 'default-text-scale-reset)
         ("C-+" . 'default-text-scale-increase)
         ("C-M-+" . 'default-text-scale-decrease)))

(setq default-frame-alist '((font . "Operator Mono AB-14")))

(require 're-builder)
(setq reb-re-syntax 'string)

(setq org-refile-targets '((nil :maxlevel . 3)
                                (org-agenda-files :maxlevel . 3)))
(advice-add 'org-refile :after
        (lambda (&rest _)
        (org-save-all-org-buffers)))

(use-package evil
  :init
  (setq org-use-speed-commands nil) ; they don't work well with Evil.
  :config
  (evil-define-key 'normal org-mode-map
    (kbd "M-l") 'org-shiftmetaright
    (kbd "M-h") 'org-shiftmetaleft
    (kbd "M-k") 'org-move-subtree-up
    (kbd "M-j") 'org-move-subtree-down
    ;; (kbd "M-p") 'org-publish-current-project
    (kbd "TAB") 'org-cycle)
  )

(defun my-prog-mode-auto-fill-hook ()
  (setq fill-column 100)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))
(add-hook 'prog-mode-hook 'my-prog-mode-auto-fill-hook)

;; First install the package:
(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :mode "\\.clj\\'"
  :config
  (require 'flycheck-clj-kondo)
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package cider
  :after clojure-mode
  :config

  (setq cider-repl-display-help-banner nil)
  )

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
  :diminish emmet-mode
  :config
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
  (define-key js2-mode-map (kbd "M-.") 'xref-find-definitions)
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))
  (add-hook 'js2-mode-hook (lambda() (subword-mode t)))

(use-package json-mode
  :mode "\\.json\\'"
  :mode "\\.eslintrc\\'")

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :config
  (use-package highlight-indentation)
  (add-hook 'coffee-mode-hook (lambda () (highlight-indentation-mode)))
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
        prettier-js-command "prettier")
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file errors (not warning)" (buffer-file-name))
  (shell-command (concat "eslint --quiet --fix " (buffer-file-name))))
(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))
(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'eslint-fix-file-and-revert nil 'make-it-local)))

(use-package context-coloring
  :ensure t
  :diminish context-coloring-mode
  :bind (("C-c oc" . context-coloring-mode))
  :config
  (add-hook 'js2-mode-hook 'context-coloring-mode))

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

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)

(use-package flycheck
  :diminish flycheck-mode
  :init
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'cfn-mode-hook 'flycheck-mode)
  (add-hook 'ruby-mode-hook 'flycheck-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (defun my/use-eslint-from-node-modules ()
    "Find eslint in the closest node-modules folder"
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

(use-package highlight-indentation
:config
(add-hook 'yaml-mode-hook (lambda () (highlight-indentation-mode))))

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

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; makes grep buffers writable and apply the changes to files.
(use-package wgrep :defer t)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(use-package paredit
   :diminish paredit-mode
   :config
   (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

 (use-package expand-region
   :bind (("M-e" . er/expand-region)))

 (global-set-key (kbd "C-c a") 'org-agenda)
 (global-set-key (kbd "C-c R") 'revert-buffer)
 (global-set-key (kbd "C-c jc") 'org-clock-jump-to-current-clock)
 (global-set-key (kbd "C-c jg") (lambda () (interactive) (find-file "~/Dropbox/org/gtd.org")))
 (global-set-key (kbd "C-c je") (lambda () (interactive) (find-file "~/.emacs.d/init.org")))
 (global-set-key (kbd "C-c jp") (lambda () (interactive) (find-file "~/projects/")))
 (global-set-key (kbd "C-c jw") (lambda () (interactive) (find-file "~/work")))
 (global-set-key (kbd "C-c ji") (lambda () (interactive) (find-file (concat **local-dropbox-folder** "/org/inbox.org"))))
 (global-set-key (kbd "C-c jr") (lambda () (interactive) (find-file (concat **local-dropbox-folder** "org/references-notes"))))
 (global-set-key (kbd "C-c jj") 'dired-jump)
 (global-set-key (kbd "C-c k") 'recompile)
 (global-set-key (kbd "C-c K") 'compile)

 (global-set-key (kbd "<f5>") 'ispell-buffer)
 (global-set-key (kbd "C-c h") 'highlight-symbol-at-point)
 (global-set-key (kbd "C-c H") 'unhighlight-regexp)

 (global-display-line-numbers-mode)
 (defun show-line-numbers ()
   (interactive)
   (setq display-line-numbers (quote absolute)))
 (global-set-key (kbd "C-c oll") 'show-line-numbers)
 (defun hide-line-numbers ()
   (interactive)
   (setq display-line-numbers (quote nil)))
 (global-set-key (kbd "C-c olh") 'hide-line-numbers)

 (global-set-key (kbd "C-c ow") 'visual-line-mode)
 (global-set-key (kbd "C-c of") 'auto-fill-mode)
 (global-set-key (kbd "C-c og") 'global-hl-line-mode)
 (global-set-key (kbd "C-c op") 'show-paren-mode)

 (use-package rainbow-mode
   :diminish rainbow-mode
   :bind (("C-c or" . rainbow-mode)))

(global-set-key (kbd "C-c ot") 'toggle-truncate-lines)

 (use-package windresize
   :bind (("C-c o h" . windresize)))

(use-package hydra
  :config
  (defhydra hydra-utils (global-map "<f8>")
    "drag"
    ("j" drag-stuff-down "down")
    ("k" drag-stuff-up "up")))

(use-package ivy-hydra)

(use-package drag-stuff
  :diminish t
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
 :bind (("<f9>" . deft))
 :commands (deft)
 :init
 (setq deft-extensions '("org" "md")
       deft-recursive t
       deft-directory (concat **local-dropbox-folder** "org/")))

(use-package markdown-mode
 :mode "\\.md\\'")

(global-set-key "\C-cl" 'org-store-link)

(use-package palimpsest
  :diminish palimpsest-mode
  :config
  (add-hook 'org-mode-hook 'palimpsest-mode))

(setq org-capture-templates
      '(("n" "Notes" entry (file+headline "~/Dropbox/org/inbox.org" "Inbox") "* %?\n")
        ("t" "todo" entry (file+headline "~/Dropbox/org/inbox.org" "Inbox")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))

(global-set-key (kbd "C-c n n") (lambda () (interactive) (org-capture nil "n")))

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
  :bind (("C-c w w" . writeroom-mode)))

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
    (browse-url (concat "http://en.wikipedia.org/wiki/" word))
    ;; (eww myUrl) ; emacs's own browser
    ))

(use-package counsel
  :bind (("C-c f" . counsel-rg)))

(defun auray/project-guess-file ()
  "Find file using current word as a guess"
  (interactive)
  (let* ((pr (project-current t))
         (dirs (list (project-root pr))))
  (counsel-fzf (current-word) (project-root (project-current t)))))

(setq counsel-fzf-cmd "fd --type f | fzf -f \"%s\"")

(evil-define-key nil evil-normal-state-map (kbd "gf") 'auray/project-guess-file)


(defun auray/project-find-file ()
  "Visit a file (with completion) in the current project."
  (interactive)
  (let* ((pr (project-current t))
         (dirs (list (project-root pr))))
  (counsel-fzf nil (project-root (project-current t)))))

(require 'auray/find-in-project)
(global-set-key (kbd "C-c s") 'auray/find-file-with-similar-name)

(use-package rg)

(use-package iedit
:bind (("C-c i" . iedit-mode)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
(setq ediff-split-window-function 'split-window-vertically)

(use-package git-link :bind (("C-c gl" . git-link)))

(use-package git-timemachine
  :bind (("C-c gt" . git-timemachine-toggle)))

(use-package fullframe
  :config
  (fullframe vc-annotate quit-window))

(use-package magit
  :bind (("C-c gs" . magit-status)
         ("C-c gc" . magit-commit)
         ("C-c gp" . magit-push-current)
         ("C-c gf" . magit-file-dispatch))
  :init
  (setq magit-commit-show-diff nil
        magit-auto-revert-mode nil
        magit-commit-show-diff nil))

(use-package fullframe
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window))

(require 'project)

(global-set-key (kbd "M-.") 'xref-find-definitions)
(use-package dumb-jump
  :init
  (setq dumb-jump-selector 'ivy)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(setq speedbar-directory-unshown-regexp "^$")
(global-set-key (kbd "C-c b") 'speedbar-get-focus)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-from-kill try-expand-all-abbrevs try-expand-list))
(require 'mode-local)
(setq-mode-local elisp-mode hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-from-kill try-expand-list try-complete-lisp-symbol-partially try-complete-lisp-symbol))
(setq-mode-local elisp-mode hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-from-kill try-expand-all-abbrevs try-complete-lisp-symbol-partially try-complete-lisp-symbol))

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

(use-package company-box
  :hook (company-mode . company-box-mode))

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
     (define-key dired-mode-map "-" 'dired-up-directory)))

(use-package dired-rsync
:bind (:map dired-mode-map ("p" . dired-rsync)))

(use-package engine-mode
  :bind (("C-c d c" . engine/search-caniuse)
         ("C-c d m" . engine/search-mdn)
         ("C-c d ra" . engine/search-rails)
         ("C-c d rr" . engine/search-ruby))
  :config
  (defengine ruby "https://apidock.com/ruby/search?query=%s")
  (defengine rails "https://api.rubyonrails.org/?q=%s")
  (defengine mdn "https://developer.mozilla.org/en-US/search?q=%s")
  (defengine caniuse "https://caniuse.com/#search=%s")
  )

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
  :bind (("<f7>" . org-tree-slide-mode)
         ("S-<f7>" . org-tree-slide-skip-done-toggle))
  :config
  (with-eval-after-load "org-tree-slide"
    (define-key org-tree-slide-mode-map (kbd "<f8>") 'org-tree-slide-move-previous-tree)
    (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-next-tree)))

(use-package ox-reveal
  :config
  (setq org-reveal-root "file:///Users/auray/.emacs.d/site-lisp/reveal.js-4.1.0"))

(use-package docker-tramp)

(use-package ivy
:bind (:map ivy-minibuffer-map
               ("C-c C-c" . ivy-restrict-to-matches)))
:init
(setq ivy-display-style 'fancy)
(setq ivy-use-selectable-prompt t)
(setq ivy-use-virtual-buffers t) ; enable bookmarks and recent-f
(setq ivy-initial-inputs-alist nil)
(setq ivy-re-builders-alist
  '((t      . ivy--regex-plus)))
(setq counsel-grep-base-command
 "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
:config
(ivy-mode)
(use-package counsel)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c b") 'counsel-bookmark)

(global-set-key (kbd "C-c v") 'ivy-switch-view)
(global-set-key (kbd "C-c V") 'ivy-push-view)
(global-set-key (kbd "C-c r") 'counsel-recentf)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


(use-package evil
:config
  (evil-set-initial-state 'ivy-occur-grep-mode 'emacs))

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

(global-set-key (kbd "C-h") 'tmux-move-left)

(global-set-key (kbd "C-j") 'tmux-move-down)
(define-key org-mode-map (kbd "C-j") 'tmux-move-down)

(global-set-key (kbd "C-k") 'tmux-move-up)
(global-set-key (kbd "C-l") 'tmux-move-right)
(use-package evil-commentary
:config
(evil-commentary-mode +1))

(use-package evil-visualstar
:config
(global-evil-visualstar-mode +1))

(use-package evil
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
  (evil-set-initial-state 'deft-mode 'insert)
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'use-package-statistics 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'ert-results-mode 'emacs))

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
  :bind  (("C-c oh" . (lambda ()
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

(use-package evil
  :config
  (setq evil-want-C-i-jump nil)
  (evil-define-key 'insert lisp-interaction-mode-map (kbd "C-c C-c") 'eval-print-last-sexp))

(use-package key-chord
  :defer 2
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state))

(use-package ace-window
:config
(global-set-key (kbd "C-x o") 'ace-window))

(setq project-switch-commands 'project-dired)

(global-set-key (kbd "C-c t") 'project-find-file)
(global-set-key (kbd "M-p") 'project-find-file)

(use-package el-patch)
(el-patch-defun project--files-in-directory (dir ignores &optional files)
  (el-patch-remove
    (require 'find-dired)
    (require 'xref)
    (defvar find-name-arg))
  (let* ((default-directory dir)
         ;; Make sure ~/ etc. in local directory name is
         ;; expanded and not left for the shell command
         ;; to interpret.
         (localdir (file-local-name (expand-file-name dir)))
         (command (el-patch-swap
                    (format "%s %s %s -type f %s -print0"
                            find-program
                            localdir
                            (xref--find-ignores-arguments ignores localdir)
                            (if files
                                (concat (shell-quote-argument "(")
                                        " " find-name-arg " "
                                        (mapconcat
                                         #'shell-quote-argument
                                         (split-string files)
                                         (concat " -o " find-name-arg " "))
                                        " "
                                        (shell-quote-argument ")"))
                              ""))
                    (format "fd -t f -0 . %s" localdir))))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))
