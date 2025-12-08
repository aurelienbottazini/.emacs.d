(use-package multiple-cursors)
(use-package windresize)

(use-package ivy-hydra)

(defun find-file-right (filename)
  (interactive)
  (split-window-right)
  (other-window 1)
  (find-file filename))

(defun find-file-below (filename)
  (interactive)
  (split-window-below)
  (other-window 1)
  (find-file filename))

(ivy-set-actions
 t
 '(("|" find-file-right "open right")
   ("%" find-file-below "open below")))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))


(use-package general
  :config

  (general-create-definer my-leader-def
    :prefix "SPC")

  (my-leader-def
    :states 'normal
    :keymaps 'override
    "A" 'org-agenda
    "a" 'find-sibling-file
    "b" 'ibuffer
    "1" 'treemacs
    "c" (lambda () (interactive) (org-capture))
    "d" 'dired-jump
    "e" 'recentf
    "h" 'highlight-symbol-at-point
    "H" 'unhighlight-regexp
    "i" 'imenu
    "je" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    "jg" (lambda () (interactive) (find-file "~/Library/CloudStorage/Dropbox/notes/gtd.org"))
    "ji" (lambda () (interactive) (find-file "~/Library/CloudStorage/Dropbox/notes/inbox.org"))
    "jj" (lambda () (interactive) (find-file "~/Library/CloudStorage/Dropbox/notes/journal.org"))
    "jn" (lambda () (interactive) (find-file "~/Library/CloudStorage/Dropbox/notes/"))
    "jp" (lambda () (interactive) (find-file "~/projects/")gtd)
    "jw" (lambda () (interactive) (find-file "~/work"))
    "k" 'recompile
    "g" 'magit-status
    "G" 'magit-file-dispatch
    "p" 'project-switch-project
    "F" 'rg
    "f" 'consult-ripgrep
    "t" 'consult-fd
    ;; "t" (lambda () (interactive) (org-capture nil "t"))
    "w" 'er/expand-region
    "W" 'er/contract-region
    "x" 'emamux:run-last-command
    "X" 'emamux:send-command
    "SPC" 'abo-run
    )

  (defun abo-run ()
    (interactive)
    (cond
     ((derived-mode-p 'ruby-ts-mode) (xmp))
     ((derived-mode-p 'ruby-mode) 'xmp)
     (t (message "nothing to run for this buffer")

     )))

  (my-leader-def
    :states 'visual
    :keymaps 'override
    "x" 'emamux:send-region)

  (winner-mode 1)

  (general-define-key
   :states 'normal
   "-" 'dired-jump
   "[[" 'previous-buffer
   "]]" 'next-buffer
   "[e" 'flymake-goto-prev-error
   "]e" 'flymake-goto-next-error
    ":" 'evil-ex
    "-" 'dired-jump
   )

  (general-define-key
   :states 'insert
   "s-/" 'hippie-expand
   "M-/" 'hippie-expand)

  (general-define-key
   :keymaps 'override

   "s-t" 'counsel-fzf
   "M-t" 'counsel-fzf

   "<f5>" 'ispell-buffer
   "<f6>" 'iedit-mode
   "<f7>" 'org-tree-slide-mode
   "S-<f7>" 'org-tree-slide-skip-done-toggle
   ;; Hydra on F8
   "<f9>" 'deft

   "M-." 'xref-find-definitions
   "M-c" 'kill-ring-save ; ⌘-c = Copy
   "M-v" 'yank ; ⌘-v = Paste
   ;; "M-x" 'counsel-M-x

   "C-h" 'tmux-move-left
   "C-j" 'tmux-move-down
   "C-l" 'tmux-move-right
   "C-k" 'tmux-move-up

   "C-r" 'undo-redo
   "C-s" 'swiper

   "C-c C-m" 'execute-extended-command ; Another =M-x= without leaving the home row
   "C-c C-q" 'org-set-tags-command

   "C-c 9" 'paredit-backward-slurp-sexp
   "C-c 0" 'paredit-forward-slurp-sexp
   "C-c [" 'paredit-backward-barf-sexp
   "C-c ]" 'paredit-forward-barf-sexp
   "C-c a" 'org-agenda
   "C-c d" 'flymake-show-buffer-diagnostics
   "C-c e" 'er/expand-region
   ;; C-c C-c "runs" what makes sense for a particular mode
   "C-c gg" 'magit-status

   "C-c gg" 'magit-status
   "C-c gf" 'magit-file-dispatch
   "C-c gl" 'git-link
   "C-c gt" 'git-timemachine-toggle
   "C-c jc" 'org-clock-jump-to-current-clock
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
   "C-c s" 'find-sibling-file
   "C-c t" 'tab-switcher

   "C-c p" 'project-find-file
   "C-c r" 'recentf-open
   "C-c R" 'revert-buffer
   "C-c w r" 'windresize

   ;; "C-x C-m" 'counsel-M-x ; Another =M-x= without leaving the home row
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


(provide 'abo-keybindings)
