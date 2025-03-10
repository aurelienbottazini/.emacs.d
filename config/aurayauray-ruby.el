
(use-package ruby-ts-mode
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
  (define-key ruby-ts-mode-map (kbd "C-c .") 'robe-jump)
  (define-key ruby-ts-mode-map (kbd "C-c C-c") 'xmp)
  (add-hook 'ruby-ts-mode-hook (defun auray-ruby-ts-mode-hook ()
             (modify-syntax-entry ?_ "w")       ; now '_' is not considered a word-delimiter
             )))

(use-package robe
  :after evil
  :diminish robe-mode
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-ts-mode-hook 'robe-mode)
  (eval-after-load 'company
  '(push 'company-robe company-backends))
  (evil-define-key 'normal ruby-ts-mode-map (kbd "gd") 'robe-jump)
  )

(use-package ruby-mode
  :config
  ;; (add-hook 'ruby-mode-hook 'subword-mode)

  (define-key ruby-mode-map (kbd "C-c C-c") 'xmp)
  (define-key ruby-mode-map (kbd "C-c r") 'rspec-rerun)
  (use-package ruby-interpolation
    :diminish ruby-interpolation-mode)
  (use-package ruby-end
    :diminish ruby-end-mode
    )
  (use-package rspec-mode
    :config
    (add-hook 'after-init-hook 'inf-ruby-switch-setup) ;When you've hit the breakpoint, hit C-x C-q to enable inf-ruby
  (define-key rspec-mode-map (kbd "C-c r") 'rspec-rerun)
    ))

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

(provide 'aurayauray-ruby)
