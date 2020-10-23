(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(ansi-term-color-vector
   [unspecified "black" "red" "green" "yellow" "blue" "brightmagenta" "cyan" "white"])
 '(blink-cursor-mode nil)
 '(blink-matching-paren 'jump-offscreen)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(company-dabbrev-char-regexp "\\sw\\|\\s_")
 '(company-dabbrev-ignore-case nil)
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   '("f2c35f8562f6a1e5b3f4c543d5ff8f24100fae1da29aeb1864bbc17758f52b70" "aae95bbe93015b723d94b7081fdb27610d393c2156e2cda2e43a1ea7624c9e6f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "0809c08440b51a39c77ec5529f89af83ab256a9d48107b088d40098ce322c7d8" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default))
 '(deft-new-file-format "%Y-%m-%dT%H:%M")
 '(deft-use-filename-as-title t)
 '(deft-use-filter-string-for-filename t)
 '(delete-trailing-lines nil)
 '(display-line-numbers nil)
 '(display-line-numbers-type 'visual)
 '(dumb-jump-force-searcher 'ag)
 '(dumb-jump-mode t)
 '(electric-pair-mode nil)
 '(electric-quote-string t)
 '(evil-undo-system 'undo-redo)
 '(fci-rule-color "#383838")
 '(flycheck-standard-error-navigation nil)
 '(frame-background-mode 'light)
 '(global-evil-search-highlight-persist t)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines nil)
 '(ispell-highlight-face 'flyspell-incorrect)
 '(ispell-program-name "/usr/bin/hunspell")
 '(ivy-preferred-re-builders
   '((ivy--regex-plus . "ivy")
     (ivy--regex-ignore-order . "order")
     (ivy--regex-fuzzy . "fuzzy")))
 '(js2-highlight-level 3)
 '(lsp-prefer-flymake :none)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-use-webkit nil)
 '(lsp-ui-sideline-enable nil)
 '(magit-auto-revert-mode t)
 '(magit-commit-show-diff nil)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-agenda-files '("~/Dropbox/org/refile-beorg.org" "~/Dropbox/org/GTD.org"))
 '(org-bullets-bullet-list '("⭐" "◾" "◽"))
 '(org-html-doctype "html5")
 '(org-html-head-include-default-style nil)
 '(org-html-head-include-scripts nil)
 '(org-html-html5-fancy t)
 '(org-html-indent t)
 '(org-html-validation-link nil)
 '(org-log-into-drawer t)
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m))
 '(org-ref-bib-html "<h1 class='org-ref-bib-h1'>References</h1>
")
 '(org-startup-folded nil)
 '(org-startup-indented t)
 '(package-selected-packages
   '(fzf zenburn-theme dracula-theme origami-mode origame-mode general evil-leader peep-dired smart-mode-line restclient engine-mode evil-easymotion ruby ivy-bibtex org-ref zotxt zotxt-emacs eglot treemacs leuven leuven-theme prettier company-statistics writeroom-mode go-mode org ob-clojurescript writegood-mode writegood osx-dictionary powerthesaurus all-the-icons-dired-mode all-the-icons-dired all-the-icons evil-iedit-state ivy dired-rsync docker xref-js2 posframe js2-refactor ruby-refactor parinfer ace-windows tide forge key-chord synonymous define-word htmlize esup inf-ruby ruby-additional counsel-etags rust-mode find-file-in-project noflet lsp-ui lsp-css company-lsp lsp-javascript-typescript lsp-mode lsp-ruby js-comint skewer-mode nodejs-repl slime-js slime markdown-mode palimpsest-mode palimpsest palimpset-mode paredit 0xc evil-fringe-mark evil-fringe-marks enh-ruby-mode 0blayout avy org-bullets web-beautify graphql-mode haskell-mode flycheck drag-stuff col-highlight crosshairs rg eacl highlight-indentation evil-multiedit coverlay coverage coverage-mode evil-collection eyebrowse telephone-line magithub git-timemachine dash-at-point diminish yaml-mode lispyville x-clip xclip evil-mc multiple-cursors evil-lispy lispy windresize dumb-jump expand-region origami company-dict ac-js2 auto-complete indium less-css-mode sass-mode scss-mode dockerfile-mode emmet-mode yatemplate yasnippet company deft cider clojure-mode which-key wgrep rainbow-mode rspec-mode ruby-end ruby-interpolation bundler paredit-everywhere ivy-hydra hydra counsel-projectile ace-window iedit emamux typescript-mode coffee-mode json-mode context-coloring prettier-js import-js js2-mode web-mode diff-hl fullframe evil-magit git-link magit highlight-parentheses projectile evil-search-highlight-persist evil-matchit evil-indent-plus evil-visualstar evil-commentary evil-surround evil-numbers evil exec-path-from-shell counsel use-package))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(projectile-tags-command "git ls-files | /usr/local/bin/ctags -e -L -")
 '(require-final-newline t)
 '(ruby-align-chained-calls t)
 '(ruby-align-to-stmt-keywords t)
 '(ruby-deep-arglist t)
 '(ruby-deep-indent-paren '(40 91 93 123 125))
 '(ruby-deep-indent-paren-style 'space)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle))
           nil t)
     (eval progn
           (require 'find-file-in-project)
           (setq ffip-prune-patterns
                 `("*/public/packs*/*" ,@ffip-prune-patterns))
           (setq prettier-js-command "/home/auray/work/doximity-client-vue/node_modules/.bin/prettier"))
     (compilation-scroll-output 'first-error)
     (compilation-scroll-output t)
     (ffip-project-root . "/home/auray/work/doximity-client-vue/")
     (eval progn
           (require 'find-file-in-project)
           (setq ffip-prune-patterns
                 `("vendor/*" ,@ffip-prune-patterns)))
     (ffip-project-root . "/home/aurelienbottazini/work/doximity-client-vue/")
     (alternative-files-rules
      ("src/\\(.*\\).vue" "test/unit/specs/\\1.spec.js")
      ("test/unit/specs/\\(.*\\).spec.js" "src/\\1.vue")
      ("src/\\(.*\\).js" "test/unit/specs/\\1.spec.js")
      ("test/unit/specs/\\(.*\\).spec.js" "src/\\1.js"))
     (eval add-hook 'after-save-hook
           (lambda nil
             (org-html-export-to-html t))
           t t)
     (eval progn
           (require 'find-file-in-project)
           (setq ffip-prune-patterns
                 `("*/public/packs*/*" ,@ffip-prune-patterns)))
     (ffip-find-options . "-not -size +64k -not -iwholename './target/*'")
     (ffip-find-options . "-not -size +64k -not -iwholename './target/debug/*'")
     (org-confirm-babel-evaluate)))
 '(scroll-bar-mode nil)
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(speedbar-default-position 'right)
 '(speedbar-frame-parameters
   '((minibuffer)
     (width . 100)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0)
     (height . 60)))
 '(speedbar-show-unknown-files t)
 '(swiper-goto-start-of-match t)
 '(tool-bar-mode nil)
 '(treemacs-position 'right)
 '(truncate-partial-width-windows nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(vc-follow-symlinks t)
 '(web-mode-code-indent-offset 0)
 '(web-mode-css-indent-offset 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro Mono" :foundry "FSD " :slant normal :weight normal :height 139 :width normal))))
 '(evil-search-highlight-persist-highlight-face ((t (:background "yellow1" :foreground "black")))))
