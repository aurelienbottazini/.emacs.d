(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(blink-matching-paren 'jump-offscreen)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(company-dabbrev-char-regexp "\\sw\\|\\s_")
 '(company-dabbrev-ignore-case nil t)
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(connection-local-criteria-alist
   '(((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(context-coloring-syntactic-strings nil)
 '(counsel-rg-base-command
   '("rg" "--hidden" "--max-columns" "240" "--with-filename" "--no-heading" "--line-number" "--color" "never" "%s"))
 '(coverlay:tested-line-background-color "#2e3600")
 '(coverlay:untested-line-background-color "#8a1f1d")
 '(custom-safe-themes
   '("d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "041bbb514ca9cbdc22fe3c50a65273dfcbb25cf75222d9a829742a2e2c0de584" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "f2c35f8562f6a1e5b3f4c543d5ff8f24100fae1da29aeb1864bbc17758f52b70" "aae95bbe93015b723d94b7081fdb27610d393c2156e2cda2e43a1ea7624c9e6f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "0809c08440b51a39c77ec5529f89af83ab256a9d48107b088d40098ce322c7d8" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default))
 '(deft-new-file-format "%Y-%m-%dT%H:%M")
 '(deft-use-filename-as-title t)
 '(deft-use-filter-string-for-filename t)
 '(delete-trailing-lines nil)
 '(desktop-files-not-to-save nil)
 '(display-line-numbers nil)
 '(display-line-numbers-current-absolute t)
 '(display-line-numbers-grow-only t)
 '(display-line-numbers-type 'visual)
 '(doom-modeline-buffer-encoding t)
 '(doom-modeline-buffer-modification-icon t)
 '(doom-modeline-buffer-state-icon t)
 '(doom-modeline-checker-simple-format t)
 '(dumb-jump-force-searcher 'ag)
 '(dumb-jump-mode t)
 '(electric-pair-mode nil)
 '(electric-quote-string t)
 '(emamux:use-nearest-pane t t)
 '(engine/browser-function 'eww)
 '(evil-mode-line-format nil)
 '(evil-toggle-key "C-c e")
 '(evil-undo-system 'undo-redo)
 '(fci-rule-color "#383838")
 '(flycheck-standard-error-navigation nil)
 '(frame-background-mode 'light)
 '(fzf/args "-x --color pointer:0,fg+:0,bg+:3,hl+:1,hl:1 --print-query")
 '(git-link-use-commit t)
 '(global-evil-search-highlight-persist t)
 '(hlt-auto-face-foreground "black")
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines nil)
 '(ispell-highlight-face 'flyspell-incorrect)
 '(ispell-program-name "/usr/bin/hunspell")
 '(ivy-posframe-border-width 5)
 '(ivy-posframe-mode t)
 '(ivy-preferred-re-builders
   '((ivy--regex-plus . "ivy")
     (ivy--regex-ignore-order . "order")
     (ivy--regex-fuzzy . "fuzzy")))
 '(jka-compr-shell "/bin/sh")
 '(js2-highlight-level 3)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-headerline-breadcrumb-enable-diagnostics nil)
 '(lsp-headerline-breadcrumb-segments '(path-up-to-project file))
 '(lsp-prefer-flymake :none)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-show-with-cursor t)
 '(lsp-ui-doc-use-childframe nil)
 '(lsp-ui-doc-use-webkit nil)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-show-code-actions nil)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-agenda-files '("~/Dropbox/org/gtd.org"))
 '(org-bullets-bullet-list '("⭐" "◾" "◽"))
 '(org-html-doctype "html5")
 '(org-html-head-include-default-style nil)
 '(org-html-head-include-scripts nil)
 '(org-html-html5-fancy t)
 '(org-html-indent t)
 '(org-html-validation-link nil)
 '(org-log-into-drawer t)
 '(org-ref-bib-html "<h1 class='org-ref-bib-h1'>References</h1>\12")
 '(org-startup-folded nil)
 '(org-startup-indented t)
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(sqlite3 multi-vterm treesit-auto swiper elm-mode vterm clj-refactor company-quickhelp-terminal dap-mode lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode projectile zenburn-theme emamux combobulate tree-sitter-langs tree-sitter treesitter-langs treesitter gruvbox-theme vertico deadgrep context-coloring-mode idle-highlight-mode idle-highlight ivy-posframe general winum doom-modeline doom-themes treemacs-all-the-icons treemacs-magit treemacs-icons-dired treemacs-evil org-superstar ace-window docker-tramp counsel-fzf el-patch el-path evil-indent-plus evil-search-highlight-persist evil-matchit evil-surround org-roam evil-visualstar evil-visual-star evil-commentary counsel guru-mode evil embark-consult embark marginalia consult selectrum-prescient selectrum ivy-hydra ivy find-file-in-project ox-reveal which-key project osx-clipboard evil-mode solarized-theme flycheck-clj-kondo company-box org-reveal org-tree-slide ob-graphql rainbow-delimiters org-msg default-text-scale dracula-theme origami-mode origame-mode evil-leader smart-mode-line restclient engine-mode ruby zotxt zotxt-emacs eglot treemacs leuven prettier company-statistics writeroom-mode go-mode ob-clojurescript writegood-mode writegood osx-dictionary all-the-icons-dired-mode all-the-icons-dired all-the-icons dired-rsync docker xref-js2 js2-refactor ruby-refactor parinfer ace-windows tide forge key-chord synonymous define-word htmlize esup inf-ruby ruby-additional counsel-etags rust-mode noflet lsp-css company-lsp lsp-javascript-typescript lsp-ruby js-comint skewer-mode nodejs-repl slime-js slime markdown-mode palimpsest-mode palimpsest palimpset-mode paredit 0xc evil-fringe-mark evil-fringe-marks enh-ruby-mode 0blayout org-bullets web-beautify graphql-mode haskell-mode flycheck drag-stuff col-highlight crosshairs rg eacl highlight-indentation evil-multiedit coverlay coverage coverage-mode evil-collection eyebrowse telephone-line magithub git-timemachine dash-at-point diminish yaml-mode lispyville x-clip xclip evil-mc multiple-cursors evil-lispy lispy windresize dumb-jump expand-region origami company-dict ac-js2 auto-complete indium less-css-mode sass-mode scss-mode dockerfile-mode emmet-mode yatemplate yasnippet company deft cider clojure-mode wgrep rainbow-mode rspec-mode ruby-end ruby-interpolation bundler paredit-everywhere hydra counsel-projectile iedit typescript-mode coffee-mode json-mode context-coloring prettier-js import-js js2-mode web-mode diff-hl fullframe evil-magit git-link magit highlight-parentheses evil-numbers exec-path-from-shell use-package))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(projectile-mode-line-prefix " Proj")
 '(projectile-switch-project-action 'projectile-dired)
 '(projectile-tags-command "git ls-files | /usr/local/bin/ctags -e -L -")
 '(pulse-delay 0.03)
 '(pulse-iterations 20)
 '(require-final-newline t)
 '(ruby-align-chained-calls t)
 '(ruby-align-to-stmt-keywords t)
 '(ruby-deep-arglist t)
 '(ruby-deep-indent-paren '(40 91 93 123 125))
 '(ruby-deep-indent-paren-style 'space)
 '(safe-local-variable-values
   '((eval progn
           (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-clojure-cli-global-options . "-A:dev")
     (cider-shadow-default-options . "web")
     (cider-default-cljs-repl . shadow)
     (coverlay:base-path . "/Users/auray/work/dox-errors-js/")
     (cider-shadow-cljs-default-options . "spa")
     (cider-shadow-cljs-default-options . "app")
     (org-reveal-title-slide . "<h1>Life beyond distributed Transactions</h1>")
     (org-reveal-title-slide . "<h1>Life beyong distributed Transactions</h1>")
     (org-reveal-title-slide . "<h1>Life Beyong Distributed Transactions</h1>")
     (org-reveal-title-slide . "<h1>Yo</h1>")
     (eval when
           (require 'rainbow-mode nil t)
           (rainbow-mode 1))
     (eval add-hook 'after-save-hook
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
 '(scroll-conservatively 1)
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
 '(tab-bar-close-button-show nil)
 '(tab-bar-history-mode t)
 '(tab-bar-tab-hints t)
 '(tool-bar-mode nil)
 '(treemacs-position 'right)
 '(truncate-lines nil)
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
 '(web-mode-code-indent-offset 0 t)
 '(web-mode-css-indent-offset 0 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Monolisa Custom"))))
 '(cider-debug-code-overlay-face ((t (:background "grey80" :foreground "black"))) t)
 '(evil-search-highlight-persist-highlight-face ((t (:background "#f8f893" :foreground "black"))))
 '(font-lock-comment-face ((t (:foreground "#7F9F7F" :slant italic))))
 '(hi-aquamarine ((t (:background "aquamarine" :foreground "black"))) t)
 '(hi-salmon ((t (:background "light salmon" :foreground "black"))) t)
 '(hlt-property-highlight ((t (:background "Wheat" :foreground "black"))) t)
 '(hlt-regexp-level-1 ((t (:background "#FA6CC847FFFF" :foreground "black"))) t)
 '(hlt-regexp-level-2 ((t (:background "#C847FFFFE423" :foreground "black"))) t)
 '(hlt-regexp-level-3 ((t (:background "#C847D8FEFFFF" :foreground "black"))) t)
 '(hlt-regexp-level-4 ((t (:background "#EF47FFFFC847" :foreground "black"))) t)
 '(hlt-regexp-level-5 ((t (:background "#FCFCE1E1FFFF" :foreground "black"))) t)
 '(hlt-regexp-level-6 ((t (:background "#E1E1FFFFF0F0" :foreground "black"))) t)
 '(hlt-regexp-level-7 ((t (:background "#E1E1EAEAFFFF" :foreground "black"))) t)
 '(hlt-regexp-level-8 ((t (:background "#F6F5FFFFE1E1" :foreground "black"))) t)
 '(icomplete-selected-match ((t (:underline t))))
 '(ivy-minibuffer-match-face-2 ((t (:background "#5F7F5F"))))
 '(ivy-minibuffer-match-face-3 ((t (:background "#7F9F7F" :foreground "black"))))
 '(ivy-minibuffer-match-face-4 ((t (:background "#8FB28F" :foreground "black"))))
 '(lsp-modeline-code-actions-face ((t (:inherit warning))) t)
 '(lsp-ui-doc-background ((t (:background "#2b2b2b"))) t)
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :slant italic :weight bold))))
 '(minibuffer-prompt ((t (:foreground "#F0DFAF" :height 1.0))))
 '(mode-line ((t (:background "#4c7073" :foreground "#dcdccc" :box (:line-width (2 . 2) :color "#4c7073") :height 1.0))))
 '(mode-line-buffer-id ((t (:foreground "#f0dfaf" :slant italic :weight bold))))
 '(mode-line-inactive ((t (:background "#383838" :foreground "#5F7F5F" :box (:line-width (2 . 2) :color "#383838" :style flat-button) :height 1.1))))
 '(org-block ((t (:extend t :background "#333333"))))
 '(org-code ((t (:inherit shadow))))
 '(org-document-info-keyword ((t (:inherit shadow :height 1.3))))
 '(org-document-title ((t (:inherit default :foreground "#8CD0D3" :weight bold :height 1.3))))
 '(org-drawer ((t (:foreground "#f0dfaf"))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :height 1.1))))
 '(org-level-3 ((t (:inherit default :extend nil :foreground "#7CB8BB" :slant italic :height 1.1))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :height 1.1))))
 '(region ((t (:extend t :background "#adcff1" :foreground "black"))))
 '(tab-bar ((t (:inherit nil :background "#88b090" :foreground "#2e3330" :slant italic :height 1.1))))
 '(tab-bar-tab ((t (:inherit tab-bar :background "#ccdc90" :foreground "#3f3f3f" :box (:line-width (3 . 3) :style pressed-button) :weight bold))))
 '(tab-bar-tab-group-current ((t (:inherit tab-bar-tab :background "#ccdc90"))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "#88b090" :foreground "#3f3f3f" :box (:line-width (3 . 3) :style released-button) :slant normal))))
 '(tab-line ((t (:inherit variable-pitch :background "#2c302d" :foreground "#dcdccc" :height 0.9))))
 '(tab-line-highlight ((t (:background "grey85" :foreground "black" :box (:line-width (1 . 1) :style released-button)))) t)
 '(tab-line-tab ((t (:inherit tab-line :box (:line-width (1 . 1) :style released-button)))) t)
 '(tab-line-tab-current ((t (:inherit tab-line-tab :background "#262626" :foreground "#dcdccc"))) t)
 '(tab-line-tab-inactive ((t (:inherit tab-line-tab))) t)
 '(tab-line-tab-modified ((t (:foreground "#e89393"))) t)
 '(web-mode-html-tag-bracket-face ((t (:foreground "#8f8f8f"))) t))
