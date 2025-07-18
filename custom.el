(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(apheleia-mode-alist
   '((asm-mode . asmfmt) (awk-mode . gawk) (bash-ts-mode . shfmt) (bazel-mode . buildifier)
     (beancount-mode . bean-format) (c++-ts-mode . clang-format) (caddyfile-mode . caddyfmt)
     (cc-mode . clang-format) (c-mode . clang-format) (c-ts-mode . clang-format)
     (c++-mode . clang-format) (caml-mode . ocamlformat) (clojure-dart-ts-mode . cljfmt)
     (clojure-jank-ts-mode . cljfmt) (clojure-mode . cljfmt) (clojure-ts-mode . cljfmt)
     (clojurec-mode . cljfmt) (clojurec-ts-mode . cljfmt) (clojurescript-mode . cljfmt)
     (clojurescript-ts-mode . cljfmt) (cmake-mode . cmake-format) (cmake-ts-mode . cmake-format)
     (common-lisp-mode . lisp-indent) (conf-toml-mode . dprint) (cperl-mode . perltidy)
     (crystal-mode . crystal-tool-format) (css-mode . prettier-css) (css-ts-mode . prettier-css)
     (dart-mode . dart-format) (dart-ts-mode . dart-format) (dockerfile-mode . dprint)
     (elixir-mode . mix-format) (elixir-ts-mode . mix-format) (elm-mode . elm-format)
     (emacs-lisp-mode . lisp-indent) (fish-mode . fish-indent) (go-mode . gofmt)
     (go-ts-mode . gofmt) (graphql-mode . prettier-graphql) (haskell-mode . brittany)
     (hcl-mode . hclfmt) (html-mode . prettier-html) (html-ts-mode . prettier-html)
     (java-mode . google-java-format) (java-ts-mode . google-java-format) (jinja2-mode)
     (js3-mode . prettier-javascript) (js-json-mode . prettier-json) (js-mode . prettier-javascript)
     (js-ts-mode . prettier-javascript) (json-mode . prettier-json) (json-ts-mode . prettier-json)
     (kotlin-mode . ktlint) (kotlin-ts-mode . ktlint) (latex-mode . latexindent)
     (LaTeX-mode . latexindent) (lua-mode . stylua) (lua-ts-mode . stylua) (lisp-mode . lisp-indent)
     (nasm-mode . asmfmt) (nix-mode . nixfmt) (perl-mode . perltidy) (php-mode . phpcs)
     (purescript-mode . purs-tidy) (python-mode . black) (python-ts-mode . black)
     (robot-mode . robotidy) (rustic-mode . rustfmt) (rust-mode . rustfmt) (rust-ts-mode . rustfmt)
     (scss-mode . prettier-scss) (sql-mode . pgformatter) (svelte-mode . prettier-svelte)
     (terraform-mode . terraform) (TeX-latex-mode . latexindent) (TeX-mode . latexindent)
     (tsx-ts-mode . prettier-typescript) (tuareg-mode . ocamlformat)
     (typescript-mode . prettier-typescript) (typescript-ts-mode . prettier-typescript)
     (web-mode . prettier) (yaml-mode . prettier-yaml) (yaml-ts-mode . prettier-yaml)
     (yang-mode . pyang) (ruby-ts-mode . rubocop) (ruby-mode . rubocop)))
 '(blink-cursor-mode nil)
 '(blink-matching-paren 'jump-offscreen)
 '(cider-injected-middleware-version "0.37.1")
 '(citre-tags-file-names '(".tags" "tags"))
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(company-dabbrev-char-regexp "\\sw\\|\\s_")
 '(company-dabbrev-ignore-case nil t)
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(connection-local-criteria-alist
   '(((:application tramp :machine "MBP-725-ABOT") tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "MBA-725-ABOT") tramp-connection-local-darwin-ps-profile)
     ((:application tramp :protocol "flatpak") tramp-flatpak-connection-local-default-profile)
     ((:application tramp :protocol "kubernetes") tramp-kubernetes-connection-local-default-profile)
     ((:application tramp :machine "MacBook-Air-de-Aurelien.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application eshell) eshell-connection-default-profile)
     ((:application tramp :machine "auray-air.home") tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "auray-air.local") tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "localhost") tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "tagada-167.local") tramp-connection-local-darwin-ps-profile)
     ((:application tramp) tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-flatpak-connection-local-default-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin"
                         "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin"
                         "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
                         "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97 (tramp-kubernetes--container (car tramp-current-connection)) 104
                               (tramp-kubernetes--pod (car tramp-current-connection)) 120
                               (tramp-kubernetes--context-namespace (car tramp-current-connection))))
     (eshell-connection-default-profile (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number) (user . string)
                                          (egid . number) (comm . 52) (state . 5) (ppid . number)
                                          (pgrp . number) (sess . number) (ttname . string)
                                          (tpgid . number) (minflt . number) (majflt . number)
                                          (time . tramp-ps-time) (pri . number) (nice . number)
                                          (vsize . number) (rss . number) (etime . tramp-ps-time)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number) (user . string) (group . string)
                                          (comm . 52) (state . 5) (ppid . number) (pgrp . number)
                                          (ttname . string) (time . tramp-ps-time) (nice . number)
                                          (etime . tramp-ps-time) (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number) (user . string)
                                          (egid . number) (group . string) (comm . 52)
                                          (state . string) (ppid . number) (pgrp . number)
                                          (sess . number) (ttname . string) (tpgid . number)
                                          (minflt . number) (majflt . number) (time . tramp-ps-time)
                                          (pri . number) (nice . number) (vsize . number)
                                          (rss . number) (etime . number) (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile (shell-file-name . "/bin/sh")
                                                   (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile (path-separator . ":")
                                                    (null-device . "/dev/null"))))
 '(context-coloring-syntactic-strings nil)
 '(copilot-indent-warning-suppress t)
 '(counsel-rg-base-command
   '("rg" "--hidden" "--max-columns" "240" "--with-filename" "--no-heading" "--line-number" "--color"
     "never" "%s"))
 '(coverlay:tested-line-background-color "#2e3600")
 '(coverlay:untested-line-background-color "#8a1f1d")
 '(custom-safe-themes
   '("5a0ddbd75929d24f5ef34944d78789c6c3421aa943c15218bac791c199fc897d"
     "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d"
     "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3"
     "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7"
     "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633"
     "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c"
     "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "f74e8d46790f3e07fbb4a2c5dafe2ade0d8f5abc9c203cd1c29c7d5110a85230"
     "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f"
     "046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012"
     "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8"
     "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298"
     "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5"
     "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703"
     "041bbb514ca9cbdc22fe3c50a65273dfcbb25cf75222d9a829742a2e2c0de584"
     "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3"
     "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773"
     "f2c35f8562f6a1e5b3f4c543d5ff8f24100fae1da29aeb1864bbc17758f52b70"
     "aae95bbe93015b723d94b7081fdb27610d393c2156e2cda2e43a1ea7624c9e6f"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     "0809c08440b51a39c77ec5529f89af83ab256a9d48107b088d40098ce322c7d8"
     "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b"
     "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739"
     "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default))
 '(deft-new-file-format "%Y-%m-%dT%H:%M")
 '(deft-use-filename-as-title t)
 '(deft-use-filter-string-for-filename t)
 '(delete-trailing-lines nil)
 '(desktop-files-not-to-save nil)
 '(dired-guess-shell-alist-user '(("(\"\\\\.jpe?g\\\\'\" \"open\")")))
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
 '(eglot-confirm-server-edits nil)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-ignored-server-capabilities '(:hoverProvider))
 '(electric-pair-mode nil)
 '(electric-quote-string t)
 '(emamux:use-nearest-pane t t)
 '(enable-remote-dir-locals t)
 '(engine/browser-function 'eww)
 '(evil-mode-line-format nil)
 '(evil-toggle-key "C-c e")
 '(evil-undo-system 'undo-redo)
 '(fci-rule-color "#383838")
 '(flycheck-standard-error-navigation nil)
 '(flymake-mode-line-format nil)
 '(flymake-suppress-zero-counters :warning)
 '(frame-background-mode 'light)
 '(fzf/args "-x --color pointer:0,fg+:0,bg+:3,hl+:1,hl:1 --print-query")
 '(git-link-use-commit t)
 '(global-evil-search-highlight-persist nil)
 '(highlight-blocks-max-innermost-block-count 1)
 '(hlt-auto-face-foreground "black")
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines nil)
 '(ispell-highlight-face 'flyspell-incorrect)
 '(ispell-program-name "/usr/bin/hunspell")
 '(ivy-posframe-border-width 5)
 '(ivy-posframe-mode t)
 '(ivy-preferred-re-builders
   '((ivy--regex-plus . "ivy") (ivy--regex-ignore-order . "order") (ivy--regex-fuzzy . "fuzzy")))
 '(jka-compr-shell "/bin/sh")
 '(js2-highlight-level 3)
 '(lsp-auto-execute-action nil)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-headerline-breadcrumb-enable-diagnostics nil)
 '(lsp-headerline-breadcrumb-segments '(path-up-to-project file))
 '(lsp-modeline-code-action-fallback-icon "(A)")
 '(lsp-modeline-code-actions-segments '(count))
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
 '(ns-command-modifier 'meta)
 '(org-agenda-files
   '("~/Dropbox/notes/journal.org"
     "~/Dropbox/notes/gtd.org"))
 '(org-bullets-bullet-list '("⭐" "◾" "◽"))
 '(org-emphasis-alist
   '(("*" bold) ("/" italic) ("_" underline) ("=" lazy-highlight verbatim) ("~" org-code verbatim)
     ("+" (:strike-through t))))
 '(org-fold-core-style 'overlays)
 '(org-hide-emphasis-markers nil)
 '(org-html-doctype "html5")
 '(org-html-head-include-default-style nil)
 '(org-html-head-include-scripts nil)
 '(org-html-html5-fancy t)
 '(org-html-indent t)
 '(org-html-validation-link nil)
 '(org-image-actual-width '(400))
 '(org-log-into-drawer t)
 '(org-ref-bib-html "<h1 class='org-ref-bib-h1'>References</h1>\12")
 '(org-startup-folded nil)
 '(org-startup-indented t)
 '(org-startup-truncated t)
 '(package-selected-packages
   '(chatgpt flymake-kondor tramp robe edit-indirect highlight-blocks highlight-thing rainbow-blocks
             iyv-hydra sqlite3 multi-vterm treesit-auto swiper elm-mode vterm clj-refactor
             company-quickhelp-terminal dap-mode lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode
             projectile zenburn-theme emamux combobulate tree-sitter-langs tree-sitter
             treesitter-langs treesitter gruvbox-theme vertico deadgrep context-coloring-mode
             idle-highlight-mode idle-highlight ivy-posframe general winum doom-modeline doom-themes
             treemacs-all-the-icons treemacs-magit treemacs-icons-dired treemacs-evil org-superstar
             ace-window docker-tramp counsel-fzf el-patch el-path evil-indent-plus
             evil-search-highlight-persist evil-matchit evil-surround org-roam evil-visualstar
             evil-visual-star evil-commentary counsel guru-mode evil embark-consult embark
             marginalia consult selectrum-prescient selectrum ivy-hydra ivy find-file-in-project
             ox-reveal which-key project osx-clipboard evil-mode solarized-theme flycheck-clj-kondo
             company-box org-reveal org-tree-slide ob-graphql rainbow-delimiters org-msg
             default-text-scale dracula-theme origami-mode origame-mode evil-leader smart-mode-line
             restclient engine-mode ruby zotxt zotxt-emacs eglot treemacs leuven prettier
             company-statistics writeroom-mode go-mode ob-clojurescript writegood-mode writegood
             osx-dictionary all-the-icons-dired-mode all-the-icons-dired all-the-icons dired-rsync
             docker xref-js2 js2-refactor ruby-refactor parinfer ace-windows tide forge key-chord
             synonymous define-word htmlize esup inf-ruby ruby-additional counsel-etags rust-mode
             noflet lsp-css company-lsp lsp-javascript-typescript lsp-ruby js-comint skewer-mode
             nodejs-repl slime-js slime markdown-mode palimpsest-mode palimpsest palimpset-mode
             paredit 0xc evil-fringe-mark evil-fringe-marks enh-ruby-mode 0blayout org-bullets
             web-beautify graphql-mode haskell-mode flycheck drag-stuff col-highlight crosshairs rg
             eacl highlight-indentation evil-multiedit coverlay coverage coverage-mode
             evil-collection eyebrowse telephone-line magithub git-timemachine dash-at-point
             diminish yaml-mode lispyville x-clip xclip evil-mc multiple-cursors evil-lispy lispy
             windresize dumb-jump expand-region origami company-dict ac-js2 auto-complete indium
             less-css-mode sass-mode scss-mode dockerfile-mode emmet-mode yatemplate yasnippet
             company deft cider clojure-mode wgrep rainbow-mode rspec-mode ruby-end
             ruby-interpolation bundler paredit-everywhere hydra counsel-projectile iedit
             typescript-mode coffee-mode json-mode context-coloring prettier-js import-js js2-mode
             web-mode diff-hl fullframe evil-magit git-link magit highlight-parentheses evil-numbers
             exec-path-from-shell use-package))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(projectile-mode-line-prefix " Proj")
 '(projectile-switch-project-action 'projectile-dired)
 '(projectile-tags-command "git ls-files | /usr/local/bin/ctags -e -L -")
 '(pulse-delay 0.03)
 '(pulse-iterations 20)
 '(require-final-newline t)
 '(rubocopfmt-show-errors nil)
 '(ruby-align-chained-calls t)
 '(ruby-align-to-stmt-keywords t)
 '(ruby-deep-arglist t)
 '(ruby-deep-indent-paren '(40 91 93 123 125))
 '(ruby-deep-indent-paren-style 'space)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook (lambda nil (eval-buffer) (ert-run-tests-interactively t)) nil t)
     (eval add-hook 'after-save-hook (lambda nil (ert-run-tests-interactively t) (other-window)) nil
           t)
     (eval add-hook 'after-save-hook
           (lambda nil (ert-run-tests-interactively t) (switch-to-buffer "find-in-project.el")) nil
           t)
     (eval add-hook 'after-save-hook (lambda nil (ert-run-tests-interactively t)) nil t)
     (find-sibling-rules ("\\([^/]+\\)\\.ts\\'" "\\1\\.test\\.ts"))
     (find-sibling-rules ("src/\\(.*)\\)\\.ts" "test/\\1\\.test\\.ts"))
     (find-sibling-rules ("src/\\(.*)\\)\\.ts" "test/\\1\\.test.ts"))
     (find-sibling-rules ("src/[^/]+//(.*)\\)\\.ts" "test/.*/\\1\\.test.ts"))
     (find-sibling-rules quote (("src/[^/]+//(.*)\\)\\.ts" "test/.*/\\1\\.test.ts")))
     (find-sibling-rules quote ("src/[^/]+//(.*)\\)\\.ts" "test/.*/\\1\\.test.ts"))
     (cider-clojure-cli-global-options . -M:dev:cider)
     (find-sibling-rules "\\([^/]+\\)\\.ts\\" "\\1\\.spec\\.ts")
     (find-sibling-rules "\\([^/]+\\)\\.ts\\" "\\1.spec.ts")
     (eval progn (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares
                        "shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-clojure-cli-global-options . "-A:dev") (cider-shadow-default-options . "web")
     (cider-default-cljs-repl . shadow) (coverlay:base-path . "/Users/auray/work/dox-errors-js/")
     (cider-shadow-cljs-default-options . "spa") (cider-shadow-cljs-default-options . "app")
     (org-reveal-title-slide . "<h1>Life beyond distributed Transactions</h1>")
     (org-reveal-title-slide . "<h1>Life beyong distributed Transactions</h1>")
     (org-reveal-title-slide . "<h1>Life Beyong Distributed Transactions</h1>")
     (org-reveal-title-slide . "<h1>Yo</h1>")
     (eval when (require 'rainbow-mode nil t) (rainbow-mode 1))
     (eval add-hook 'after-save-hook (lambda nil (org-babel-tangle)) nil t)
     (eval progn (require 'find-file-in-project)
           (setq ffip-prune-patterns `("*/public/packs*/*" ,@ffip-prune-patterns))
           (setq prettier-js-command
                 "/home/auray/work/doximity-client-vue/node_modules/.bin/prettier"))
     (compilation-scroll-output 'first-error) (compilation-scroll-output t)
     (ffip-project-root . "/home/auray/work/doximity-client-vue/")
     (eval progn (require 'find-file-in-project)
           (setq ffip-prune-patterns `("vendor/*" ,@ffip-prune-patterns)))
     (ffip-project-root . "/home/aurelienbottazini/work/doximity-client-vue/")
     (alternative-files-rules ("src/\\(.*\\).vue" "test/unit/specs/\\1.spec.js")
                              ("test/unit/specs/\\(.*\\).spec.js" "src/\\1.vue")
                              ("src/\\(.*\\).js" "test/unit/specs/\\1.spec.js")
                              ("test/unit/specs/\\(.*\\).spec.js" "src/\\1.js"))
     (eval add-hook 'after-save-hook (lambda nil (org-html-export-to-html t)) t t)
     (eval progn (require 'find-file-in-project)
           (setq ffip-prune-patterns `("*/public/packs*/*" ,@ffip-prune-patterns)))
     (ffip-find-options . "-not -size +64k -not -iwholename './target/*'")
     (ffip-find-options . "-not -size +64k -not -iwholename './target/debug/*'")
     (org-confirm-babel-evaluate)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(show-paren-style 'parenthesis)
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(speedbar-default-position 'right)
 '(speedbar-frame-parameters
   '((minibuffer . t) (width . 100) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0)
     (unsplittable . t) (left-fringe . 0) (height . 60)))
 '(speedbar-show-unknown-files t)
 '(swiper-goto-start-of-match t)
 '(tab-always-indent nil)
 '(tab-bar-close-button-show nil)
 '(tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
 '(tab-bar-history-mode t)
 '(tab-bar-tab-hints t)
 '(tags-add-tables nil)
 '(tool-bar-mode nil)
 '(treemacs-no-png-images nil)
 '(treemacs-position 'right)
 '(truncate-lines nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F")
     (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F")
     (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB")
     (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(vc-follow-symlinks t)
 '(web-mode-auto-close-style 2)
 '(web-mode-code-indent-offset 0 t)
 '(web-mode-css-indent-offset 0 t)
 '(web-mode-enable-auto-expanding t)
 '(web-mode-enable-auto-opening t)
 '(web-mode-enable-auto-pairing t)
 '(web-mode-enable-comment-interpolation t)
 '(web-mode-enable-front-matter-block t)
 '(xref-search-program 'ripgrep))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#282828" :foreground "#ebdbb2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Operator Mono SSm AB"))))
 '(fixed-pitch ((t (:family "Operator Mono Ssm AB"))))
 '(org-block-begin-line ((t (:inherit font-lock-comment-face :extend t :slant italic))))
 '(org-block-end-line ((t (:inherit font-lock-comment-face :extend t :slant italic))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :weight bold :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :weight bold :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :weight bold :height 1.0)))))
