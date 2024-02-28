
;;; auray-find-in-project.el --- Utilities to navigate files in a project  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author: Aurélien Bottazini

(require 'cl-lib)

(defun auray/fip-base-name (file-name)
  "Extract the base name for current buffer. This base name will be used to find simlarly named files for the current project."
  (downcase
   (car (split-string
         (car
          (split-string
           (car (split-string
                 (file-name-nondirectory (file-name-sans-extension file-name))
                 "_spec"))
           ".test"))
         ".jest"))))

(ert-deftest auray/fip-base-name-test ()
  (should (string= "foo" (auray/fip-base-name "path/foo.el")))
  (should (string= "foo.client" (auray/fip-base-name "path/foo.client.ts")))
  (should (string= "foo.client" (auray/fip-base-name "path/foo.client.jest.ts")))
  (should (string= "foo" (auray/fip-base-name "path/foo.test.ts")))
  (should (string= "foo" (auray/fip-base-name "path/foo_spec.rb"))))

(defun auray/alternate-files-for-current-buffer ()
  (nbutlast
   (split-string
    (shell-command-to-string
     (concat
      "fd --color never --hidden --exclude .git --exclude "
      (file-name-nondirectory buffer-file-name)
      " "
      (string-replace "_controller" nil (auray/fip-base-name buffer-file-name))
      " $(git rev-parse --show-toplevel)"
      ))
    "\n")
 1))

(defun auray/filter-out-extra-files (a-file-string-list suffix)
  "Filter out files that do not end with any of the given SUFFIX-LIST from A-FILE-STRING-LIST."
  (let ((suffix-list (cond
                      ((or (string= "ts" suffix)
                           (string= "tsx" suffix)
                           (string= "vue" suffix)
                           (string= "js" suffix)
                           ) '("vue" "js" "ts" "tsx"))
                      (t (list suffix))
                      )))
    (cl-remove-if-not (lambda (str)
                        (some (lambda (suff)
                                (string-suffix-p suff str)) suffix-list))
                      a-file-string-list)))


(ert-deftest auray/filter-out-extra-files-test ()
  (should (equal '("foo.ts")  (auray/filter-out-extra-files '("foo.clj" "foo.ts") "ts")))
  (should (equal '("foo.tsx")  (auray/filter-out-extra-files '("foo.clj" "foo.tsx") "tsx")))
  (should (equal '("foo.vue" "foo.ts")  (auray/filter-out-extra-files '("foo.vue" "foo.ts") "ts")))
  (should (equal '("foo.vue" "foo.ts")  (auray/filter-out-extra-files '("foo.vue" "foo.ts") "vue")))
  (should (equal '("foo.vue" "foo.ts")  (auray/filter-out-extra-files '("foo.vue" "foo.ts") "js")))
  )

(defun git-root-directory (&optional file)
  "Return the root of the Git repository containing FILE, or nil if none.
If FILE is nil, use the current buffer's file name."
  (let ((file (or file (buffer-file-name))))
    (locate-dominating-file file ".git")))

(defun relative-path-to-git-root (&optional file)
  "Return the path of FILE relative to the root of the Git repository.
If FILE is nil, use the current buffer's file name."
  (let* ((file (or file (buffer-file-name)))
         (root (git-root-directory file)))
    (if (and file root)
        (file-relative-name file root)
      nil)))

(defun auray/default-alternate-file (a-file-path)
  (cond
   ((not (stringp a-file-path)) "")

   ((string-match "app/helpers" a-file-path) (string-replace "app/helpers" "spec/helpers" a-file-path))
   ((string-match "spec/requests" a-file-path) (concat "app/controllers/" (auray/fip-base-name a-file-path) "_controller.rb"))
   ((string-match "spec/" a-file-path) (concat (substring (file-name-directory a-file-path) (+ 1 (length "spec"))) (auray/fip-base-name a-file-path)".rb"))
   ((string-match "app/controllers" a-file-path) (concat "spec/requests/" (string-replace "_controller" nil  (auray/fip-base-name a-file-path))"_spec.rb"))

   ((string-match "app/" a-file-path) (concat "spec/" (substring (file-name-directory a-file-path)) (auray/fip-base-name a-file-path)"_spec.rb"))

   ((string-match "test/" a-file-path) (concat "src/" (substring (file-name-directory a-file-path) (+ 1 (length "test"))) (auray/fip-base-name a-file-path)".clj"))
   ((string-match "src/" a-file-path) (concat "test/" (substring (file-name-directory a-file-path) (+ 1 (length "src"))) (auray/fip-base-name a-file-path)"_test.clj"))
   (t "")))

(ert-deftest auray/default-alternate-file-test ()
  (should (string= "" (auray/default-alternate-file nil)))
  (should (string= "" (auray/default-alternate-file "")))
  (should (equal "app/foo.rb" (auray/default-alternate-file "spec/app/foo.rb")))
  (should (equal "spec/requests/contacts_spec.rb" (auray/default-alternate-file "app/controllers/contacts_controller.rb")))
  (should (equal "app/helpers/contacts_helper.rb" (auray/default-alternate-file "spec/helpers/contacts_helper_spec.rb")))
  (should (equal "app/controllers/contacts_controller.rb" (auray/default-alternate-file "spec/requests/contacts_spec.rb")))
  (should (equal "spec/app/foo_spec.rb" (auray/default-alternate-file "app/foo.rb")))
  (should (equal "test/foo_test.clj" (auray/default-alternate-file "src/foo.clj")))
  (should (equal "test/bar_test.clj" (auray/default-alternate-file "src/bar.clj")))
  (should (equal "src/foo.clj" (auray/default-alternate-file "test/foo_test.clj")))
  (should (equal "src/bar.clj" (auray/default-alternate-file "test/bar_test.clj")))
  (should (equal "src/nested/path/bar.clj" (auray/default-alternate-file "test/nested/path/bar_test.clj")))
  )

(defhydra auray/hydra-alternate-files (:color blue)
  "Choose action"
  ("c" (auray/create-alternate-file) "Create alternate file")
  ("s" (auray/search-for-alternate-files-with-ido) "Search for alternate files with ido")
  ("q" nil "Quit"))

(defun auray/create-alternate-file ()
  (interactive)
  ;; Your code for creating an alternate file here
  (let ((default-alternative-filepath (concat (git-root-directory) (auray/default-alternate-file (relative-path-to-git-root)))))
    (cond
     ((or (string-empty-p default-alternative-filepath)
          (file-directory-p default-alternative-filepath)) (message "don't know how to choose a default alternate file"))
     (t (find-file default-alternative-filepath)))))

(defun auray/search-for-alternate-files-with-ido ()
  (interactive)
  (let ((alternate-files (auray/filter-out-extra-files (auray/alternate-files-for-current-buffer) (file-name-extension (buffer-file-name)))))
    (cond
     ((null alternate-files) (message "no alternate files"))
     (t (find-file (ido-completing-read
                    "Alternate files: "
                    alternate-files)
)))))

(defun auray/find-file-with-similar-name ()
  "Find file with similar name in project."
  (interactive)
  (let ((alternate-files (auray/filter-out-extra-files (auray/alternate-files-for-current-buffer) (file-name-extension (buffer-file-name))))
        (default-alternative-filepath (concat (git-root-directory) (auray/default-alternate-file (relative-path-to-git-root)))))
    (cond
     ((and (not (file-directory-p default-alternative-filepath)) (file-exists-p default-alternative-filepath)) (find-file default-alternative-filepath))
     ((equal 1 (length alternate-files))
      (find-file (car alternate-files)))
     (t (auray/hydra-alternate-files/body))

     )))

(defun auray/project-guess-file ()
  "Find file using current word as a guess. There are adjustements made from my workflow. For example this is made to navigate file imports. I rarely import test file so to make the navigation quicker I excluded test files from the results."
  (interactive)
  (let* ((pr (project-current t))
         (dirs (list (project-root pr)))
         (filename (replace-regexp-in-string "~/" (git-root-directory) (substring-no-properties (thing-at-point 'filename))))
         (results
          (split-string
           (shell-command-to-string
            (concat
             "fd --hidden --type file --exclude '*.spec.*' -p '.*"
             (replace-regexp-in-string "\\.\\./" ""
                                       (replace-regexp-in-string "^~" "" filename))
             "'")))))
    (cond
     ((and (file-exists-p filename)
           (not (file-directory-p filename))) (find-file filename))
     ((file-exists-p (concat filename ".vue")) (find-file (concat filename ".vue")))
     ((file-exists-p (concat filename ".js")) (find-file (concat filename ".js")))
     ((file-exists-p (concat filename ".mjs")) (find-file (concat filename ".mjs")))
     ((file-exists-p (concat filename ".rb")) (find-file (concat filename ".rb")))
     ((and (file-directory-p filename)
           (file-exists-p (concat filename "/index.vue"))) (find-file (concat filename "/index.vue")))
     ((and (file-directory-p filename)
           (file-exists-p (concat filename "/index.js"))) (find-file (concat filename "/index.js")))
     ((equal 1 (length results)) (find-file (car results)))
     (t (find-file (ido-completing-read "Guessed files: " results)))
     )))


(defun auray/project-guess-file-refine-multiple (file-list query-name)
  "Filter FILE-LIST to return files that best match QUERY-NAME."
  (let ((pattern (concat "/?" query-name "[^/]*")))
    (cl-remove-if-not
     (lambda (file-name) (string-match-p pattern (file-name-nondirectory file-name)))
     file-list)))

(ert-deftest auray/project-guess-file-refine-multiple-test ()
  (should (equal '("foo/AppNav.js" "AppNav.vue" "AppNav.js")  (auray/project-guess-file-refine-multiple '("foo/AppNav.js" "AppNav.vue" "AppNav.js" "foo/AppNav/somefile") "AppNav"))))

(setq counsel-fzf-cmd "fd --hidden --type f | fzf -f \"%s\"")

(defun auray/project-find-file ()
  "Visit a file (with completion) in the current project."
  (interactive)
  (let* ((pr (project-current t))
         (dirs (list (project-root pr))))
    (counsel-fzf nil (project-root (project-current t)))))

(provide 'auray/find-in-project)

;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda () (eval-buffer) (ert-run-tests-interactively t)) nil t)
;; End:
