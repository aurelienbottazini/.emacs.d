
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
      (auray/fip-base-name buffer-file-name)
      " $(git rev-parse --show-toplevel)"
      ))
    "\n")
 1))

(defun auray/filter-out-extra-files (a-file-string-list suffix)
  (cl-remove-if-not (lambda (str) (string-suffix-p suffix str)) a-file-string-list))

(ert-deftest auray/filter-out-extra-files-test ()
  (should (equal '("foo.ts")  (auray/filter-out-extra-files '("foo.clj" "foo.ts") "ts"))))

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
   ((string-match "test/" a-file-path) (concat "src/" (substring (file-name-directory a-file-path) (+ 1 (length "test"))) (auray/fip-base-name a-file-path)".clj"))
   ((string-match "src/" a-file-path) (concat "test/" (substring (file-name-directory a-file-path) (+ 1 (length "src"))) (auray/fip-base-name a-file-path)"_test.clj"))
   (t nil)))

(ert-deftest auray/default-alternate-file-test ()
  (should (string= nil (auray/default-alternate-file "")))
  (should (equal "test/foo_test.clj" (auray/default-alternate-file "src/foo.clj")))
  (should (equal "test/bar_test.clj" (auray/default-alternate-file "src/bar.clj")))
  (should (equal "src/foo.clj" (auray/default-alternate-file "test/foo_test.clj")))
  (should (equal "src/bar.clj" (auray/default-alternate-file "test/bar_test.clj")))
  (should (equal "src/nested/path/bar.clj" (auray/default-alternate-file "test/nested/path/bar_test.clj")))
  )

(defun auray/find-file-with-similar-name ()
  "Find file with similar name in project."
  (interactive)
  (let ((alternate-files (auray/filter-out-extra-files (auray/alternate-files-for-current-buffer) (file-name-extension (buffer-file-name))))
        (tramp-prefix (file-remote-p (buffer-file-name)))
        )
    (cond
     ((file-exists-p (concat (git-root-directory) (auray/default-alternate-file (relative-path-to-git-root)))) (find-file (concat (git-root-directory) (auray/default-alternate-file (relative-path-to-git-root)))))
     ((zerop (length alternate-files))
      (find-file (auray/default-alternate-file (buffer-file-name)))
      )
     ((equal 1 (length alternate-files))
      (find-file (concat tramp-prefix (car alternate-files))))
     (t (find-file (concat tramp-prefix
                           (ido-completing-read
                            "Alternate files: "
                            alternate-files)
                           ))))))

(defun auray/project-guess-file ()
  "Find file using current word as a guess. There are adjustements made from my workflow. For example this is made to navigate file imports. I rarely import test file so to make the navigation quicker I excluded test files from the results."
  (interactive)
  (let* ((pr (project-current t))
         (dirs (list (project-root pr)))
         (results
          (split-string
           (shell-command-to-string
            (concat
             "fd --hidden --type file --exclude '*.spec.*' -p '.*"
             (replace-regexp-in-string "\\.\\./" ""
                                       (replace-regexp-in-string "^~" "" (substring-no-properties (thing-at-point 'filename))))
             "'")))))
    (cond
     ((and (file-exists-p (substring-no-properties (thing-at-point 'filename)))
           (not (file-directory-p (substring-no-properties (thing-at-point 'filename))))) (find-file (substring-no-properties (thing-at-point 'filename))))
     ((zerop (length results)) (message "Cannot guess"))
     ((equal 1 (length results)) (find-file (car results)))
     (t (find-file (ido-completing-read "Guessed files: " results)))
     )))


(defun auray/project-guess-file-refine-multiple (file-list query-name)
  "Filter FILE-LIST to return files that best match QUERY-NAME."
  (let ((pattern (concat "/"? (regexp-quote query-name) "\\.")))
    (cl-remove-if-not
     (lambda (file-name) (string-match-p pattern (file-name-nondirectory file-name)))
     file-list)))

(ert-deftest auray/project-guess-file-refine-multiple-test ()
  (should (equal '("foo/AppNav.js" "AppNav.vue" "AppNav.js")  (auray/project-guess-file-refine-multiple '("foo/AppNav.js" "AppNav.vue" "AppNav.js" "foo/AppNav/somefile") "AppNav"))))

(setq counsel-fzf-cmd "fd --type f | fzf -f \"%s\"")

(defun auray/project-find-file ()
  "Visit a file (with completion) in the current project."
  (interactive)
  (let* ((pr (project-current t))
         (dirs (list (project-root pr))))
    (counsel-fzf nil (project-root (project-current t)))))

(provide 'auray/find-in-project)
