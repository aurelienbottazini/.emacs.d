
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

(defun auray/find-file-with-similar-name ()
  "Find file with similar name in project."
  (interactive)
  (let ((alternate-files (auray/filter-out-extra-files (auray/alternate-files-for-current-buffer) (file-name-extension (buffer-file-name))))
        (tramp-prefix (file-remote-p (buffer-file-name)))
        )
    (cond
     ((zerop (length alternate-files))
      (message "There's no alternate files"))
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
             "fd --hidden --exclude '*.spec.*' -p '.*"
             (replace-regexp-in-string "\\.\\./" ""
                                       (replace-regexp-in-string "^~" "" (substring-no-properties (thing-at-point 'filename)))) ".*' $(git rev-parse --show-toplevel)"
             )))))

    (message (car results))
    (cond
     ((zerop (length results)) (message "Cannot guess"))
     ((equal 1 (length results)) (find-file (car results)))
     (t (find-file (ido-completing-read "Guessed files: " results)))
     )

    ))

(setq counsel-fzf-cmd "fd --type f | fzf -f \"%s\"")

(defun auray/project-find-file ()
  "Visit a file (with completion) in the current project."
  (interactive)
  (let* ((pr (project-current t))
         (dirs (list (project-root pr))))
    (counsel-fzf nil (project-root (project-current t)))))

(provide 'auray/find-in-project)
