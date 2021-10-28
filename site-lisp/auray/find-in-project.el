;;; abott-find-in-project.el --- Utilities to navigate files in a project  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  DESKTOP-2O67MQ6

;; Author: Aurélien Bottazini

(defun auray/fip-base-name (file-name)
  "Extract the base name for current buffer. This base name will be used to find simlarly named files for the current project."
  (downcase
   (car (split-string
         (car (split-string (file-name-nondirectory file-name) "\\."))
         "_spec"))))

(ert-deftest auray/fip-base-name-test ()
  (should (string= "foo" (auray/fip-base-name "path/foo.el")))
  (should (string= "foo" (auray/fip-base-name "path/foo_spec.rb"))))

(defun auray/alternate-files-for-current-buffer ()
  (nbutlast
   (split-string
    (shell-command-to-string
     (concat
      "fd --hidden --exclude "
      (file-name-nondirectory buffer-file-name)
      " "
      (auray/fip-base-name buffer-file-name)
      " (git rev-parse --show-toplevel)"
      ))
    "\n")
   1))

(defun auray/find-file-with-similar-name ()
  "Find file with similar name in project."
  (interactive)
  (let ((alternate-files (auray/alternate-files-for-current-buffer)))
    (cond
     ((zerop (length alternate-files))
      (message "There's no alternate files"))
     ((equal 1 (length alternate-files))
      (find-file (car alternate-files)))
     (t (find-file (ido-completing-read
                    "Alternate files: "
                    alternate-files))))))

(provide 'auray/find-in-project)
