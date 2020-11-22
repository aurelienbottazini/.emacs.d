;;; abott-find-in-project.el --- Utilities to navigate files in a project  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  DESKTOP-2O67MQ6

;; Author: Aurélien Bottazini

(defun abott-fip-base-name (file-name)
  "Extract the base name for current buffer. This base name will be used to find simlarly named files for the current project."
  (downcase
   (car (split-string
         (car (split-string (file-name-nondirectory file-name) "\\."))
         "_spec"))))

(ert-deftest abott-fip-base-name-test ()
  (should (string= "foo" (abott-fip-base-name "path/foo.el")))
  (should (string= "foo" (abott-fip-base-name "path/foo_spec.rb"))))

(defun abott-alternate-files-for-current-buffer ()
  (nbutlast
   (split-string
    (shell-command-to-string
     (concat
      "fd --hidden "
      (abott-fip-base-name buffer-file-name)
      " $(git rev-parse --show-toplevel) | rg -v "
      (file-name-nondirectory buffer-file-name)))
    "\n")
   1))

(defun abott-find-file-with-similar-name ()
  "Find file with similar name in project."
  (interactive)
  (let ((alternate-files (abott-alternate-files-for-current-buffer)))
    (cond
     ((zerop (length alternate-files))
      (message "There's no alternate files"))
     ((equal 1 (length alternate-files))
      (find-file (car alternate-files)))
     (t (find-file (ido-completing-read
                    "Alternate files: "
                    alternate-files))))))

(provide 'abott-find-in-project)
