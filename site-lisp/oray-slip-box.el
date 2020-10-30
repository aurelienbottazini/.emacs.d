(defun slip-box()
  "Setup windows to work with my slip box"
  (interactive)
  (delete-other-windows)
  (find-file (concat **local-dropbox-folder** "org/slip-box/index.org"))
  (split-window-horizontally)
  (other-window 1)
  (find-file (concat **local-dropbox-folder** "org/slip-box/"))
  (other-window 1))

(defun add-slip-box-card()
  "Add slip box file."
  )

(defun next-slip-box-card-letter (card-filename)
  (let* ((last-char (substring card-filename -1))
         (all-but-last-char (substring card-filename 0 -1))
         (last-number (string-to-number last-char)))
    (cond ((> last-number 0) (concat all-but-last-char (number-to-string (+ 1 last-number))))
          (t 3)
          ))
  )

(ert-deftest next-slip-box-card-letter-test ()
  "Tests the rendering of `quote' symbols in `pp-to-string'."
  (should (string= (next-slip-box-card-letter "1") "2"))
  (should (string= (next-slip-box-card-letter "9") "10"))
  (should (string= (next-slip-box-card-letter "a1") "a2"))
  (should (string= (next-slip-box-card-letter "a19") "a20"))
  )

;; (ert "")

(provide 'oray-slip-box)
