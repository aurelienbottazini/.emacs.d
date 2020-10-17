;;; ruby-interpolation-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ruby-interpolation" "ruby-interpolation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ruby-interpolation.el

(autoload 'ruby-interpolation-mode "ruby-interpolation" "\
Automatic insertion of ruby string interpolation.

If called interactively, enable Ruby-Interpolation mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ruby-interpolation" '("ruby-interpolation-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ruby-interpolation-autoloads.el ends here
