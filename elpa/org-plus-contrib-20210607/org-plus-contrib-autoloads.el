;;; org-plus-contrib-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ob-C" "ob-C.el" (0 0 0 0))
;;; Generated autoloads from ob-C.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-C" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-J" "ob-J.el" (0 0 0 0))
;;; Generated autoloads from ob-J.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-J" '("obj-" "org-babel-")))

;;;***

;;;### (autoloads nil "ob-R" "ob-R.el" (0 0 0 0))
;;; Generated autoloads from ob-R.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-R" '("ob-R-" "org-babel-")))

;;;***

;;;### (autoloads nil "ob-abc" "ob-abc.el" (0 0 0 0))
;;; Generated autoloads from ob-abc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-abc" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-arduino" "ob-arduino.el" (0 0 0 0))
;;; Generated autoloads from ob-arduino.el

(autoload 'org-babel-execute:arduino "ob-arduino" "\
org-babel arduino hook.

\(fn BODY PARAMS)" nil nil)

(eval-after-load 'org '(add-to-list 'org-src-lang-modes '("arduino" . arduino)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-arduino" '("ob-arduino:" "org-babel-default-header-args:sclang")))

;;;***

;;;### (autoloads nil "ob-asymptote" "ob-asymptote.el" (0 0 0 0))
;;; Generated autoloads from ob-asymptote.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-asymptote" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-awk" "ob-awk.el" (0 0 0 0))
;;; Generated autoloads from ob-awk.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-awk" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-calc" "ob-calc.el" (0 0 0 0))
;;; Generated autoloads from ob-calc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-calc" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-clojure" "ob-clojure.el" (0 0 0 0))
;;; Generated autoloads from ob-clojure.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-clojure" '("ob-clojure-" "org-babel-")))

;;;***

;;;### (autoloads nil "ob-clojure-literate" "ob-clojure-literate.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ob-clojure-literate.el

(defvar ob-clojure-literate-auto-jackin-p nil "\
Auto jack in ob-clojure project.
Don't auto jack in by default for not rude.")

(custom-autoload 'ob-clojure-literate-auto-jackin-p "ob-clojure-literate" t)

(autoload 'ob-clojure-literate-specify-session "ob-clojure-literate" "\
Specify ob-clojure header argument :session with value selected from a list of available sessions." t nil)

(autoload 'ob-clojure-literate-auto-jackin "ob-clojure-literate" "\
Auto setup ob-clojure-literate scaffold and jack-in Clojure project." t nil)

(autoload 'ob-clojure-literate-enable "ob-clojure-literate" "\
Enable Org-mode buffer locally for `ob-clojure-literate'." nil nil)

(autoload 'ob-clojure-literate-disable "ob-clojure-literate" "\
Disable Org-mode buffer locally for `ob-clojure-literate'." nil nil)

(if ob-clojure-literate-auto-jackin-p (ob-clojure-literate-auto-jackin))

(autoload 'ob-clojure-literate-mode "ob-clojure-literate" "\
A minor mode to toggle `ob-clojure-literate'.

If called interactively, enable Ob-Clojure-Literate mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-clojure-literate" '("ob-clojure-literate-")))

;;;***

;;;### (autoloads nil "ob-comint" "ob-comint.el" (0 0 0 0))
;;; Generated autoloads from ob-comint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-comint" '("org-babel-comint-")))

;;;***

;;;### (autoloads nil "ob-coq" "ob-coq.el" (0 0 0 0))
;;; Generated autoloads from ob-coq.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-coq" '("coq-program-name" "org-babel-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-plus-contrib-autoloads.el ends here
