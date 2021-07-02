;;; osx-clipboard-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "osx-clipboard" "osx-clipboard.el" (0 0 0 0))
;;; Generated autoloads from osx-clipboard.el

(let ((loads (get 'osx-clipboard 'custom-loads))) (if (member '"osx-clipboard" loads) nil (put 'osx-clipboard 'custom-loads (cons '"osx-clipboard" loads))))

(defvar osx-clipboard-mode nil "\
Non-nil if Osx-Clipboard mode is enabled.
See the `osx-clipboard-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `osx-clipboard-mode'.")

(custom-autoload 'osx-clipboard-mode "osx-clipboard" nil)

(autoload 'osx-clipboard-mode "osx-clipboard" "\
Kill and yank using the OS X clipboard when running in a text terminal.

If called interactively, enable Osx-Clipboard mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

This mode allows Emacs to use the OS X system clipboard when
running in the terminal, making killing and yanking behave
similarly to a graphical Emacs.  It is not needed in a graphical
Emacs, where NS clipboard integration is built in.

It sets the variables `interprogram-cut-function' and
`interprogram-paste-function' to thin wrappers around the
\"pbcopy\" and \"pbpaste\" command-line programs.

Consider also customizing the variable
  `save-interprogram-paste-before-kill' to `t' for best results.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "osx-clipboard" '("osx-clipboard-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; osx-clipboard-autoloads.el ends here
