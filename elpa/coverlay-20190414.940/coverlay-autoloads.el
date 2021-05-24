;;; coverlay-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "coverlay" "coverlay.el" (0 0 0 0))
;;; Generated autoloads from coverlay.el

(autoload 'coverlay-load-file "coverlay" "\
(re)load lcov coverage data from FILEPATH.

\(fn FILEPATH)" t nil)

(autoload 'coverlay-reload-file "coverlay" "\
(re)load lcov coverage data from current filepath." t nil)

(autoload 'coverlay-watch-file "coverlay" "\
Watch file at FILEPATH for coverage data.

\(fn FILEPATH)" t nil)

(autoload 'coverlay-toggle-overlays "coverlay" "\
Toggle coverage overlay in BUFFER.

\(fn BUFFER)" t nil)

(autoload 'coverlay-display-stats "coverlay" "\
Display buffer with current coverage statistics." t nil)

(autoload 'coverlay-minor-mode "coverlay" "\
overlays for uncovered lines

If called interactively, enable Coverlay minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-coverlay-mode "coverlay" "\
Turn on `coverlay-mode'." nil nil)

(put 'global-coverlay-mode 'globalized-minor-mode t)

(defvar global-coverlay-mode nil "\
Non-nil if Global Coverlay mode is enabled.
See the `global-coverlay-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-coverlay-mode'.")

(custom-autoload 'global-coverlay-mode "coverlay" nil)

(autoload 'global-coverlay-mode "coverlay" "\
Toggle Coverlay minor mode in all buffers.
With prefix ARG, enable Global Coverlay mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Coverlay minor mode is enabled in all buffers where
`turn-on-coverlay-mode' would do it.
See `coverlay-minor-mode' for more information on Coverlay minor mode.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'coverlay-mode #'global-coverlay-mode "3.0.0")

(define-obsolete-variable-alias 'coverlay-mode-hook 'coverlay-minor-mode-hook "3.0.0")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "coverlay" '("coverlay")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; coverlay-autoloads.el ends here
