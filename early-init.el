;;; -*- lexical-binding: t; -*-

(setenv "LSP_USE_PLISTS" "true")

;; Keep expensive handlers and GC tuned for startup as early as possible.
(defvar abo/default-file-name-handler-alist file-name-handler-alist)
(defvar abo/default-gc-cons-threshold (* 16 1024 1024))
(defvar abo/default-gc-cons-percentage 0.1)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      package-enable-at-startup nil)

;; Native compilation is not usable on this machine right now.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))
(when (boundp 'comp-enable-subr-trampolines)
  (setq comp-enable-subr-trampolines nil))
