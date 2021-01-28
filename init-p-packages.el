;; -*- coding: utf-8; lexical-binding: t; -*-

;; non-Melpa packages
(add-to-list 'load-path "~/.emacs.p/packages/color-rg")

;; void variable issue in the new version, so use stable version
;; most recent version has no issue
;; (require-package 'ivy-rich)

;; use stable version of ess
;; new version splited stata-mode separately
(require-package 'ess)

(provide 'init-p-packages)
