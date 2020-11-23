;; -*- coding: utf-8; lexical-binding: t; -*-

;; minimum UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(menu-bar-mode -1)

;; maximise window at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; let titlebar match theme
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

;; frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat " " (abbreviate-file-name (buffer-file-name)))
                 " %b"))))

;; disable fringe
(fringe-mode 0)

;; load theme
(setq srcery-invert-region nil)
(load-theme 'srcery t)

;; change cursor color
;; (set-cursor-color "#cf5a65")
;; https://github.com/hlissner/doom-emacs/issues/1848
(setq evil-normal-state-cursor '(box "#cf5a65")
      evil-insert-state-cursor '(bar "#cf5a65")
      evil-visual-state-cursor '(hollow "#cf5a65"))

;; change default font
;; (set-frame-font "Fira Code 12" nil t)
(set-frame-font "JetBrains Mono 12" nil t)

;; column indicator
(setq-default display-fill-column-indicator-column 80)

;; selected text color
(set-face-attribute 'region nil :background "#666666")

;; highlight brackets
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(set-face-attribute 'show-paren-match nil :weight 'bold :background "#349cd9")

;; company tooltip color
(with-eval-after-load 'company
  (custom-set-faces
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "#ffeead" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "#69adc6" :foreground "white"))))
   '(company-tooltip-common
     ((t (:background "#ffeead" :foreground "black"))))))

;; highlight current line
(global-hl-line-mode 1)

;; org src block faces
(with-eval-after-load 'org
  (custom-set-faces
   '(org-block-begin-line
     ((t (:underline t))))
   '(org-block-end-line
     ((t (:overline t))))))
