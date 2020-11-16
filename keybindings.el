;; -*- coding: utf-8; lexical-binding: t; -*-

(define-key evil-normal-state-map (kbd "god") 'p-delete-parens)
(define-key evil-normal-state-map (kbd "gor") 'p-ex-evil-buffer-replace)
(define-key evil-normal-state-map (kbd "gok") 'p-surround-parens)
(define-key evil-normal-state-map (kbd "gof") 'p-surround-brackets)
(define-key evil-normal-state-map (kbd "goh") 'p-surround-curly)
(define-key evil-visual-state-map (kbd "gok") 'p-surround-parens)
(define-key evil-visual-state-map (kbd "gof") 'p-surround-brackets)
(define-key evil-visual-state-map (kbd "goh") 'p-surround-curly)
(define-key evil-visual-state-map (kbd "gor") 'p-ex-evil-selection-replace)

;; evil-escape
(setq-default evil-escape-key-sequence "fd")

;; evil state
(define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
(define-key evil-motion-state-map "j" #'evil-next-visual-line)
(define-key evil-motion-state-map "k" #'evil-previous-visual-line)
(define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-u") 'p-kill-to-begin-of-line)

;; global
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-unset-key (kbd "C-c c"))
(global-set-key (kbd "C-c c") 'org-capture)


(require 'dired)
(define-key dired-mode-map (kbd "<C-return>") 'p-open-in-external-app)

(general-create-definer p-dired-leader-normal-def
  :prefix ";"
  :states 'normal
  :keymaps 'dired-mode-map)
(p-dired-leader-normal-def
 "l" 'dired-downcase
 "u" 'dired-upcase
 "n" 'dired-create-empty-file
 "a" 'dired-create-directory
 "f" 'dired-mark-files-regexp
 "c" 'dired-mark-files-containing-regexp
 "p" 'dired-up-directory)

;; Insert hash on Mac with UK keyboard
;; https://stackoverflow.com/questions/3977069/emacs-question-hash-key
;; this does not work in markdown file
;; (define-key key-translation-map (kbd "M-3") (kbd "#"))
;; this works but need to comment out keybindings for winum in .emacs.d/lisp/init-windows.el
;; https://stackoverflow.com/questions/3977069/emacs-question-hash-key
(global-unset-key (kbd "M-3"))
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; enable C-n and C-p in autocompletion suggestion
(with-eval-after-load "company"
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort)
  ;; enable C-w when company completes the codes
  (define-key company-active-map (kbd "C-w") #'backward-kill-word))

;; space-leader
(my-space-leader-def
  "<SPC>" 'counsel-M-x
  ;; quit
  "qw" 'save-buffers-kill-terminal
  "qq" 'kill-emacs
  ;; buffers, files and projects
  "bb" 'ivy-switch-buffer
  "bi" 'ibuffer
  "bn" 'scratch ;; new scratch buffer
  "bs" 'p-switch-to-scratch
  "bk" 'buf-move-up
  "bj" 'buf-move-down
  "bh" 'buf-move-left
  "bl" 'buf-move-right
  "bd" 'kill-this-buffer
  "bD" 'kill-buffer
  "bm" (lambda () (interactive) (switch-to-buffer "*Messages*"))
  "`" (lambda () (interactive) (switch-to-buffer nil)) ; to previous buffer
  "ff" 'counsel-find-file
  "fs" 'save-buffer
  "fr" 'counsel-recentf
  "fp" 'find-file-in-project
  "fc" 'find-file-in-current-directory
  "fa" 'find-file-in-project-at-point
  "fk" 'find-file-in-project-by-selected
  "fn" 'find-file-with-similar-name ; ffip v5.3.1
  "fd" 'find-directory-in-project-by-selected
  "fR" 'vc-rename-file-and-buffer
  "fC" 'vc-copy-file-and-rename-buffer
  "fP" 'p-find-file-in-private-config
  "dj" 'dired-jump ;; open the dired from current file
  "pp" 'counsel-projectile-find-file
  "ps" 'counsel-projectile-switch-project
  ;; windows
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wk" 'evil-window-up
  "wj" 'evil-window-down
  "wa" 'ace-swap-window
  "wm" 'ace-maximize-window
  "wd" 'delete-window
  "wD" 'kill-buffer-and-window ;; "k" is preserved to replace "C-g"
  "wo" 'delete-other-windows
  "wv" 'split-window-horizontally
  "ws" 'split-window-vertically
  "w1" 'ffip-split-window-horizontally
  "w2" 'ffip-split-window-vertically
  "wr" 'rotate-windows
  "0" 'winum-select-window-0-or-10
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9
  "jj" 'scroll-other-window
  "kk" 'scroll-other-window-up
  ;; search
  "ss" 'my-swiper
  "sS" 'swiper-thing-at-point
  "sf" 'counsel-fzf
  "sb" 'counsel-grep ; grep current buffer
  "sc" 'counsel-etags-grep-current-directory
  "sw" 'counsel-etags-find-tag-at-point
  "sa" 'counsel-ag
  "sh" 'my-select-from-search-text-history
  "sr" 'rgrep
  "sp" 'counsel-projectile-rg
  "sl" 'avy-goto-line
  "sg" 'counsel-locate
  "gf" 'counsel-git
  "gg" 'my-counsel-git-grep
  ;; workgroup
  "ec" 'wg-create-workgroup
  "es" 'wg-switch-to-workgroup
  "ed" 'wg-kill-workgroup-and-buffers
  "eo" 'wg-open-workgroup
  ;; toggle
  "tm" 'toggle-frame-maximized
  "tt" 'counsel-load-theme
  "tw" 'display-time-world
  ;; hydra
  "hw" 'hydra-window/body
  "hs" 'hydra-search/body
  "hg" 'hydra-git/body
  "hl" 'hydra-launcher/body
  "hz" 'hydra-zoom/body
  ;; org-agenda
  "na" 'org-agenda
  ;; org-roam
  "nrf" 'org-roam-find-file
  "nri" 'org-roam-insert
  "nrr" 'org-roam
  ;; org-journal
  "njj" 'org-journal-new-entry)

;; comma-leader
(my-comma-leader-def
  ;; select copied item and insert it above the current line
  "ki" 'counsel-browse-kill-ring
  ;; list and select comment
  "lc" 'my-imenu-comments
  ;; list function
  "lf" 'my-imenu-or-list-tag-in-current-file
  ;; highlight cursor word
  "hs" 'highlight-symbol
  ;; replace cursor word interactively
  "hr" 'highlight-symbol-query-replace
  ;; highlight symbol and navigation
  "hn" 'highlight-symbol-nav-mode ; use M-n/M-p to navigation between symbols
  ;; narrow or widen window
  "nw" 'narrow-or-widen-dwim
  ;; git
  "gm" 'vc-msg-show
  "gh" 'git-gutter:popup-hunk
  "gs" 'magit-status
  "gp" 'magit-diff-popup
  ;; display current directory
  "dd" 'pwd
  ;; copy file name
  "yn" 'cp-filename-of-current-buffer
  ;; copy full path
  "yp" 'cp-fullpath-of-current-buffer
  ;; yank popup
  "yh" 'counsel-yank-pop
  ;; visual replace
  "rv" 'vr/replace
  "re" 'replace-regexp
  "rb" 'evilmr-replace-in-buffer
  "rf" 'evilmr-replace-in-defun
  ;; eval
  "es" 'eval-last-sexp
  "ep" 'eval-print-last-sexp
  ;; insert current buffer name
  "in" 'p-insert-file-name
  ;; google search
  "sg" 'p-google-search
  ;; youtube search
  "sy" 'p-youtube-search
  ;; insert date
  "iu" 'p-insert-uk-date
  "id" 'p-insert-date
  ;; move to beginning of function
  "fb" 'beginning-of-defun
  "fe" 'end-of-defun
  ;; xref
  "jd" 'xref-find-definitions
  "jr" 'xref-find-references
  "jg" 'xref-goto-xref)

;; semicolon-leader
(my-semicolon-leader-def
  "d" 'my-evil-goto-definition
  "h" 'evil-shift-left
  "l" 'evil-shift-right
  "n" 'my-goto-next-hunk
  "p" 'my-goto-previous-hunk
  ";" 'transpose-words)


;; Efficient typing -------------------------------------
(with-eval-after-load 'general
  (general-evil-setup t)
  ;; parenthesis
  (defun p-insert-paren ()
    (interactive)
    (insert "()")
    (backward-char 1))
  (general-imap "k"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'p-insert-paren))
  ;; curly brackets
  (defun p-insert-cbracket ()
    (interactive)
    (insert "{}")
    (backward-char 1))
  (general-imap "h"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "h" 'p-insert-cbracket))
  ;; exclamation
  (defun p-insert-exc ()
    (interactive)
    (insert "!"))
  (general-imap "g"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "t" 'p-insert-exc))
  ;; at
  (defun p-insert-at ()
    (interactive)
    (insert "@"))
  (general-imap "q"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "a" 'p-insert-at))
  ;; british pound
  (defun p-insert-pound ()
    (interactive)
    (insert "£"))
  (general-imap "y"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "b" 'p-insert-pound))
  ;; dollar
  (defun p-insert-dollar ()
    (interactive)
    (insert "$"))
  (general-imap "l"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'p-insert-dollar))
  ;; percentage
  (defun p-insert-percent ()
    (interactive)
    (insert "%"))
  (general-imap "f"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "h" 'p-insert-percent))
  ;; carat
  (defun p-insert-carat ()
    (interactive)
    (insert "^"))
  (general-imap "p"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "w" 'p-insert-carat))
  ;; and
  (defun p-insert-and ()
    (interactive)
    (insert "&"))
  (general-imap "a"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "a" 'p-insert-and))
  ;; asterisk
  (defun p-insert-asterisk ()
    (interactive)
    (insert "*"))
  (general-imap "c"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-asterisk))
  ;; underscore
  (defun p-insert-underscore ()
    (interactive)
    (insert "_"))
  (general-imap "u"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "u" 'p-insert-underscore))
  ;; plus
  (defun p-insert-plus ()
    (interactive)
    (insert "+"))
  (general-imap "j"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-plus))
  ;; pipe
  (defun p-insert-pipe ()
    (interactive)
    (insert "|"))
  (general-imap "s"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "g" 'p-insert-pipe))
  ;; tilde
  (defun p-insert-tilde ()
    (interactive)
    (insert "~"))
  (general-imap "b"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "w" 'p-insert-tilde))
  ;; less than
  (defun p-insert-less ()
    (interactive)
    (insert "<"))
  (general-imap "x"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "y" 'p-insert-less))
  ;; greater than
  (defun p-insert-greater ()
    (interactive)
    (insert ">"))
  (general-imap "d"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "y" 'p-insert-greater))
  ;; question
  (defun p-insert-question ()
    (interactive)
    (insert "?"))
  (general-imap "w"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "w" 'p-insert-question))
  ;; r assign
  (defun p-insert-r-assign ()
    (interactive)
    (insert "<-"))
  (general-imap "e"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-r-assign))
  ;; r connect
  (defun p-insert-r-connect ()
    (interactive)
    (insert "%>%"))
  (general-imap "r"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-r-connect))
  ;; hash
  (defun p-insert-hash ()
    (interactive)
    (insert "#"))
  (general-imap "v"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "v" 'p-insert-hash)))

