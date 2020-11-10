;; -*- coding: utf-8; lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time))))))

;; disable warning message
(setq exec-path-from-shell-check-startup-files nil)

;; default folder in scratch buffer
(setq default-directory "~/")

;; disable bell sound
(setq ring-bell-function 'ignore)

;; trash
(setq trash-directory "~/.Trash")
(setq delete-by-moving-to-trash t)

;; maximum number of recent saved items
(setq recentf-max-saved-items 50)

;; disable automatic cursor re-centering
;; https://www.reddit.com/r/emacs/comments/2dgy52/how_to_stop_emacs_automatically_recentering_the/
(setq scroll-step 1)

;; counsel-locate
;; locate is not available on Mac, so use spotlight (mdfind)
(setq counsel-locate-cmd #'counsel-locate-cmd-mdfind)

;; company
(setq company-minimum-prefix-length 1)

;; snippets
(setq yas-snippet-dirs '("~/.emacs.snippets"))

;; evil-matchit
(setq evilmi-shortcut "m")
(global-evil-matchit-mode 1)

;; ivy-rich
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;; ffip
(setq ffip-use-rust-fd t)

;; projectile
(projectile-mode 1)
(setq projectile-project-search-path '("~/Git/" "~/project/" "~/working/"))

;; increase check interval
(with-eval-after-load 'wucuo
  ;; 300 seconds
  (setq wucuo-update-interval 300))

;; disable spell checking for the major modes below
(setq wucuo-spell-check-buffer-predicate
    (lambda ()
        (not (memq major-mode
                    '(dired-mode
                    log-edit-mode
                    compilation-mode
                    help-mode
                    profiler-report-mode
                    Info-mode
                    python-mode
                    emacs-lisp-mode
                    ess-mode)))))

;; activity-watch
(global-activity-watch-mode)

