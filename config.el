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

;; world time
(setq display-time-world-list
  '(("America/Los_Angeles" "Seattle")
    ("America/New_York" "New York")
    ("Europe/London" "London")
    ("Europe/Paris" "Paris")
    ("Asia/Shanghai" "Shanghai")
    ("Asia/Tokyo" "Tokyo")
    ("Pacific/Auckland" "Auckland")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

;; spell-fu
;; (setq spell-fu-directory "~/.spell-fu")

;; set up aspell path, otherwise wucuo does not work
(setq ispell-program-name "/usr/local/bin/aspell")

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


;; functions
;; https://stackoverflow.com/questions/2951797/wrapping-selecting-text-in-enclosing-characters-in-emacs
(defun p-surround-parens ()
  (interactive)
  (if (region-active-p)
      (insert-pair 1 ?\( ?\))
    (backward-char)))

(defun p-surround-brackets ()
  (interactive)
  (if (region-active-p)
      (insert-pair 1 ?\[ ?\])
    (backward-char)))

(defun p-surround-curly ()
  (interactive)
  (if (region-active-p)
      (insert-pair 1 ?{ ?})
    (backward-char)))

;; https://emacs.stackexchange.com/questions/54659/how-to-delete-surrounding-brackets
(defun p-delete-parens ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (let ((beg (point)))
      (forward-list)
      (delete-backward-char 1)
      (goto-char beg)
      (delete-char 1))))

;; ex-evil replace
(defun p-ex-evil-buffer-replace ()
  (interactive)
  (evil-ex (concat "%s/")))

(defun p-ex-evil-selection-replace ()
  (interactive)
  (evil-ex (concat "'<,'>s/")))

;; switch to scratch buffer
(defun p-switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;; find file in my private config
(require 'counsel)
(defun p-find-file-in-private-config ()
  (interactive)
  (counsel--find-file-1
   "Find file: " "~/.emacs.p"
   #'counsel-find-file-action
   'counsel-find-file))

;; insert date
(defun p-insert-uk-date ()
  (interactive)
  (insert (format-time-string "%d-%m-%Y")))

(defun p-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; insert current buffer name
(defun p-insert-file-name ()
  (interactive)
  (insert (buffer-file-name)))

;; backward kill to the beginning of line
(defun p-kill-to-begin-of-line ()
  (interactive)
  (kill-line 0))

;; google search
;; https://emacsredux.com/blog/2013/03/28/google/
(defun p-google-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;; youtube search
;; https://emacsredux.com/blog/2013/08/26/search-youtube/
(defun p-youtube-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))

;; open using external app in dired
;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun p-open-in-external-app (&optional @fname)
  (interactive)
  (let* (($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" $fpath))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))
         $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath)))
         $file-list))))))

