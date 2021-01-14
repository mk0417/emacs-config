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
(with-eval-after-load 'company
  (setq company-minimum-prefix-length 1))

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
;; my empirical root
(defun p-setup-empirical-environment ()
  (interactive)
  (when (ffip-current-full-filename-match-pattern-p "Git")
    (setq-local ffip-use-rust-fd nil)
    ;; Though PROJECT_DIR is team's project, I care only its sub-directory "subproj1""
    (setq-local ffip-project-root "~/Git")
    ;; well, I'm not interested in concatenated BIG js file or file in dist/
    (setq-local ffip-find-options "-follow")
    ;; for this project, I'm only interested certain types of files
    ;; (setq-local ffip-patterns '("*.html" "*.js" "*.css" "*.java" "*.xml" "*.js"))
    ;; ignore files whose name match certain glob pattern
    (setq-local ffip-ignore-filenames '("*.pdf"))
    ;; exclude `dist/' directory
    (add-to-list 'ffip-prune-patterns "*/research_project"))
  ;; insert more WHEN statements below this line for other projects
  )
;; most major modes inherit from prog-mode, so below line is enough
(add-hook 'prog-mode-hook 'p-setup-empirical-environment)

;; projectile
(projectile-mode 1)
(setq projectile-project-search-path '("~/Git/" "~/project/"))

;; update tags automatically
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      'counsel-etags-virtual-update-tags 'append 'local)))

;; popwin
(popwin-mode 1)

;; lsp-mode
;; performance improvements: @see https://emacs-lsp.github.io/lsp-mode/page/performance/
;; (with-eval-after-load 'lsp-mode
;;   ;; If use pyls: find executable pyls otherwise pyls is not on path
;;   (setq lsp-pyls-server-command "~/anaconda3/bin/pyls")
;;   (add-hook 'python-mode-hook #'lsp-deferred)
;;   (setq lsp-enable-file-watchers nil)
;;   ;; @see https://github.com/emacs-lsp/lsp-mode/blob/master/docs/tutorials/how-to-turn-off.md
;;   (setq lsp-modeline-code-actions-enable nil)
;;   (setq lsp-modeline-diagnostics-enable nil)
;;   ;; @see https://github.com/emacs-lsp/lsp-mode/blob/db2e03738d24085213908076cd0e9a4cbeecf13d/docs/tutorials/PYLS-guide.md
;;   (setq lsp-print-io nil)
;;   (setq lsp-pyls-plugins-jedi-completion-fuzzy nil)
;;   (setq lsp-pyls-plugins-pylint-enabled nil)
;;   ;; enable log only for debug
;;   (setq lsp-log-io nil)
;;   ;; use `evil-matchit' instead
;;   (setq lsp-enable-folding nil)
;;   ;; no real time syntax check
;;   (setq lsp-diagnostic-package :none)
;;   ;; handle yasnippet by myself
;;   (setq lsp-enable-snippet nil)
;;   ;; use `company-ctags' only.
;;   ;; Please note `company-lsp' is automatically enabled if it's installed
;;   (setq lsp-enable-completion-at-point nil)
;;   ;; turn off for better performance
;;   (setq lsp-enable-symbol-highlighting nil)
;;   ;; use find-fine-in-project instead
;;   (setq lsp-enable-links nil)
;;   ;; auto restart lsp
;;   (setq lsp-restart 'auto-restart)
;;   ;; don't watch 3rd party javascript libraries
;;   (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored)
;;   ;; don't ping LSP language server too frequently
;;   (defvar lsp-on-touch-time 0)
;;   (defun my-lsp-on-change-hack (orig-fun &rest args)
;;     ;; do NOT run `lsp-on-change' too frequently
;;     (when (> (- (float-time (current-time))
;;                 lsp-on-touch-time) 120) ;; 2 mins
;;       (setq lsp-on-touch-time (float-time (current-time)))
;;       (apply orig-fun args)))
;;   (advice-add 'lsp-on-change :around #'my-lsp-on-change-hack))

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

;; from https://blog.sumtypeofway.com/posts/emacs-config.html
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; evil-goggles
(evil-goggles-mode)

;; functions
;; https://stackoverflow.com/questions/2951797/wrapping-selecting-text-in-enclosing-characters-in-emacs
(defun p-surround-parens ()
  (interactive)
  (if (region-active-p)
      (progn
        (insert-pair 1 ?\( ?\))
        (backward-char))
    (progn
      (forward-sexp)
      (backward-sexp)
      (mark-sexp)
      (insert-pair 1 ?\( ?\)))))

(defun p-surround-brackets ()
  (interactive)
  (if (region-active-p)
      (progn
        (insert-pair 1 ?\[ ?\])
        (backward-char))
    (progn
      (forward-sexp)
      (backward-sexp)
      (mark-sexp)
      (insert-pair 1 ?\[ ?\]))))

(defun p-surround-curly ()
  (interactive)
  (if (region-active-p)
      (progn
        (insert-pair 1 ?\{ ?\})
        (backward-char))
    (progn
      (forward-sexp)
      (backward-sexp)
      (mark-sexp)
      (insert-pair 1 ?\{ ?\}))))

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
(defun p-find-file-in-private-config ()
  (interactive)
  (counsel-find-file "~/.emacs.p"))

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

;; counsel find my literature
(defun p-counsel-find-literature ()
  (interactive)
  (counsel-find-file "~/Dropbox/roam_literature"))

;; dired open my literature
(defun p-dired-jump-literature ()
  (interactive)
  (dired "~/Dropbox/roam_literature"))

;; delete to tab
(defun p-delete-backward-to-tab ()
  (interactive)
  (kill-line 0)
  (insert "    "))

;; select functions
(defun p-select-function ()
  (interactive)
  (beginning-of-defun)
  (evilmi-select-items))

;; select block between blank lines: Xah Lee
;; http://ergoemacs.org/emacs/modernization_mark-word.html
(defun p-select-block ()
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move"))))

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

(provide 'init-p-config)
