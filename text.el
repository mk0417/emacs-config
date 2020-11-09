;; -*- coding: utf-8; lexical-binding: t; -*-

;; Org mode ---------------------------------------------------------------
(setq org-agenda-files (quote ("~/org/" "~/org/journal" "~/org/roam"))
      org-agenda-breadcrumbs-separator " ❱ "
      org-directory "~/org"
      org-confirm-babel-evaluate nil
      org-agenda-tags-column 0
      org-html-htmlize-output-type 'css
      org-roam-directory "~/org-roam"
      org-journal-dir "~/org/journal"
      org-journal-date-format "%A, %d %B %Y"
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-file-type 'monthly)

(with-eval-after-load 'org
  (setq org-superstar-remove-leading-stars t
        org-superstar-headline-bullets-list '("◉" "○" "▷")
        org-superstar-item-bullet-alist
        '((?+ . ?•)
          (?* . ?➤)
          (?- . ?–)))
  (setq org-tree-slide-breadcrumbs nil
        org-tree-slide-header nil
        org-tree-slide-slide-in-effect nil
        org-tree-slide-heading-emphasis nil
        org-tree-slide-cursor-init t
        org-tree-slide-modeline-display nil
        org-tree-slide-skip-done nil
        org-tree-slide-skip-comments t
        org-tree-slide-fold-subtrees-skipped t
        org-tree-slide-skip-outline-level 8
        org-tree-slide-never-touch-face t
        org-tree-slide-activate-message
        (propertize "Presentation mode ON" 'face 'success)
        org-tree-slide-deactivate-message
        (propertize "Presentation mode OFF" 'face 'error))
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (add-hook 'org-mode-hook (lambda () (visual-line-mode)))
  (add-hook 'after-init-hook 'org-roam-mode))

;; https://protesilaos.com/dotemacs/#h:7b88b89a-6eb3-4da3-a9fe-0e447300a250
(setq org-capture-templates
        '(("m" "Meeting and event" entry
           (file+headline "meeting.org" "Meetings, events, and appointments")
           "* %^{Scope of meeting|Staff meeting: |Student meeting: |Event:} %^{Title} %^g\nSCHEDULED: %^t\n")
          ("t" "TODO" entry
           (file+headline "todo.org" "Todo and task")
           "* TODO [#A] %^{Title} \nSCHEDULED: %^t\n")))

(defun p-org-presentation-on ()
  (interactive)
  (progn
    (org-tree-slide-mode 1)
    (olivetti-mode 1)))

(defun p-org-presentation-off ()
  (interactive)
  (progn
    (org-tree-slide-mode -1)
    (olivetti-mode -1)))

(general-create-definer p-org-leader-normal-def
  :prefix ","
  :states 'normal
  :keymaps 'org-mode-map)
(p-org-leader-normal-def
  "n" 'org-tree-slide-move-next-tree
  "p" 'org-tree-slide-move-previous-tree)

;; latex
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  ;; beamer class, for presentations
  '("beamer"
     "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

     ("\\section{%s}" . "\\section*{%s}")
     ("\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}"
       "\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}")))

;; Deft ------------------------------------------------------------------
;; (setq deft-directory "~/org/journal"
;;     deft-use-filename-as-title nil
;;     deft-use-filter-string-for-filename t
;;     deft-auto-save-interval -1
;;     deft-extensions '("org" "txt" "md")
;;     deft-file-naming-rules
;;     '((noslash . "-")
;;     (nospace . "-")
;;     (case-fn . downcase)))

;; olivetti ------------------------------------------------------------------
(setq olivetti-body-width 0.7
      olivetti-minimum-body-width 80
      olivetti-recall-visual-line-mode-entry-state t)
