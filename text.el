;; -*- coding: utf-8; lexical-binding: t; -*-

;; Org mode ---------------------------------------------------------------
(setq org-agenda-files (quote ("~/org/agenda"))
      org-agenda-breadcrumbs-separator " ❱ "
      org-directory "~/org"
      org-confirm-babel-evaluate nil
      org-agenda-tags-column 0
      org-html-htmlize-output-type 'css
      org-roam-directory "~/org/roam"
      org-roam-db-location "~/org/roam/org-roam.db"
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
  (add-hook 'after-init-hook 'org-roam-mode)
  ;; org-habit
  (add-to-list 'org-modules 'org-habit)
  ;; org python
  (require 'jupyter)
  (require 'ob-jupyter)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (jupyter . t)))
  ;; source block template
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("b" . "src shell"))
  (add-to-list 'org-structure-template-alist '("y" . "src python :session py :results output"))
  (add-to-list 'org-structure-template-alist '("p" . "src elisp"))
  ;; org capture template
  ;; https://protesilaos.com/dotemacs/#h:7b88b89a-6eb3-4da3-a9fe-0e447300a250
  (setq org-capture-templates
        '(("m" "Meeting and event" entry
           (file+headline "meeting.org" "Meetings, events, and appointments")
           "* %^{Scope of meeting|Event: |Research discussion: |Staff meeting: |Student meeting: } %^{Title} %^g\nSCHEDULED: %^t\n")
          ("t" "TODO" entry
           (file+headline "todo.org" "Todo and task")
           "* TODO %^{Title} \nSCHEDULED: %^t\n")
          ("r" "Routine" entry
           (file+headline "routine.org" "Routine")
           "* TODO %^{Title} \nSCHEDULED: %^t\n :PROPERTIES:\n :STYLE:    habit\n :END:\n")))
  ;; export to latex beamer
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
                  "\\end{frame}"))))

