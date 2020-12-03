;; -*- coding: utf-8; lexical-binding: t; -*-

;; Python ---------------------------------------------------

(with-eval-after-load 'python
  ;; column indicator
  (add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
  ;; enable lsp-mode automatically
  (add-hook 'python-mode-hook (lambda() (lsp)))
  ;; let jupyter-output window on right
  (push '("*jupyter-output*" :position right :width 60 :noselect t) popwin:special-display-config)
  ;; disable warning
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-guess-indent-offset nil))

;; jupyter
;; enable inline outout
(setq jupyter-eval-use-overlays t)

(defun p-jupyter-remove-line-overlay ()
  (interactive)
  (evil-open-below 0)
  (kill-whole-line)
  (evil-escape)
  (previous-line))

;; keybindings
(general-create-definer p-python-leader-normal-def
  :prefix ";"
  :states 'normal
  :keymaps 'python-mode-map)
(p-python-leader-normal-def
  "jj" 'jupyter-run-repl
  "jf" 'jupyter-eval-defun
  "jr" 'jupyter-eval-line-or-region
  "jR" 'jupyter-repl-restart-kernel
  "jC" 'jupyter-repl-clear-cells
  "jI" 'jupyter-repl-interrupt-kernel
  "ji" 'jupyter-inspect-at-point
  "jc" 'p-jupyter-remove-line-overlay
  "jC" 'jupyter-eval-remove-overlays)

(general-create-definer p-python-leader-visual-def
  :prefix ";"
  :states 'visual
  :keymaps 'python-mode-map)
(p-python-leader-visual-def
  "jr" 'jupyter-eval-line-or-region)

(provide 'init-p-prog)