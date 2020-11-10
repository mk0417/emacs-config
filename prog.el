;; -*- coding: utf-8; lexical-binding: t; -*-

;; Python ---------------------------------------------------

(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
  ;; disable warning
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-guess-indent-offset nil))

;; jupyter
(setq jupyter-eval-use-overlays t)

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
  "jI" 'jupyter-interrupt-kernel
  "ji" 'jupyter-inspect-at-point
  "jc" 'jupyter-eval-remove-overlays)

(general-create-definer p-python-leader-visual-def
  :prefix ";"
  :states 'visual
  :keymaps 'python-mode-map)
(p-python-leader-visual-def
  "jr" 'jupyter-eval-line-or-region)

