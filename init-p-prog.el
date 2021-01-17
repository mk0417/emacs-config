;; -*- coding: utf-8; lexical-binding: t; -*-

;; Python ---------------------------------------------------

(with-eval-after-load 'python
  ;; column indicator
  (add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
  ;; enable lsp-mode automatically
  ;; (require 'lsp-mode)
  ;; disable warning
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-guess-indent-offset nil))

;; jupyter
;; enable inline outout
(setq jupyter-eval-use-overlays t)
;; let jupyter-output window on right
(push '("*jupyter-output*" :position right :width 60 :noselect t) popwin:special-display-config)

(defun p-jupyter-remove-line-overlay ()
  (interactive)
  (evil-open-below 0)
  (kill-whole-line)
  (evil-escape)
  (previous-line))

(defun p-jupyter-eval-block ()
    (interactive)
    (p-select-block)
    (let (beg end)
      (setq beg (region-beginning) end (region-end))
      (jupyter-eval-region beg end)))

;; keybindings
(general-create-definer p-python-leader-normal-def
  :prefix ";"
  :states 'normal
  :keymaps 'python-mode-map)
(p-python-leader-normal-def
  "jj" 'jupyter-run-repl
  "jf" 'jupyter-eval-defun
  "jr" 'jupyter-eval-line-or-region
  "je" 'p-jupyter-eval-block
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

;; ESS
(with-eval-after-load 'ess
  (setq ess-ask-for-ess-directory nil))

(general-create-definer p-ess-leader-normal-def
  :prefix ";"
  :states 'normal
  :keymaps 'ess-mode-map)
(p-ess-leader-normal-def
  "jf" 'ess-eval-function
  "jr" 'ess-eval-region-or-line-and-step)

(general-create-definer p-ess-leader-visual-def
  :prefix ";"
  :states 'visual
  :keymaps 'ess-mode-map)
(p-ess-leader-visual-def
  "jr" 'ess-eval-region-or-line-and-step)

(provide 'init-p-prog)
