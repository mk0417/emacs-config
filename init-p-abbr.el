;; -*- coding: utf-8; lexical-binding: t; -*-

;; Credit: Xah Lee
;;   http://ergoemacs.org/emacs/emacs_abbrev_mode.html
;;   http://ergoemacs.org/emacs/emacs_abbrev_mode_tutorial.html
;;   https://github.com/xahlee/xah_emacs_init/blob/master/xah_emacs_abbr.el

(clear-abbrev-table global-abbrev-table)

(defun p-abbrev-hook-function () t)
(put 'p-abbrev-hook-function 'no-self-insert t)

(defun p-global-abbrev-position-cursor (&optional @pos)
  (interactive)
  (let (($found-p (search-backward "▮" (if @pos @pos (max (point-min) (- (point) 100))) t)))
    (when $found-p (delete-char 1))
    $found-p))

(defun p-global-expand-abbrev ()
  (interactive)
  (let ($p1 $p2
            $abrStr
            $abrSymbol)
    (save-excursion
      (forward-symbol -1)
      (setq $p1 (point))
      (forward-symbol 1)
      (setq $p2 (point)))
    (setq $abrStr (buffer-substring-no-properties $p1 $p2))
    (setq $abrSymbol (abbrev-symbol $abrStr))
    (if $abrSymbol
        (progn
          (abbrev-insert $abrSymbol $abrStr $p1 $p2 )
          (p-global-abbrev-position-cursor $p1)
          $abrSymbol)
      nil)))

(setq abbrev-expand-function 'p-global-expand-abbrev)
;; (setq abbrev-expand-function 'abbrev--default-expand)

(define-abbrev-table 'global-abbrev-table
  '(("rdw" "\\([A-Za-z0-9]+\\)" p-abbrev-hook-function)
    ("rbracket" "\\[\\([^]]+?\\)\\]" p-abbrev-hook-function)
    ("rcurly" "“\\([^”]+?\\)”" p-abbrev-hook-function)
    ("rdigit" "\\([0-9]+\\)" p-abbrev-hook-function)
    ("rdate" "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" p-abbrev-hook-function)
    ("rdot" "\\(.\\)" p-abbrev-hook-function)
    ("rstr" "\\([^\"]+?\\)" p-abbrev-hook-function)
    ("rtag" "\\([</>=\" A-Za-z0-9]+\\)" p-abbrev-hook-function)))

;; python
(progn
  (define-abbrev-table 'python-mode-abbrev-table
    '(("pt" "print(▮)" p-abbrev-hook-function)
      ("rt" "return" p-abbrev-hook-function)
      ("eq" "==" p-abbrev-hook-function)
      ("ne" "!=" p-abbrev-hook-function)
      ("le" "<=" p-abbrev-hook-function)
      ("ge" ">=" p-abbrev-hook-function)
      ("rc" "read_csv(▮)" p-abbrev-hook-function)
      ("rp" "read_parquet(▮)" p-abbrev-hook-function)
      ("tc" "to_csv(▮)" p-abbrev-hook-function)
      ("tp" "to_parquet(▮)" p-abbrev-hook-function)
      ("ty" "to_clipboard(▮)" p-abbrev-hook-function)
      ("ap" "apply(▮)" p-abbrev-hook-function)
      ("apl" "apply(lambda x: ▮)" p-abbrev-hook-function)
      ("ri" "reset_index(▮)" p-abbrev-hook-function)
      ("rid" "reset_index(drop=True)" p-abbrev-hook-function)
      ("dp" "drop_duplicates(▮)" p-abbrev-hook-function)
      ("sv" "sort_values(▮, ignore_index=True)" p-abbrev-hook-function)
      ("mg" "merge(▮, how=, on=)" p-abbrev-hook-function)
      ("gp" "groupby(▮)" p-abbrev-hook-function))))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

(provide 'init-p-abbr)