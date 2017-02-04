;; AucTeX configuration

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Compile as pdf
(setq TeX-PDF-mode t)

;; Mouse scrolling
(mouse-wheel-mode t)
 
;; Spellcheck in LaTex mode
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Math mode for LaTex
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; RefTex mode for LaTex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
 
;; Show line-number and column-number in the mode line
(line-number-mode 1)
(column-number-mode 1)
 
;; Highlight current line
(global-hl-line-mode 1)

;; Other useful features
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

;; Magit keybinds
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
