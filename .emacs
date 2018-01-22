;; Emacs configuration
;; Brian Hu

;; The following is used to automatically install emacs packages
;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

;; List the packages you want
(setq package-list '(auctex
                     magit
                     git-commit
                     magit-popup
                     with-editor
                     dash
                     async
                     py-autopep8
                     zenburn-theme))

;; List the repositories containing them
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; BASIC
;; -------------------------------
;; Hide the startup message
(setq inhibit-startup-message t)

;; Load zenburn theme
(load-theme 'zenburn t)

;; Show line-number and column-number in the mode line
(line-number-mode 1)
(column-number-mode 1)
 
;; Highlight current line
(global-hl-line-mode 1)

;; Mouse scrolling
(mouse-wheel-mode t)


;; PYTHON
;; -------------------------------
;; Load elpy (for Python editing)
(elpy-enable)

;; Fix a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)

;; Python virtual environments (conda)
(setenv "WORKON_HOME" "/home/brianh/miniconda3/envs")
(pyvenv-mode 1)

;; Autopep8 formatting
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;; LATEX
;; -------------------------------
;; Tex options
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Compile as pdf
(setq TeX-PDF-mode t)

;; Forward/Inverse Search
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
 
;; Spellcheck in LaTex mode
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Math mode for LaTex
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; RefTex mode for LaTex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Other useful features
(add-hook 'LaTeX-mode-hook 'visual-line-mode)


;; GIT
;; -------------------------------
;; Magit keybinds
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
