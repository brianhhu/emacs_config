;; Emacs configuration
;; Brian Hu

;; The following is used to automatically install emacs packages
;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

;; List the packages you want
(setq package-list '(async
                     auctex
                     dash
                     elpy
                     git-commit
                     magit
                     magit-popup
                     py-autopep8
                     with-editor
                     zenburn-theme))

;; List the repositories containing them
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

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

;; Automatically switch to newly created window
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; Use windmove to navigate between windows (shift+arrow key)
(windmove-default-keybindings)

;; Open buffer list in current window
(global-set-key "\C-x\C-b" 'buffer-menu)

;; Open shell in current window
(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))

;; Don't delete shell prompt
(setq comint-prompt-read-only t)


;; PYTHON
;; -------------------------------
;; Load elpy (for Python editing)
(elpy-enable)

;; Python virtual environments (conda)
(setenv "WORKON_HOME" "/home/brianh/miniconda3/envs")
(pyvenv-mode 1)

;; Use ipython as default interpreter
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; Disable warnings
(setq python-shell-completion-native-enable nil)
(setq python-shell-prompt-detect-failure-warning nil)

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
