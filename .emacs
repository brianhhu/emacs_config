;; Emacs configuration
;; Brian Hu

;; The following is used to automatically install emacs packages
;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

;; List the packages you want
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      my-packages
      '(auctex
        company
        eglot
        magit
        ruff-format
        zenburn-theme))

(let (refreshed)
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install package))))


;; BASIC
;; -------------------------------
;; Ensure same path as .bashrc
(defun set-exec-path-from-shell ()
  "Set Emacs PATH from the login shell."
  (let ((path (shell-command-to-string
               "$SHELL --login -c 'printf %s \"$PATH\"'")))
    (setenv "PATH" path)
    (setq exec-path (split-string path path-separator t))))

(set-exec-path-from-shell)

;; Hide the startup message
(setq inhibit-startup-message t)

;; Load zenburn theme
(load-theme 'zenburn t)

;; Show column-number in the mode line
(column-number-mode 1)
 
;; Highlight current line
(global-hl-line-mode 1)

;; Automatically switch to newly created window
(global-set-key (kbd "C-x 2")
                (lambda ()
                  (interactive)
                  (select-window (split-window-below))))

(global-set-key (kbd "C-x 3")
                (lambda ()
                  (interactive)
                  (select-window (split-window-right))))

;; Use windmove to navigate between windows (shift+arrow key)
(windmove-default-keybindings)

;; Open buffer list in current window
(global-set-key "\C-x\C-b" 'buffer-menu)

;; Open shell in current window
(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))

;; Don't delete shell prompt
(setq comint-prompt-read-only t)

;; Disable auto-save-list
(setq auto-save-list-file-prefix nil)

;; ;; Tmux integration
;; (defadvice terminal-init-screen
;;   ;; The advice is named `tmux', and is run before `terminal-init-screen' runs.
;;   (before tmux activate)
;;   ;; Docstring.  This describes the advice and is made available inside emacs;
;;   ;; for example when doing C-h f terminal-init-screen RET
;;   "Apply xterm keymap, allowing use of keys passed through tmux."
;;   ;; This is the elisp code that is run before `terminal-init-screen'.
;;   (if (getenv "TMUX")
;;     (let ((map (copy-keymap xterm-function-map)))
;;     (set-keymap-parent map (keymap-parent input-decode-map))
;; (set-keymap-parent input-decode-map map))))


;; ;; PYTHON
;; ;; -------------------------------
(require 'python)
(require 'eglot)
(require 'company)
(require 'ruff-format)

;; Use ordinary Python for the interactive shell.
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

;; Python REPL toggle
(defvar-local python-source-buffer nil
  "Python source buffer associated with this REPL.")

(defun python-toggle-repl ()
  "Toggle between a Python source buffer and its Python REPL."
  (interactive)
  (cond
   ;; From REPL -> source.
   ((derived-mode-p 'inferior-python-mode)
    (if (buffer-live-p python-source-buffer)
        (pop-to-buffer python-source-buffer)
      (message "No associated Python source buffer.")))

   ;; From source -> REPL.
   ((derived-mode-p 'python-base-mode)
    (let ((source (current-buffer))
          (process (python-shell-get-process)))
      (unless (process-live-p process)
        (run-python (python-shell-calculate-command) nil nil)
        (setq process (python-shell-get-process)))

      (unless (process-live-p process)
        (user-error "Could not start Python REPL"))

      (let ((repl (process-buffer process)))
        (with-current-buffer repl
          (setq python-source-buffer source))

        (if-let ((window (get-buffer-window repl)))
            (select-window window)
          (let ((window (split-window-right)))
            (set-window-buffer window repl)
            (select-window window))))))))

(dolist (map '(python-mode-map
               python-ts-mode-map
               inferior-python-mode-map))
    (when (boundp map)
      (keymap-set (symbol-value map)
                  "C-c C-z"
                  'python-toggle-repl)))

;; Use Pyright for diagnostics, completion, and navigation.
(add-to-list 'eglot-server-programs
	     '(python-base-mode . ("pyright-langserver" "--stdio")))

;; Company completion settings.
(setq company-idle-delay 0.2
      company-minimum-prefix-length 1
      company-selection-wrap-around t)

(add-hook 'eglot-managed-mode-hook 'company-mode)

(defun python-setup ()
  "Use a project's .venv, start Eglot, and format with Ruff."
  (let* ((root (locate-dominating-file default-directory
                                      ".venv"))
         (python (and root
                      (expand-file-name ".venv/bin/python" root))))
    (when (and python (file-executable-p python))
      ;; C-c C-p starts this project's Python interpreter.
      (setq-local python-shell-interpreter python)

      ;; Tell Pyright which environment contains project dependencies.
      (setq-local eglot-workspace-configuration
                  `(:python (:pythonPath ,python)))))

  ;; Ruff formats the buffer before saving.
  (ruff-format-on-save-mode 1)

  ;; Eglot displays Pyright diagnostics through Flymake.
  (eglot-ensure))

(add-hook 'python-base-mode-hook 'python-setup)


;; LATEX
;; -------------------------------
;; Tex options
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-source-correlate-mode t
      TeX-source-correlate-start-server t
      reftex-plug-into-AUCTeX t)

(setq-default TeX-master nil)
 
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
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
