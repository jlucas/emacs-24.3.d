;; Minimal config for Emacs 24.3
;; 24.3 is the version of Emacs in RHEL7 and other older Linux distributions

;;
;; Enabled commands
;;

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;
;; Basic preferences
;;

;; Allow short answers to yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Remove UI elements from GUI mode
;; emacs-nox doesn't have these
(when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1)
    (tool-bar-mode -1))
(menu-bar-mode -1)

;; No splash screen messages
(setq inhibit-startup-message t)

;; Silence description of the *scratch* buffer
(setq initial-scratch-message "")

;; Display cursor line/column number in modeline
(column-number-mode)

;; Save clipboard strings into kill ring before replacing them.
(setq save-interprogram-paste-before-kill t)

;; https://www.emacswiki.org/emacs/DeleteSelectionMode
(delete-selection-mode t)

;; Show matched parens
(setq show-paren-mode 1)
(setq show-paren-delay 0)

;; linum package line number formatting
(setq linum-format "%3d ")
(global-set-key (kbd "C-c n") 'linum-mode)

;;
;; Temp directory, autosaves, and backups
;;

(defconst my-temp-dir (format "~/tmp/emacs-%s.%s/"
			      emacs-major-version
			      emacs-minor-version))
(make-directory my-temp-dir t)
(setq auto-save-file-name-transforms `((".*" ,my-temp-dir t)))
(setq backup-directory-alist `((".*" . ,my-temp-dir)))
(setq create-lockfiles nil)

;;
;; Set up load-path
;;

(let ((default-directory "~/.emacs.d/elisp/"))
  (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path))

;;
;; Set up theme
;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")
(load-theme 'xterm16 t)

;;
;; Set up package
;;

(require 'package)

;; MELPA setup instructions from
;; https://melpa.org/#/getting-started
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Initialize the package package
(package-initialize)

;; Fix a bug where dependencies are returned in reverse order
(defadvice package-compute-transaction
  (before package-compute-transaction-reverse (package-list requirements) activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements))

;;
;; Magit 1.4.2 setup
;;

(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-auto-revert-mode t)
(require 'magit)
(defadvice magit-key-mode-popup-committing
  (after enable-verbose-commit activate)
  (magit-key-mode-toggle-option (quote committing) "--verbose"))
(global-set-key (kbd "C-x g") 'magit-status)

;;
;; Undo-tree 0.7.4 setup
;;

(setq global-undo-tree-mode 1)
(require 'undo-tree)
(global-set-key (kbd "C-c u") 'undo-tree-visualize)

;;
;; Paredit
;;

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;
;; Key binds
;;

;; C-SPC is my tmux prefix!
;; You can get M-SPC #'just-one-space functionality with M-\
;; Git Bash on Windows has an option to disable Alt-Space toggling the window menu
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x C-k") 'kill-region) ; instead of default bind to C-w
                                              ; usually bound to 'edit-kbd-macro
(global-set-key (kbd "C-w") 'backward-kill-word) ; as in the shell, vim. etc.
(global-set-key (kbd "M-%") 'replace-regexp) ; do i ever not want this?

;;
;; Notes
;;

;; Rectangle mark mode (C-x SPC) was added in Emacs 24.4.  In Emacs
;; 24.3 you have to use these older commands to operate on a
;; rectangular selection.
;;
;; * Kill a rectangle: kill-rectangle ‘C-x r k’
;; * Yank the last killed rectangle: yank-rectangle ‘C-x r y’
;; * Copy a rectangle: copy-rectangle-as-kill ‘C-x r M-w’
;; * Insert a text to replace rectangle region: string-rectangle ‘C-x r t’
;; * Insert a text rectangle: string-insert-rectangle (unbound). This
;;   command is almost superfluous, as you can achieve the same result by
;;   marking an “empty rectangle” as described above, and then call
;;   ‘string-rectangle’. The one tiny difference, though, is that
;;   ‘string-insert-rectangle’ will leave point after the rectangle you
;;   have marked. In my case, that does not justify a separate command.
;; * Delete the selected rectangle: delete-rectangle ‘C-x r d’
;; * Insert a whitespace rectangle into the region: open-rectangle ‘C-x r o’
;; * Number lines in rectangle: number-to-register ‘C-x r N’. This
;;   command also inserts a column of spaces after numbers, and can
;;   conveniently be used to convert consecutive lines into a numbered
;;   list. With optional arguments START-WITH and FORMAT (prompted for if
;;   you prefix with C-u), you can number, say, 5 consecutive lines with,
;;   “3.)” to “7.)”.