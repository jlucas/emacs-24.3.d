;; Minimal config for Emacs 24.3
;; 24.3 is the version of Emacs in RHEL7 and other older Linux distributions

;; Always load this file as an entry point into my config
(find-file load-file-name)

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
(show-paren-mode 1)
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

(defun my/load-magit ()
  (require 'magit)

  ;; Always show the diff when committing
  (defadvice magit-key-mode-popup-committing
    (after enable-verbose-commit activate)
    (magit-key-mode-toggle-option (quote committing) "--verbose"))

  ;; Replace current buffer with magit status
  ;; https://stackoverflow.com/a/9440613
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq my/load-magit-was-run t))

(global-set-key (kbd "C-x g") (lambda ()
				(interactive)
				(unless (fboundp 'magit-status)
				  (my/load-magit))
				(call-interactively 'magit-status)))

;;
;; git-link
;;

(require 'git-link)
(global-set-key (kbd "C-c g l") 'git-link)

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
;; Windmove
;;

(require 'windmove)

;; Use meta plus HJKL to move around windows.
;; This replaces some default and mode keybinds

;; M-h bound to mark-paragraph (now rebound to C-M-h)
;; C-M-h bound to mark-defun (now unbound)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "C-M-h") 'mark-paragraph)
;; Used to show only files in magit status mode
(add-hook 'magit-mode-hook
          (lambda() (local-unset-key (kbd "M-h"))))

;; M-j and C-M-j bound to indent-new-comment-line
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "C-M-j") 'indent-new-comment-line)

;; M-k bound to kill-sentence (now rebound to C-M-k)
;; C-M-k bound to kill-sexp (now unbound, but kill-sentence has a
;; similar effect under paredit))
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "C-M-k") 'kill-sentence)

;; M-l bound to downcase-word (now rebound to C-M-l)
;; C-M-l bound to reposition-window (now unbound)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "C-M-l") 'downcase-word)

;; Switch to new windows when opening them
(defadvice split-window-right
  (after switch-to-window-right activate)
  (windmove-right))
(defadvice split-window-below
  (after switch-to-window-below activate)
  (windmove-down))

;;
;; Dired
;;

(require 'dired)

;; Use minus to go up a level like netrw in vim
(define-key dired-mode-map (kbd "-") (lambda ()
				       (interactive)
				       (find-alternate-file "..")))

;; Open dired quickly from anywhere
(global-set-key (kbd "C-c d") (lambda ()
                                (interactive)
                                (dired default-directory)))

;;
;; eshell
;;

(setq eshell-prompt-function
      (lambda ()
	"Prompt with Git branch"
	(let ((branch
	       (replace-regexp-in-string
		"\n$" ""
		(shell-command-to-string
		 "git symbolic-ref HEAD 2>/dev/null"))))
	  (concat
	   (abbreviate-file-name (eshell/pwd))
	   (when (> (length branch) 0)
	     (format " (%s)"
		     (replace-regexp-in-string
		      "refs/heads/" ""
		      (propertize branch 'face `(:foreground "orange")))))
	   (if (eq (user-uid) 0) " # " " $ ")))))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier.  From:
http://www.howardism.org/Technical/Emacs/eshell-fun.html"
  (interactive)
  (let* ((parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   default-directory))
	 (height (/ (window-total-height) 3))
	 (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun jl/eshell-quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (progn
	(eshell-life-is-too-much) ; Why not? (eshell/exit)
	(ignore-errors
	  (delete-window)))
    (delete-forward-char arg)))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-d") 'jl/eshell-quit-or-delete-char)))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (mapcar (lambda (x)
		      (add-to-list 'eshell-visual-commands x))
		    '("ssh"
		      "tail"
		      "tig"
		      "irssi"
		      "bitchx"
		      "talk"
		      "ytalk"
		      "mutt"
		      "nano"))))

(global-set-key (kbd "C-c t") 'eshell-here)
(global-set-key (kbd "C-c T") 'eshell)

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
