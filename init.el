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
(custom-set-variables
 '(auto-save-file-name-transforms `((".*" ,my-temp-dir t)))
 '(backup-directory-alist `((".*" . ,my-temp-dir))))
(setq create-lockfiles nil)

;;
;; Set up load-path
;;

(let ((default-directory "~/.emacs.d/elisp/"))
  (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path))

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

