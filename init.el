;; Load my setup
(setq gc-cons-threshold 100000000)
(let ((file-name-handler-alist nil))
  (load (concat (file-name-directory load-file-name) "user.el")))
