;;              _    _                  _       _ _                   _ _
;;  _ __   ___ | | _| |_ __ _          (_)_ __ (_) |_       _____   _(_) |
;; | '_ \ / _ \| |/ / __/ _` |  _____  | | '_ \| | __|____ / _ \ \ / / | |
;; | | | | (_) |   <| || (_| | |_____| | | | | | | ||_____|  __/\ V /| | |
;; |_| |_|\___/|_|\_\\__\__,_|         |_|_| |_|_|\__|     \___| \_/ |_|_|
;;

(provide 'init-evil)
(defvar LEADER ",")
(defun evil-config ()
  (use-package evil-leader
    :ensure t
    :config (global-evil-leader-mode))
  (use-package evil-magit
    :ensure t)
  (evil-mode 1)

  (evil-leader/set-leader LEADER)
  (evil-leader/set-key "e v" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
  (evil-leader/set-key "e e" 'find-file)
  (evil-leader/set-key "s v" (lambda () (interactive) (eval-buffer (find-file "~/.emacs.d/init.el"))))
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "Q" 'evil-quit))

(use-package evil
  :ensure t
  :config
  (evil-config))

