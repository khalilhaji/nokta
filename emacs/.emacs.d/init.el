(require 'package)
;; Activate package manager
(package-initialize)
;; initialize melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/lisp/")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(package-selected-packages
   (quote
    (gruvbox-theme smartparens yasnippet org-bullets speed-type company company-mode evil-magit monokai-theme racer cargo edit-indirect tp doom-themes evil-indent-textobject evil-surround evil-leader evil hacker-typer magit rust-mode smartparens-config which-key ace-window ace-windows racket-mode cyberpunk-theme try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Prevent startup message
(setq inhibit-startup-message t)

;; Remove title from title bar
;; (setq frame-title-format nil)

;; Hide toolbar:
(tool-bar-mode -1)


;; y and n mean yes and no
(fset 'yes-or-no-p 'y-or-n-p)
;; Open configuration file.
;; (bind-key "ESC ESC c" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; EVIL MODE:
(require 'init-evil)
(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; (use-package cyberpunk-theme
;;   :ensure t
;;   :config (load-theme 'cyberpunk t))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t doom-themes-enable-italic t)
;;   (load-theme 'doom-city-lights t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-org-config))

;; (use-package monokai-theme
;;   :ensure t
;;   :config (load-theme 'monokai))

(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox-dark-hard))

(use-package racket-mode
  :ensure t
  :config (setq racket-program "/usr/bin/racket") ;; <--- Path to racket installation
  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable))

(use-package hacker-typer
  :ensure t)

(setq org-html-postamble nil)
(use-package org-bullets
  :ensure t
  :defer t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package smartparens-config
  :ensure smartparens
  :config (show-smartparens-global-mode t))
(add-hook 'prog-mode-hook 'smartparens-mode)

;; Allow undoing
(winner-mode 1)

(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list)))

(bind-key "C-x K" 'nuke-all-buffers)


;; Rust setup:
;; Rust mode:
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(use-package cargo
  :ensure t)

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

;; Markdown mode:
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))


;; Git stuff:
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))
