;;; init.el --- Emacs config
;;              _    _                                                      _
;;  _ __   ___ | | _| |_ __ _             ___ _ __ ___   __ _  ___ ___   __| |
;; | '_ \ / _ \| |/ / __/ _` |  _____    / _ \ '_ ` _ \ / _` |/ __/ __| / _` |
;; | | | | (_) |   <| || (_| | |_____|  |  __/ | | | | | (_| | (__\__ \| (_| |
;; |_| |_|\___/|_|\_\\__\__,_|         (_)___|_| |_| |_|\__,_|\___|___(_)__,_|
;;


;;; Commentary:
;;; This is commentary

(require 'package)
;; activate package manager
;;; Code:
(package-initialize)
(global-display-line-numbers-mode)
(add-hook 'shell-mode-hook  (lambda () (display-line-numbers-mode 0)))
(add-hook 'dired-mode-hook (lambda () (display-line-numbers-mode 0)))
(setq column-number-mode t)

;; initialize melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; prevent startup message
(setq inhibit-startup-message t)

;; remove title from title bar
;; (setq frame-title-format nil)

;; hide toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; tab width
;; (setq default-tab-width 2)
(set-default 'tab-width 2)

;; y and n mean yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;; Esc-Esc-c opens config
(bind-key "ESC ESC c" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Disable annoying backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; file manager tree
(use-package all-the-icons
  :ensure t)

;; (use-package centaur-tabs
;;   :ensure t
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   (setq centaur-tabs-style "bar"
;; 	centaur-tabs-set-bar 'left
;; 	centaur-tabs-set-icons t
;; 	centaur-tabs-gray-out-icons 'buffer
;; 	centaur-tabs-set-modified-marker t)

;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

(use-package neotree
  :ensure t
  :hook (neotree-mode . (lambda () (display-line-numbers-mode 0)))
  :config
  (global-set-key (kbd "<C-M-tab>") 'neotree-toggle)
  (setq neo-smart-open t
				projectile-switch-project-action 'neotree-projectile-action
				neo-theme 'icons
				neo-hide-cursor t
				projectile-completion-system 'ivy)
  (setq-default neo-show-hidden-files nil))




(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-switch-project-action 'neotree-projectile-action))

;; Sudo support
(use-package sudo-edit
	:ensure t)


(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config (setq doom-modeline-height 0)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-buffer-modification-icon t))


;; try packages without installing them
(use-package try
  :ensure t)

;; keybinding suggestions
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; counsel mode
(use-package counsel
  :ensure t
  :config (counsel-mode 1))

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)))


(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
				doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t))


(use-package racket-mode
  :ensure t
  :config
  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable))

(use-package hacker-typer
  :ensure t)

(setq org-html-postamble nil)
(use-package org-bullets
  :ensure t
  ;; :defer t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package smartparens-config
  :ensure smartparens
  :config (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'smartparens-mode)
	(sp-local-pair 'rust-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
	(sp-local-pair 'js-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))




;; Allow undoing
(winner-mode 1)

(bind-key "C-x K" 'nuke-all-buffers)

;; Completion
(use-package company
  :ensure t
  :init (global-company-mode 1)
  :bind (:map company-mode-map ("<C-tab>" . company-complete))
	:config (add-hook 'after-init-hook 'global-company-mode))

(use-package company-lsp
	:ensure t
	:config
	(push 'company-lsp company-backends)
	:commands company-lsp)

(use-package lsp-mode
	:ensure t
	:hook ((go-mode . lsp-deferred)
				 (python-mode . lsp-deferred))
	:commands (lsp lsp-deferred)
	:config (setq lsp-enable-symbol-highlighting nil))

(use-package lsp-ui
	:ensure t
	:config (setq lsp-ui-sideline-enable nil
								lsp-ui-doc-enable t
								lsp-ui-imenu-enable t
								lsp-ui-sideline-ignore-duplicate t))

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

;; Haskell:
(use-package haskell-mode
  :ensure t)

;; Go:
(use-package go-mode
  :ensure t
	:config
	(add-hook 'before-save-hook #'gofmt-before-save))

;; Markdown mode:
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
				 ("\\.md\\'" . markdown-mode)
				 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;; Vue:
(use-package vue-mode
  :ensure t)

;; Docker:
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

;; Git stuff:
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
	:config
	(add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package esup
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

;; (use-package yasnippet-snippets
;;   :ensure t)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

;; Books
(use-package nov
  :ensure t)

;; Set default font
(set-face-attribute 'default nil :height 110)

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 (quote
		(org-pdftools sudo-edit yasnippet-snippets which-key vue-mode use-package try smartparens smart-mode-line rjsx-mode racket-mode racer projectile org-bullets nov neotree magit lsp-ui julia-mode haskell-mode hacker-typer floobits esup emmet-mode doom-themes doom-modeline dockerfile-mode docker-compose-mode docker counsel company-lsp company-go centaur-tabs cargo))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
