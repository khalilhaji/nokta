;;              _    _                                                      _
;;  _ __   ___ | | _| |_ __ _             ___ _ __ ___   __ _  ___ ___   __| |
;; | '_ \ / _ \| |/ / __/ _` |  _____    / _ \ '_ ` _ \ / _` |/ __/ __| / _` |
;; | | | | (_) |   <| || (_| | |_____|  |  __/ | | | | | (_| | (__\__ \| (_| |
;; |_| |_|\___/|_|\_\\__\__,_|         (_)___|_| |_| |_|\__,_|\___|___(_)__,_|
;;

(require 'package)
;; activate package manager
(package-initialize)

(setq-default cursor-type 'bar)

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
(setq default-tab-width 2)

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
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq-default neo-show-hidden-files t))

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

(use-package all-the-icons
  :ensure t)

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t
;; 	doom-themes-enable-italic t)
;;   (load-theme 'doom-one t))

(use-package racket-mode
  :ensure t
  :config
  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable))

(use-package hacker-typer
  :ensure t)

(setq org-html-postamble nil)
(use-package org-bullets
  :ensure t
  :defer t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package smartparens-config
  :ensure smartparens
  :config (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'smartparens-mode))

(sp-local-pair 'rust-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
(sp-local-pair 'js-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET")))


;; Allow undoing
(winner-mode 1)

(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list)))

(bind-key "C-x K" 'nuke-all-buffers)

;; Completion
(use-package company
  :ensure t
  :init (global-company-mode 1)
  :bind (:map company-mode-map ("<C-tab>" . company-complete))
  :config (add-hook 'after-init-hook 'global-company-mode))

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
  :bind (("C-c g" . magit-status)))

(use-package esup
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t))


;; Books
(use-package nov
  :ensure t)


;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" default)))
 '(package-selected-packages
   (quote
    (floobits floobits-emacs yasnippet-snippets yasnippet doom-themes web-mode docker-compose-mode docker dockerfile-mode vue-mode haskell-mode nov mu4e-alert mu4e scribble-mode rainbow-delimiters doom-modeline doom-city-lights-brighter-modeline neotree all-the-icons evil-magit evil-leader evil esup magit racer cargo rust-mode company smartparens org-bullets hacker-typer racket-mode gruvbox-theme counsel which-key try projectile use-package)))
 '(send-mail-function (quote smtpmail-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
