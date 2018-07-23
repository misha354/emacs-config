;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)



; requires sbcl
;(setq enable-common-lisp t) 

;you'll want either racket, chicken, etc installed
;(setq enable-scheme t) 

;;;; PACKAGE MANAGEMENT
;;package repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; this installs use-package if it's not present (i.e. clean installs)

(setq package-list '(spacemacs-theme ansible neotree))

; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))

; use package
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

;;;; magit
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

;;;; SYSTEM OVERRIDES
;; no splash intro
(setq inhibit-splash-screen t)
;;let emacs use plenty of memory
(setq gc-cons-threshold 100000000)

;;;; USER SETTINGS
;; theme - monokia is dark, like Sublime. Leuven is light. uncomment what you'd like to use,
;; or find more at https://pawelbx.github.io/emacs-theme-gallery/
;(use-package monokai-theme
;  :ensure t
;  :init (load-theme 'monokai t))

;(use-package leuven-theme
;  :ensure t
;  :init (load-theme 'leuven t))

(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))


;; project management
(use-package projectile
  :ensure t
  :init (projectile-mode t))
(use-package ido
  :ensure t
  :init (ido-mode t) (ido-everywhere t))
(use-package helm
  :ensure t
  :init (helm-mode t))
(use-package org
  :ensure t
  :init (setq org-todo-keywords
	      '((sequence "TODO(t)" "CURRENT(c!)" "HOLD(h@/!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(x@/!)"))))

(use-package yaml-mode
  :ensure t)

;; code editing
(use-package smartparens
  :ensure t
  :init (smartparens-global-mode 1))
(use-package flyspell
  :ensure t)
(use-package company
  :ensure t ; start company-mode completion in all but php buffers
  :init (global-company-mode t) (setq company-global-modes '(not php-mode)))

;; HTML
(use-package company-web
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
    (yas-global-mode 1))

;; ansible mode
(use-package ansible
  :init (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))


;; Common Lisp
;(if enable-common-lisp
;    (progn
;      (use-package paredit
;	:ensure t)
;      (use-package slime
;	:ensure t)    
;      (add-hook 'slime-mode-hook
;		'(lambda ()
;		   (paredit-mode t)
;		   (setq inferior-lisp-program (locate-file "sbcl" exec-path))
;		   (setq slime-controls '(slime-fancy))))))

;; Scheme
;(if enable-scheme
;    (progn
;      (use-package geiser
;	:ensure t)
;      (use-package ac-geiser
;	:ensure t)
;      (add-hook 'geiser-mode-hook 'ac-geiser-setup)
;      (add-hook 'geiser-repl-mode-hook
;		'(lambda ()
;			(ac-geiser-setup)
;			(add-to-list 'ac-modes 'geiser-repl-mode)))))

(use-package rust-mode
  :ensure t)
(use-package flycheck-rust
  :ensure t)
(add-hook 'rust-mode-hook '(lambda ()
	  (flycheck-rust-setup)
	  (flycheck-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (pbcopy ansible company-ansible neotree magit company-erlang company-web web-mode vue-mode visual-regexp undo-tree tabbar smex smartparens slime rust-mode paredit monokai-theme jedi ivy ido-ubiquitous icicles helm-swoop ggtags geiser geben-helm-projectile flycheck-rust direx dired+ company-php ac-python ac-php))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(save-place-mode 1) 
(global-linum-mode 1)
(setq column-number-mode t)
(setq large-file-warning-threshold nil)

;; Store backups in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-hook 'c-mode-common-hook 
          (lambda () (define-key c-mode-base-map (kbd "C-c C-l") 'compile)))

(define-key global-map "\M-*" 'pop-tag-mark)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq-default fill-column 80)
(put 'upcase-region 'disabled nil)


(defun neotree-startup ()
  (interactive)
  (neotree-show)
  (call-interactively 'other-window))

(if (daemonp)
    (add-hook 'server-switch-hook #'neotree-startup)
  (add-hook 'after-init-hook #'neotree-startup)
)


(add-to-list 'company-backends 'company-ansible)


(setq sentence-end-double-space nil)

 (global-set-key (kbd "C-x c") 'helm-apropos)

;;(setq helm-google-suggest-use-curl-p t)

;;; .emacs ends here


(setq auto-mode-alist (delete (rassq 'git-rebase-mode auto-mode-alist) auto-mode-alist))

;(defun interprogram-paste-function () ; Interactive version
;  "Paste from OS clipboard"
;  (interactive nil)
;  (shell-command-to-string "pbpaste"))

;(defun interprogram-cut-function (text &optional push)
;  "Copy to OS clipboard"
;  (interactive "p")
;  (let ((process-connection-type nil))
;    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;      (process-send-string proc text)
;      (process-send-eof proc))));

;(global-set-key (kbd "C-c C-c") 'interprogram-cut-function)
;(global-set-key (kbd "C-c C-y") 'interprogram-paste-function)

