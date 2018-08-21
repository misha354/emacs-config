;; Aded by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;;; PACKAGE MANAGEMENT
;;package repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq package-list '(use-package spacemacs-theme ansible neotree pbcopy))

; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))

;;;; SYSTEM OVERRIDES
;; no splash intro
(setq inhibit-splash-screen t)
;;let emacs use plenty of memory
(setq gc-cons-threshold 100000000)


;;;; USER SETTINGS
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

(use-package pod-mode
  :ensure t)

;; GENERIC PACKAGES

; project management
(use-package projectile
  :ensure t
  :init (projectile-mode t))

; emacs auto-completion
(use-package ido
  :ensure t
  :init (ido-mode t) (ido-everywhere t))

; emacs autocompletion
(use-package helm
  :ensure t
  :init (helm-mode t))
 (global-set-key (kbd "C-x c") 'helm-apropos)

;;;; magit - version control
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

; Don't start magit if trying to rebase from command line
(setq auto-mode-alist (delete (rassq 'git-rebase-mode auto-mode-alist) auto-mode-alist))

;; tags for code navigation
(use-package ggtags
  :ensure t
  :config 
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1))))
  )
(global-set-key (kbd "C-x r") 'ggtags-find-tag-regexp)
(setq load-path (cons "/usr/local/bin/gtags" load-path))
; allow code navigation for different langauges
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook 'ggtags-mode)
(add-hook 'python-mode-hook 'ggtags-mode)


;; productivity
(use-package org
  :ensure t
  :init (setq org-todo-keywords
	      '((sequence "TODO(t)" "CURRENT(c!)" "HOLD(h@/!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(x@/!)"))))


;; CODE EDITING
(use-package smartparens
  :ensure t
  :init (smartparens-global-mode 1))
(use-package flyspell
  :ensure t)
(use-package company
  :ensure t 
  :init (global-company-mode t) )
(use-package yasnippet
  :ensure t
  :init
    (yas-global-mode 1))
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; LANGUAGE SUPPORT

;; HTML
(use-package company-web
  :ensure t)

;; Mason
(use-package mmm-mode
  :ensure t)

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(add-to-list 'auto-mode-alist '("\\(auto\\|d\\)handler\\'" . html-mode))
(mmm-add-mode-ext-class 'html-mode nil 'mason)
(add-to-list 'auto-mode-alist '("\\.mc\\'" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mc\\'" 'mason)

;; YAML
(use-package yaml-mode
  :ensure t)

;Perl 
(load-library "cperl-mode")
  (add-to-list 'auto-mode-alist '("\\\\.[Pp][LlMm][Cc]?$" . cperl-mode))
  (while (let ((orig (rassoc 'perl-mode auto-mode-alist)))
              (if orig (setcdr orig 'cperl-mode))))
  (while (let ((orig (rassoc 'perl-mode interpreter-mode-alist)))
           (if orig (setcdr orig 'cperl-mode))))
  (dolist (interpreter '("perl" "perl5" "miniperl" "pugs"))
    (unless (assoc interpreter interpreter-mode-alist)
      (add-to-list 'interpreter-mode-alist (cons interpreter 'cperl-mode))))

(global-set-key "\M-p" 'cperl-perldoc) ; alt-p

(require 'pod-mode)
(add-to-list 'auto-mode-alist
  '("\\.pod$" . pod-mode))

(setq cperl-electric-keywords t)

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

; Rust
(use-package rust-mode
  :ensure t)
(use-package flycheck-rust
  :ensure t)
(add-hook 'rust-mode-hook '(lambda ()
	  (flycheck-rust-setup)
	  (flycheck-mode)))


; Convenience settings
(save-place-mode 1) 
(global-linum-mode 1)
(setq column-number-mode t)
(setq large-file-warning-threshold nil)
(put 'upcase-region 'disabled nil)

 (global-set-key (kbd "C-c m") 'menu-bar-open)

; Bind Ctrl+c-c to compile command
(add-hook 'c-mode-common-hook 
          (lambda () (define-key c-mode-base-map (kbd "C-c c") 'compile)))

;; Store backups in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


; Text file settings
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq sentence-end-double-space nil)
(setq-default fill-column 80)



; Random package configurations
(turn-on-pbcopy)


; Show file tree on the left
(defun neotree-startup ()
  (interactive)
  (neotree-show)
  (call-interactively 'other-window))

(if (daemonp)
    (add-hook 'server-switch-hook #'neotree-startup)
  (add-hook 'after-init-hook #'neotree-startup)
)
(setq-default neo-show-hidden-files t)


(global-set-key "\M-o" 'ace-window) ; alt-o

; Custom code
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name))
)
(global-set-key "\C-cz" 'show-file-name)


(defun scp()
(interactive)
(let* (
       (buffer (generate-new-buffer "untitled"))
       (full_path (file-truename (buffer-file-name)))
       (path (substring full_path (length (projectile-project-root)) nil))
       (remote_path (concat "hostname@address:path/" path))
       (args `(,full_path ,remote_path)))

  (progn 
    (with-output-to-temp-buffer "tester"
      (print remote_path)
      )	
    (apply 'start-process "scp" "tester" "scp" args)       
   )

;args
))

(global-set-key (kbd "C-c s") 'scp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (firestarter ace-window pbcopy ansible company-ansible neotree magit company-erlang company-web web-mode vue-mode visual-regexp undo-tree tabbar smex smartparens slime rust-mode paredit monokai-theme jedi ivy ido-ubiquitous icicles helm-swoop ggtags geiser geben-helm-projectile flycheck-rust direx dired+ company-php ac-python ac-php))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
