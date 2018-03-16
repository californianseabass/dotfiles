(require 'package)

(package-initialize)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))


(setq package-list
  '(ace-window
    clojure-mode
    company
    company-jedi
    csv-mode
    dockerfile-mode
    elpy
    evil
    flycheck
    helm
    inf-clojure
    magit
    restclient
    solarized-theme
    tide
    typescript-mode
    web-mode))

;; todo this doesn't happen when necessary
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (message " must install %s" package)
    (package-install package)))

(eval-when-compile
  (require 'use-package))

;; BASIC CONFIGURATION

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(setq inhibit-startup-message t) ;; hide the startup message
;; https://www.emacswiki.org/emacs/NoTabs
(setq-default indent-tabs-mode nil) ;; turn tabs into spaces
(setq split-width-threshold 1) ;; set split screen to be vertical


;; global key bindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x l") 'company-jedi)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)
;; used to be connected to tab-to-tab stop
(global-set-key (kbd "M-i") 'imenu)

;; ace-window
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
  (?m aw-swap-window "Swap Windows")
  (?M aw-move-window "Move Window")
  (?j aw-switch-buffer-in-window "Select Buffer")
  (?c aw-split-window-fair "Split Fair Window")
  (?| aw-split-window-vert "Split Vert Window")
  (?b aw-split-window-horz "Split Horz Window")
  (?o delete-other-windows "Delete Other Windows")
  (?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.")


(require 'clojure-mode)

(require 'company)
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'company-jedi)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)


(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


(require 'elpy)
(elpy-enable)

(require 'evil)
(evil-mode t)

(require 'flycheck)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'inf-clojure)

(require 'helm)

(require 'magit)

;; python-mode
;; https://www.emacswiki.org/emacs/IndentingPython
(add-hook 'python-mode-hook
    (lambda ()
      (setq-default indent-tabs-mode t)
      (setq-default tab-width 4)
      (setq-default py-indent-tabs-mode t)
    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(require 'restclient)

;; Solarized
(require 'solarized-theme)
;; https://github.com/sellout/emacs-color-theme-solarized/pull/187
(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit markdown-mode+ markdown-mode tide solarized-theme use-package evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(defun setup-tide-mode ()                                                      
  (interactive)                                                                
  (tide-setup)                                                                 
  (flycheck-mode +1)                                                           
  (setq flycheck-check-syntax-automatically '(save mode-enabled))              
  (eldoc-mode +1)  

  (tide-hl-identifier-mode +1)                                        
  ;; company is an optional dependency. You have to                            
  ;; install it separately via package-install                                 
  ;; `M-x package-install [ret] company`                                       
  (company-mode +1))                                                           

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
