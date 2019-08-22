(require 'package)

(package-initialize)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))


(setq package-list
  '(ace-window
    cider
    clojure-mode
    company
    company-go
    csv-mode
    dockerfile-mode
    ein
    elpy
    evil
    exec-path-from-shell
    flycheck
    graphviz-dot-mode
    go-autocomplete
    go-eldoc
    go-guru
    go-mode
    helm
    inf-clojure
    jedi
    jeison
    js2-mode
    json-mode
    magit
    markdown-mode
    mocha
    paredit
    prettier-js
    rjsx-mode
    restclient
    scala-mode
    slime
    solarized-theme
    tide
    typescript-mode
    use-package
    web-mode))

;; todo this doesn't happen when necessary
(unless package-archive-contents
  (package-refresh-contents))
(package-refresh-contents)

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
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control))

(setq inhibit-startup-message t) ;; hide the startup message
;; https://www.emacswiki.org/emacs/NoTabs
(setq-default indent-tabs-mode nil) ;; turn tabs into spaces
(setq split-width-threshold 1) ;; set split screen to be vertical


;; global key bindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)
;; used to be connected to tab-to-tab stop
(global-set-key (kbd "M-i") 'imenu)


;; Javascript Configuration
(setq js-indent-level 2)

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

(require 'cider)
;;https://markhudnall.com/2016/04/25/starting-figwheel-in-emacs/
(setq cider-lein-parameters "repl :headless :host localhost")
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")
         
(require 'clojure-mode)

(require 'company)
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'ein)
(require 'ein-loaddefs)
(require 'ein-notebook)
(require 'ein-subpackages)

(require 'elpy)
(elpy-enable)

(require 'evil)
(evil-mode t)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "M-.") nil))

(add-hook 'eww-mode-hook 'visual-line-mode)

(require 'flycheck)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'graphviz-dot-mode)
(require 'go-mode)
; pkg go installation
(setq exec-path (append '("$GOPATH/bin") exec-path))
(setenv "PATH" (concat "/opt/go/bin:" (getenv "PATH")))

(defun init-go-mode ()
  ; https://johnsogg.github.io/emacs-golang
  ; golang installed deps:
  ; - uses exec-path-from-shell to get GOPATH variable (maybe)
  ; $ go get github.com/rogpeppe/godef
  ; $ go get golang.org/x/tools/cmd/goimports
  ; $ go get -u github.com/nsf/gocode
  ; $ go get -u github.com/dougm/goflymake
  ; $ go get -u golang.org/x/tools/cmd/guru
  ; $ export GOROOT="" // readlink -n $(which go)
  ; $ export GOPATH="$HOME/go_wrkspc"
  ; $ export PATH="$GOPATH/bin:$PATH"

  (setq tab-width 2 indent-tabs-mode 1)
  (add-to-list 'load-path "~/go_wrkspc/src/github.com/dougm/goflymake")
  (require 'go-flymake)
  (require 'go-flycheck)

  ; eldoc shows the signature of the function at point in the status bar.
  (go-eldoc-setup)

  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  (local-set-key (kbd "M-.") #'godef-jump)
  (add-hook 'before-save-hook #'gofmt-before-save)

  (add-hook 'completion-at-point-functions 'go-complete-at-point)

  ; Customize compile command to run go build
  ; (if (not (string-match "go" compile-command))
  ;     (set (make-local-variable 'compile-command)
  ;          "go build -v && go test -v && go vet"))
  ;(add-hook 'after-save-hook (lambda () (flycheck-compile 'go-build)))
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)

  ; extra keybindings from https://github.com/bbatsov/prelude/blob/master/modules/prelude-go.el
  ; (let ((map go-mode-map))
  ;   (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
  ;   (define-key map (kbd "C-c m") 'go-test-current-file)
  ;   (define-key map (kbd "C-c .") 'go-test-current-test)
  ;   (define-key map (kbd "C-c b") 'go-run))
)

(add-hook 'go-mode-hook (lambda ()
  (require 'company)                           ; load company mode
  (require 'company-go)

  (setq tab-width 2 indent-tabs-mode 1)
  (setq truncate-lines t)
  (add-to-list 'load-path "~/go_wrkspc/src/github.com/dougm/goflymake")
  (require 'go-flymake)
  (require 'go-flycheck)
  (require 'go-guru)

  (go-guru-hl-identifier-mode)


  ; eldoc shows the signature of the function at point in the status bar.
  (go-eldoc-setup)

  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  (local-set-key (kbd "M-.") #'godef-jump)
  (add-hook 'before-save-hook #'gofmt-before-save)

  (add-hook 'completion-at-point-functions 'go-complete-at-point)

  ; Customize compile command to run go build
  ; (if (not (string-match "go" compile-command))
  ;     (set (make-local-variable 'compile-command)
  ;          "go build -v && go test -v && go vet"))
  ;(add-hook 'after-save-hook (lambda () (flycheck-compile 'go-build)))
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)

  ; extra keybindings from https://github.com/bbatsov/prelude/blob/master/modules/prelude-go.el
  ; (let ((map go-mode-map))
  ;   (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
  ;   (define-key map (kbd "C-c m") 'go-test-current-file)
  ;   (define-key map (kbd "C-c .") 'go-test-current-test)
  ;

))

(require 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

(require 'inf-clojure)

;; note this library requires virutalenv
;; start with M-x jedi:install-server
(require 'jedi)
(setq jedi:complete-on-dot t)

;; https://github.com/SavchenkoValeriy/jeison
;; https://news.ycombinator.com/item?id=20448753
(require 'jeison)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'json-mode)
(add-hook 'json-mode-hook #'flycheck-mode)

;; npm install jsonlint -g
; (add-hook 'json-mode-hook 'flymake-json-load)

(require 'magit)

(require 'markdown-mode)
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))
  ;:init (setq markdown-command "multimarkdown"))

;; python-mode
;; https://www.emacswiki.org/emacs/IndentingPython
(add-hook 'python-mode-hook
    (lambda ()
      (setq-default indent-tabs-mode t)
      (setq-default tab-width 2)
      (setq-default py-indent-tabs-mode t)
      (jedi:setup)
      (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lispparedit-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(require 'prettier-js)
(setq prettier-js-args '(
  "--print-width" "100"
  "--trailing-comma" "es5"
  "--tab-width" "2"
  "--bracket-spacing" "true"
  "--arrow-parens" "avoid"
  "--use-tabs" "false"
  "--single-quote"
))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.(j|t)sx?\\'" . prettier-js-mode))))

(require 'restclient)

(require 'scala-mode)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(require 'slime)
(setq inferior-lisp-program "/usr/bin/sbcl")
;(setq temporary-file-directory "/tmp") (add-to-list 'load-path "/tmp/")
(setq slime-contribs '(slime-fancy))
(setq inferior-lisp-program "sbcl")

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

(require 'tls)

;;tramp
(setq tramp-default-method "ssh")

(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(require 'web-mode)

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
 '(ein:jupyter-default-notebook-directory "/home/sebastian/developer/jupyter-notebooks/")
 '(package-selected-packages
   (quote
    (markdown-preview-mode magit markdown-mode+ markdown-mode tide solarized-theme use-package evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Utility functions
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))


;; Garbage almost
; (defun setup-tide-mode ()
;   (interactive)
;   (tide-setup)
;   (flycheck-mode +1)
;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;   (eldoc-mode +1)

;   (tide-hl-identifier-mode +1)
;   ;; company is an optional dependency. You have to
;   ;; install it separately via package-install
;   ;; `M-x package-install [ret] company`
;   (company-mode +1))

;   ;; aligns annotation to the right hand side
;   (setq company-tooltip-align-annotations t)

;   ;; formats the buffer before saving
;   ;;(add-hook 'before-save-hook 'tide-format-before-save)
;   (add-hook 'typescript-mode-hook 'prettier-js-mode)

;   (add-hook 'typescript-mode-hook #'setup-tide-mode)

;   (require 'web-mode)
;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;   (add-hook 'web-mode-hook
;           (lambda ()
;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;               (setup-tide-mode))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq web-mode-markup-indent-offset 2)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (prettier-js-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)

   (setq-default typescript-indent-level 2)
  ;;(add-hook 'before-save-hook #'tide-format-before-save) maybe interferes with pretteir?
  (local-set-key (kbd "M-.") #'tide-jump-to-definition))

(require 'web-mode)
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

;; not sure how this got here
;; (put 'downcase-region 'disabled nil)

;;(server-start)
