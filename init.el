;; default settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(scroll-bar-mode 0)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq delete-auto-save-files t)
(save-place-mode 1)
(setq inhibit-startup-message t)

(savehist-mode 1)

;; message log
(setq message-log-max 5000)

;; 4 lsp performance
(setq read-process-output-max (* 2 1024 1024))
(setq gc-cons-threshold 100000000)

;; ;; for mac to run shell env
;; (when (memq window-system '(mac ns x))
;;   (progn (exec-path-from-shell-initialize)
;; ;; set meta key to command
;;   (setq mac-command-modifier 'meta)))

;; frame-size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))
(set-frame-size-according-to-resolution)

;; encoding
(set-language-environment 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; global key-bindings
(global-set-key (kbd "M-:") 'comment-dwim)


;; kill whole line
(setq kill-whole-line t)

;; display line number
(progn
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'relative)
  (set-face-attribute 'line-number-current-line nil
		      :foreground "gold"))
(setq column-number-mode t)

(defvar kang/indent-width 2)
(setq indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'default-tab-width)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))

;; ;; Garbbage collection magic hack
;; (use-package gcmh
;;   :ensure t
;;   :init (gcmh-mode 1))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package undo-tree
  :ensure)

(add-to-list 'custom-theme-load-path "~/.emacs.d/everforest")
(load-theme 'everforest-hard-dark t)
(set-frame-font "Sarasa Mono J-18")


(use-package material-theme
  :ensure t
  :config ;(load-theme 'material-light t)
  (set-frame-font "Sarasa Mono J-18"))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config ;(load-theme 'sanityinc-tomorrow-day t)
  (set-frame-font "Sarasa Mono J-18"))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  ;(load-theme 'doom-tokyo-night t)
  (set-frame-font "Sarasa Mono J-18"))

;; ivy-mode
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq enable-recursive-minibuffers t))

;; swipe
(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

;; smartparens
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode))

;; Ido mode
(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; magit
(use-package magit
  :ensure t
  :config (global-set-key (kbd "C-x g") 'magit-status))

(setq-default c-basic-offset kang/indent-width)
;; c-mode-hook
(add-hook 'c-mode-hook (lambda()
			    (setq-default)
			    (setq tab-width 8)
			    (setq standard-indent 8)
			    (setq ident-tabs-mode nil)))

;; terminal-emulator
;; installed vterm-module in home.nix
(use-package vterm
  :ensure t)

;; ;; evil-mode
;; (use-package evil
;;   :ensure t
;;   :config (evil-mode 1))

;; (use-package key-chord
;;   :ensure t
;;   :config (setq key-chord-two-keys-delay 0.5)
;;   (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
;;   (key-chord-mode 1))


;; clang-format
(use-package clang-format
  :ensure t
  :after (cc-mode)
  :config
  (set-default 'clang-format-style "Google")
  (add-hook 'c-mode-common-hook #'(lambda()
                                    (add-hook 'before-save-hook
                                              'clang-format-buffer t t))))

;; spell checking
(use-package flyspell
  :config
  (add-hook 'c-mode-hook '(lambda()
			    (flyspell-prog-mode)))
  (add-hook 'c++-mode-hook '(lambda()
			      (flyspell-prog-mode)))
  (add-hook 'go-mode-hook '(lambda()
			     (flyspell-prog-mode)))
  (add-hook 'rust-mode-hook '(lambda()
				 (flyspell-prog-mode)))
  (add-hook 'python-mode-hook '(lambda()
				 (flyspell-prog-mode))))


;; company-mode
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 1
	company-selection-wrap-around t
	company-show-numbers t
	company-tooltip-limit 12
	)
  (setq company-tooltip-minimum-width 60)
  (setq company-tooltip-margin 60)
  (setq company-tooltip-align-annotations t)
  :bind
  (:map company-active-map
	("C-n" . company-select-next)
	("C-p" . company-select-previous)
	("M-<" . company-select-first)
	("M->" . company-select-last)
	("C-;" . company-complete-common)))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors '(:candidate "purple" :annotation "#7C4Dff"
						 :select (:background "purple" :foreground "white")))
  (add-to-list 'company-box-frame-parameters '(font . "Iosevka-14")))


;; eglot lsp
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls")))
  (add-hook 'eglot--managed-mode-hook (lambda() (flymake-mode -1)))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  ;; format on save
  ;; (add-hook 'c-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  ;; (add-hook 'c++-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (add-hook 'python-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-format))


;; go-mode
(use-package go-mode
  :ensure t
  :defer t
  :mode ("\\.go$" . go-mode)
  :config
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook (lambda()
			    (setq-default)
			    (setq tab-width 4)
			    (setq standard-indent 4)
			    (setq ident-tabs-mode nil)))
  (add-hook 'before-save-hook 'gofmt-before-save))


;; rust-mode
(use-package rust-mode
  :ensure t
  :defer t
  :mode ("\\.rs$" . rust-mode)
  :config
  (setq rust-format-on-save t)
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq-default rustic-format-trigger 'on-save)
  )


(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))


;; typescript-mode
(use-package typescript-mode
  :ensure t
  :defer t
  :mode (("\\.ts\\'" . typescript-mode)))

(use-package tide
  :ensure t
  :defer t
  :config
  (add-hook 'typescript-mode-hook
	    (lambda ()
	      (tide-setup)
	      (flycheck-mode t)
	      (setq flycheck-check-syntax-automatically '(save mode-enabled))
	      (eldoc-mode t)
	      (company-mode-on))))

;; geiser
(use-package geiser-guile
  :ensure t)

;; nix-mode
(use-package nix-mode
  :mode "\\.nix\\'")

;; move text
(use-package move-text
  :ensure t
  :config
  (global-set-key (kbd "M-p") 'move-text-up)
  (global-set-key (kbd "M-n") 'move-text-down))


;; auto-revert
(use-package autorevert
  :config
  ;; also auto refresh dired, but be quiet about it
  (global-auto-revert-mode t)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil))


;; ibuffer
(use-package all-the-icons-ibuffer
  :ensure t
  :config
  (all-the-icons-ibuffer-mode 1))
(use-package ibuffer
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))


;; all the icons
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t)

;; dired-x
(use-package direx
  :after dired
  :defer t
  :commands dired-jump)


;; (use-package yasnippet
;;   :ensure t
;;   :init (yas-global-mode 1))

;; (use-package yasnippet-snippets
;;   :defer t
;;   :after yasnippet)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))
 '(package-selected-packages
   '(doom-themes magit sanityinc-tomorrow-day solarized-theme material-theme color-theme-sanityinc-tomorrow key-chord organic-green-theme undo-tree everforest-theme everforest powerline-evil powerline evil vterm cl clang-format monokai-pro-theme nix-mode darkokai-theme darkokai gruvbox-theme yasnippet-snippets yasnippet tide typescript-mode all-the-icons-dired all-the-icons-ibuffer gcmh move-text zenburn-theme darcula-theme darcula zenburn exec-path-from-shell company-box python-black go-mode dracula-theme which-key try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column-indicator ((t (:foreground "gray80" :weight normal))))
 '(multi-magit-repo-heading ((t (:inherit magit-section-heading :box nil))))
 '(speedbar-selected-face ((t (:foreground "#008B45" :underline t)))))
(put 'upcase-region 'disabled nil)
