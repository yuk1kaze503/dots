;; default settings
(setq byte-compile-warnings '(cl-functions))
(setq debug-on-error t)
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
(setq-default fill-column 80)
(savehist-mode 1)
;;(global-set-key (kbd "<mouse-2>") 'clipboard-yank)
;; message log
(setq message-log-max 5000)

;; 4 lsp performance
(setq read-process-output-max (* 2 1024 1024))
(setq gc-cons-threshold 100000000)

;; ;; for mac to run shell env
(when (memq window-system '(mac ns x))
  (progn (exec-path-from-shell-initialize)
;; ;; set meta key to command
(setq mac-command-modifier 'meta)))

;; frame-size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 180))
           (add-to-list 'default-frame-alist (cons 'width 50)))
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
(global-set-key (kbd "<f4>") 'compile)

;; kill whole line
(setq kill-whole-line t)

;; display line number
(progn
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'relative))
   ;; (set-face-attribute 'line-number-current-line nil
   ;; 		      :foreground "gold")
(setq column-number-mode t)

(defvar kang/indent-width 2)
(setq indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'default-tab-width)

;; my lisp functions

;; insert date by %y%m%d %T
(progn
(defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d\" \"%T)")))
(global-set-key "\C-x\M-d" 'insert-current-date))

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
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  ;;(setq highlight-indent-guides-responsive "stack")
  ;; (highlight-indent-guides-method . 'character)
  ;; (highlight-indent-guides-auto-character-face-perc . 20)
  ;; (highlight-indent-guides-character . ?\|)
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package undo-tree
  :ensure t
  :after evil
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history nil)
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

;; ;; everforest theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/everforest-theme")
(load-theme 'everforest-hard-dark t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/tokyo-night-theme")
;;(load-theme 'tokyo t)
;; (add-to-list 'default-frame-alist '(font . "Sarasa Mono J Nerd Font"))
;; (set-face-attribute 'default t :font "Sarasa Mono J Nerd Font-20")
;; (set-frame-font "Sarasa Fixed J Nerd Font-22")
(set-frame-font "ComicShannsMono Nerd Font-22")
;;(set-frame-font "Cica-24")

(use-package modus-themes
  :ensure t
  :config
  ;; (load-theme 'modus-operandi t) ;; Light theme
  ;;(load-theme 'modus-vivendi t)  ;; Dark theme
  )

(use-package catppuccin-theme
  :ensure t
  :config
  ;;(setq catppuccin-flavor 'latte) ;; or 'frappe, 'macchiato, or 'mocha
  ;;(setq catppuccin-flavor 'frappe)
  (setq catppuccin-flavor 'macchiato)
  ;;(setq catppuccin-flavor 'mocha)
  ;; (load-theme 'catppuccin t)
  )

(use-package dracula-theme
  :ensure t
  :config
  ;;(load-theme 'dracula t)
  )

(use-package gruvbox-theme
  :ensure t
  :config
  ;;(load-theme 'gruvbox-dark-soft t)
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  ;;(load-theme 'color-theme-sanityinc-tomorrow-night t)
  )

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

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
  (sp-with-modes 'rust-mode
    (sp-local-pair "'" nil :actions nil))
  (smartparens-global-mode t))
(setq electric-pair-open-newline-between-pairs t)
(electric-indent-mode 1)

;; ido mode
(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; magit
(use-package magit
  :ensure t
  :config (global-set-key (kbd "C-x g") 'magit-status))

(use-package magit-gitflow
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(use-package magit-todos
  :defer t)

(setq-default c-basic-offset kang/indent-width)
;; c-mode-hook
(add-hook 'c-mode-hook (lambda()
			    (setq-default)
			    (setq tab-width 8)
			    (setq standard-indent 8)
			    (setq ident-tabs-mode nil)))
;; auto-sudoedit
(use-package auto-sudoedit
  :ensure t
  :config
  (auto-sudoedit-mode 1))

;; editor config
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; terminal-emulator
;; installed vterm-module in home.nix(nixos)
(use-package vterm
  :ensure t)

;; ;; evil-mode
;; trying agian 2023-04-19 11:43:16
;; failed. 2023-04-21 09:00:34
;; 2023-11-20 
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
  )

(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jh" 'evil-normal-state)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; ;; smart-mode-line
;; (use-package smart-mode-line-atom-one-dark-theme
;;   :ensure t)
;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   (setq sml/no-confirm-load-theme t)
;;   (setq sml/theme 'respectful)
;;   ;(setq sml/theme 'atom-one-dark)
;;   ;(setq sml/thme 'light)
;;   (sml/setup))

;; (use-package mini-echo
;;   :ensure t
;;   :config
;;   (mini-echo-mode 1))    

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-modal t)
  (setq doom-modeline-modal-icon '𝝀)
  (setq doom-modeline-buffer-file-name-style 'identity)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-height 25)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-github nil)
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-time t)
  (display-time-mode 1))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  )

;; Utilities
(use-package s
  :ensure t
  :defer t)

(use-package dash
  :ensure t
  :defer t)

;; (use-package visual-fill-column
;;   :ensure t
;;   :config
;;   (add-hook 'visual-line-mode #'visual-fill-column-mode)
;;   (global-visual-fill-column-mode))

(use-package hl-todo
  :diminish hl-todo
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold)))
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))
;; for mac
(when (memq window-system '(mac ns))
	    (setq mac-command-modifier 'meta))

;; smart-jump
(use-package smart-jump
  :ensure t
  :config
  (smart-jump-setup-default-registers))

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
  :after company-statistics
  :bind (("M-<tab>" . company-complete)
	 :map company-active-map
	 ("M-n" . nil)
	 ("M-p" . nil)
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous)
	 ("C-s" . company-filter-candidates)
	 :map company-search-map
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous))
  :init
  (global-company-mode)
  :config
  (define-key emacs-lisp-mode-map (kbd "C-M-i") nil)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (setq completion-ignore-case t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))
  (custom-set-faces
 '(company-tooltip ((t (:background "#333c43" :foreground "#d3c6aa"))))))

(use-package company-statistics
  :ensure t
  :init
  (company-statistics-mode))

;; (use-package company-dwim
;;   :straight '(company-dwim
;; 	      :type git
;; 	      :host github
;; 	      :repo "zk-phi/company-dwim")
;;   :ensure t
;;   :init
;;   (define-key company-active-map (kbd "TAB") 'company-dwim)
;;   (setq company-frontends
;; 	'(company-pseudo-tooltip-unless-just-one-frontend
;; 	  company-dwim-frontend
;; 	  company-echo-metadata-frontend)))

;; (use-package company-anywhere
;;   :ensure t)

;; (use-package company-same-mode-buffers
;;   :after company
;;   :ensure t
;;   :init
;;   (require 'company-same-mode-buffers)
;;   (company-same-mode-buffers-initialize)
;;   :config
;;   (setq company-backends
;; 	'((company-capf :with company-same-mode-buffers)
;; 	  (company-dabbrev-code :with company-same-mode-buffers)
;; 	  company-keywords
;; 	  company-files
;; 	  company-dabbrev)))

;; (use-package company
;;   :ensure t
;;   :defer t
;;   ;;:init (add-hook 'after-init-hook 'global-company-mode)
;;   :config
;;   (global-company-mode 1)
;;   (setq company-idle-delay 0.0
;; 	company-minimum-prefix-length 1
;; 	company-selection-wrap-around t
;; 	company-show-numbers t
;; 	company-tooltip-limit 12
;; 	)
;;   (setq company-tooltip-minimum-width 60)
;;   (setq company-tooltip-margin 60)
;;   (setq company-tooltip-align-annotations t)
;;   :bind
;;   (:map company-active-map
;; 	("C-n" . company-select-next)
;; 	("C-p" . company-select-previous)
;; 	("M-<" . company-select-first)
;; 	("M->" . company-select-last)
;; 	("C-;" . company-complete-common)))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   ;; (setq company-box-backends-colors
;;   ;; 	'(:candidate "purple" :annotation "#7C4Dff"
;;   ;; 	  :select (:background "purple" :foreground "white")))
;;   (setq company-box-backends-colors
;; 	'(:candidate "white" :annotation "#7C4Dff"
;;    	  :select (:background "green" :foreground "white")))
;;   (add-to-list 'company-box-frame-parameters '(font . "Hack Nerd Font-16")))

(use-package fish-mode
  :ensure t
)

;; counsel
(use-package counsel
  :ensure t
  :defer t)

;; eglot lsp
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))) ;; python-lsp-server
  (add-to-list 'eglot-server-programs '(kotlin-mode . ("kotlin-language-server")))
  (add-to-list 'eglot-server-programs '(java-mode . ("java-language-server")))
  (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(zig-mode . ("zls")))
  (add-hook 'eglot--managed-mode-hook (lambda() (flymake-mode -1)))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'java-mode-hook 'eglot-ensure)
  (add-hook 'kotlin-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'zig-mode-hook 'eglot-ensure)
  (with-eval-after-load "eglot"
    (add-to-list 'eglot-stay-out-of 'eldoc)) ;; disabled showing minibuffers
  ;; format on save disabled 03-02-2024
  ;; (add-hook 'c-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  ;; (add-hook 'c++-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  ;; (add-hook 'python-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-format))

;; zig-mode
(use-package zig-mode
  :ensure t
  :config
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

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
  (setq rust-format-on-save t))
(use-package cargo-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; python
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq python-indent 4)
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"))

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))


;; typescript
(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2))

(use-package tide
  :ensure t
  :after typescript-mode
  :hook ((typescript-mode . tide-setup)
	 (typescript-mode . tide-hl-identifier-mode)
	 (before-save . tide-format-before-save)))
  

;; kotlin
(use-package kotlin-mode
  :ensure t)

;; ocaml
(use-package tuareg
  :ensure t)

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
					; (setq ibuffer-default-sorting-mode 'major-mode)
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-formats
	'((mark modified read-only locked " "
		(name 30 30 :left :elide)
		" "
		(size 9 -1 :right)
		" "
		(mode 16 16 :left :elide)
		" " filename-and-process)
	  (mark " "
		(name 16 -1)
		" " filename)))
  :hook
  (ibuffer-mode . hl-line-mode)
  :bind
  ("C-x C-b" . ibuffer))


;; all the icons
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t)

;; dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dired-guess-shell-alist-user `(("\\.png\\'" "gimp")
                                       ("\\.jpe?g\\'" "gimp")
                                       ("\\.mp4\\'" "mpv")))
  (add-hook 'dired-load-hook (lambda () (load "dired-x"))))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; disabled 2023-09-27 18:23:24
(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

;; (use-package yasnippet-snippets
;;   :defer t
;;   :after yasnippet)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("71c615b0a662a491a5ae8266adfbd8f75dcb0b980f5fecae9dd6494121e4e5a5" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" "b40f11c174e7e475508f1e2c1cfca354d37212494c143a494f27239c7d71a294" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3" "67f6b0de6f60890db4c799b50c0670545c4234f179f03e757db5d95e99bac332" "046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012" "0527c20293f587f79fc1544a2472c8171abcc0fa767074a0d3ebac74793ab117" "2cc1b50120c0d608cc5064eb187bcc22c50390eb091fddfa920bf2639112adb6" "fc608d4c9f476ad1da7f07f7d19cc392ec0fb61f77f7236f2b6b42ae95801a62" "3c3507184cd9b63d58106de248415981dac5facdd22f7266f0b820a9c18f4f5b" "b4c8beafbdaf78e2624f0e4c06b00f40f833c7c8c1d1263f2201f719cb4b4ff9" "2858c51f2d5afa229e836bc303f5c0b7c672a9905a55e947922922b146b44d73" "4dcf06273c9f5f0e01cea95e5f71c3b6ee506f192d75ffda639d737964e2e14e" "603a831e0f2e466480cdc633ba37a0b1ae3c3e9a4e90183833bc4def3421a961" "7b303763746ab4ab92fd18d911073aadb1393d36263ba1f04f5e0641e94f6d54" "3de5c795291a145452aeb961b1151e63ef1cb9565e3cdbd10521582b5fd02e9a" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "4b287bfbd581ea819e5d7abe139db1fb5ba71ab945cec438c48722bea3ed6689" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))
 '(package-selected-packages
   '(highlight-indent-guides volatile-highlights beacon doom-modeline mini-echo moody company-anywhere company-statistics zig-mode markdown-mode geiser-guile rust-mode smartparens vscode-dark-plus-theme catppuccin-theme ace-window modus-themes unicode-fonts fish-mode evil-collection editorconfig smart-mode-line-powerline-theme tuareg magit-todos dired tree-sitter-langs tree-sitter cargo-mode rainbow-delimiters hl-todo magit-gitflow arjen-grey-theme exec-path-frome-shell counsel rainbow-mode visual-fill-column virtual-fill-column elpy smart-mode-line-atom-one-dark-theme smart-mode-line smart-jump auto-sudoedit cl-libify lsp-ivy lsp-ui lsp-mode eglot-java kotlin-mode python-mode doom-themes magit sanityinc-tomorrow-day solarized-theme material-theme color-theme-sanityinc-tomorrow key-chord organic-green-theme undo-tree everforest-theme everforest powerline-evil powerline evil vterm cl clang-format monokai-pro-theme nix-mode darkokai-theme darkokai gruvbox-theme yasnippet-snippets yasnippet tide typescript-mode all-the-icons-dired all-the-icons-ibuffer gcmh move-text zenburn-theme darcula-theme darcula zenburn exec-path-from-shell company-box python-black go-mode dracula-theme which-key try use-package))
 '(warning-suppress-log-types '((comp) (comp) (comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:background "#333c43" :foreground "#d3c6aa"))))
 '(fill-column-indicator ((t (:foreground "gray80" :weight normal))))
 '(multi-magit-repo-heading ((t (:inherit magit-section-heading :box nil))))
 '(speedbar-selected-face ((t (:foreground "#008B45" :underline t)))))
(put 'upcase-region 'disabled nil)
