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
(savehist-mode 1)
(setq inhibit-startup-message t)
(setq-default fill-column 80)
(setq use-short-answers t)
;; message log
(setq message-log-max 5000)

;; 4 lsp performance
;; added at eglot init 03-08-2024
;; (setq read-process-output-max (* 2 1024 1024))
;; (setq gc-cons-threshold 100000000)

;; defconst 4 os-type
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; ;; for mac to run shell env
;; ;; fo darwin not [memq] using [eq]
(when (eq window-system '(mac ns x))
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
           (add-to-list 'default-frame-alist (cons 'width 120))
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
(defvaralias 'c-basic-offset 'default-tab-width)

;; my lisp functions

;; insert date by %y%m%d %T
(progn
(defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d\" \"%T)")))
(global-set-key "\C-x\M-d" 'insert-current-date))

;; package-things
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq package-archive-priorities
      '(("gnu" . 3)
	("melpa" . 2)
	("nongnu" . 1)))

;; enabale package-native-compile
(setq package-install-upgrade-built-in t
      package-native-compile t)
;; disable native-comp-errors
(setq native-comp-async-report-warnings-errors 'silent)
;; delete native-comp file
(setq native-compile-prunce-cache t)

(package-initialize)

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))
;; compose use-package and vc-use-package
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; auto package update
(use-package auto-package-update
  :config
  (setq auto-package-update-interval 120)
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

;; auto-save-visited
(use-package files
  :ensure nil
  :config
  (setq auto-save-visited-interval 30)
  (auto-save-visited-mode 1))

;; indent -> space
(use-package simple
  :ensure nil
  :init
  (setq-default indent-tabs-mode nil))

;; emacsclient
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; recent file log
(use-package recentf
  :init
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

;; paren-things
(use-package paren
  :init
  (show-paren-mode 1))
(use-package elec-pair
  :config
  (electric-pair-mode 1))

;; Garbbage collection magic hack
(setq gc-cons-percentage 0.2
      gc-cons-threshold (* 128 1024 1024))
(setq garbage-collection-messages t)

;; 4 long line file
(use-package so-long
  :init
  (global-so-long-mode 1))

;; 4 performance
(setq blink-matching-paren nil)
(setq vc-handled-backends '(Git))
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq ffap-machine-p-known 'reject)
(setq idle-update-delay 1.0)
(setq redisplay-skip-fontification-on-input t)
(setq command-line-ns-option-alist nil) ;;4 mac

;; org-things
(use-package org
  :init
  (setq org-return-follows-link t
	org-mouse-1-follows-link t))

(setq org-use-sub-superscripts '{}
      org-export-with-sub-superscripts nil)

(use-package org-agenda
  :ensure nil
  :after org
  :config
  (setq org-agenda-files (file-expand-wildcards (concat org-directory "/*.org"))))

(use-package org-indent
  :ensure nil
  :hook (org-mode . org-indent-mode))

(use-package org-pomodoro
  :after org)

(use-package ox-qmd
  :defer t) ;; C-c C-e for export org to markdown

(use-package org-modern
  :after org
  :init
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  :config
  (global-org-modern-mode 1))

(use-package org-modern-indent
  :vc ( :fetcher github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))


;; icons
(use-package nerd-icons)
(use-package nerd-icons-completion
  :hook (after-init . nerd-icons-completion-mode))
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :vc ( :fetcher github :repo "LuigiPiucco/nerd-icons-corfu")
  :after corfu nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; corfu
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-count 16)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0)
  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package tabnine
  :demand t
  :hook
  (kill-emacs . tabnine-kill-process)
  :config
  (tabnine-start-process)
  (global-tabnine-mode 1))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  :hook
  (((prog-mode
     text-mode
     conf-mode
     ;;eglot-managed-mode
     ) . my-capf))
  :config
  (setq dabbrev-friend-buffer-function (lambda (other-buffer)
                                       (< (buffer-size other-buffer) (* 1024 1024))))
  (defun my-capf (&optional arg)
    (setq-local completion-at-point-functions
                (list (cape-capf-properties
                       (cape-capf-case-fold
                        (cape-capf-buster
                         (cape-capf-super
                          (if arg
                              arg
                            (car completion-at-point-functions))
                          :with
                          #'tempel-complete
                          #'tabnine-completion-at-point
                          #'cape-dabbrev
                          #'cape-file)))
                       :sort t
                       :exclusive 'no)))))

(use-package corfu-popupinfo
  :ensure nil
  :hook (corfu-mode .corfu-popupinfo-mode))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode 1))

(use-package corfu-prescient
  :config
  (setq corfu-prescient-enable-filtering nil)
  (corfu-prescient-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil)

  (with-eval-after-load 'corfu
    (defun orderless-fast-dispatch (word index total)
      (and (= index 0) (= total 1) (length< word 4)
           'orderless-literal-prefix))

    (orderless-define-completion-style orderless-fast
      (orderless-style-dispatchers '(orderless-fast-dispatch))
      (orderless-matching-styles '(orderless-flex)))

    (defun my-corfu-for-orderless ()
      (setq-local corfu-auto-delay 0
                  corfu-auto-prefix 1
                  completion-styles '(orderless-fast)))

    (add-hook 'corfu-mode-hook #'my-corfu-for-orderless)))

(use-package prescient
  :config
  (setq prescient-aggressive-file-save t)
  (prescient-persist-mode 1))

;; https://qiita.com/nobuyuki86/items/122e85b470b361ded0b4
(use-package flx
  :config
  (with-eval-after-load 'prescient
    (defvar-local my/input-query nil)
    (defun my/store-input-query (string &rest _args)
      "Store the current completion query in `my/input-query'."
      (setq my/input-query (replace-regexp-in-string " " "" string)))
    (advice-add 'completion-all-completions :before #'my/store-input-query)

    (defvar vertico--total nil)
    (defvar corfu--total nil)

    ;; cache
    (defvar my/flx-cache (make-hash-table :test 'equal :size 1000))

    (defun my/get-flx-score (str query)
      (or (gethash (cons str query) my/flx-cache)
          (let ((score (condition-case nil
                           (car (flx-score str query flx-file-cache))
                         (error nil))))
            (puthash (cons str query) score my/flx-cache)
            score)))

    (defun my/flx-tiebreaker (c1 c2)
      (let ((total (or vertico--total corfu--total 0))
            (query-length (length my/input-query)))
        (if (and (< total 3000)
                 (> query-length 2)
                 (< (length c1) 100)
                 (< (length c2) 100))
            (let ((score1 (my/get-flx-score c1 my/input-query))
                  (score2 (my/get-flx-score c2 my/input-query)))
              (cond ((and (integerp score1) (integerp score2))
                     (cond ((> score1 score2) -1)
                           ((< score1 score2) 1)
                           (t (- (length c1) (length c2)))))
                    (t 0)))
          (- (length c1) (length c2)))))

    (setq prescient-tiebreaker #'my/flx-tiebreaker)

    (defun my/clear-flx-cache ()
      (clrhash my/flx-cache))

    ;; clear cache per hour 
    (defvar my/flx-cache-timer nil)
    (setq my/flx-cache-timer
          (run-with-timer 3600 3600 #'my/clear-flx-cache))))


;; vertico repalce helm & ivy
(use-package vertico
  :init
  (setq vertico-cycle t)
  (vertico-mode 1))
;; TODO: set evil key for vertico-repeat
;; (use-package vertico-repeat
;;   :ensure nil
;;   :after vertico
;;   :hook (minibuffer-setup . vertico-repeat-save))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)))

(use-package vertico-buffer
  :ensure nil
  :config
  (setq vertico-buffer-display-action '(display-buffer-at-bottom))
  (vertico-buffer-mode 1))

(use-package vertico-prescient
  :config
  (setq vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode 1))

(use-package vertico-truncate
  :vc ( :fetcher github :repo "jdtsmith/vertico-truncate")
  :config
  (vertico-truncate-mode 1))


(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package embark
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; TODO: config consult
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 1.0 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package consult-eglot
  :after eglot
  :bind ( :map eglot-mode-map
          ("C-c s" . consult-eglot-symbols)))

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode))

;; NOTE: disabled 06-08-2024 retry 07-08-2024
(use-package highlight-indent-guides
:ensure t
:config
(setq highlight-indent-guides-method 'character)
;; (setq highlight-indent-guides-method 'bitmap)
;;(setq highlight-indent-guides-responsive "stack")
;;   ;; (set-face-background 'highlight-indent-guides-character-face "dimgray") 
   :hook
   (prog-mode . highlight-indent-guides-mode))

;; tempel 4 replace yasnippet
(use-package tempel
  :demand t
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  ;; :custom
  ;; (defun my-tempel-setup()
  ;;   (setq-local completion-at-point-functions
  ;;               (cons #'tempel-expand ;; exact match
  ;;                     completion-at-point-functions)))
  ;; :hook
  ;; (add-hook 'prog-mode-hook 'my-tempel-setup)
  ;; (add-hook 'text-mode-hook 'my-tempel-setup)
  :config
  (with-eval-after-load "tempel"
    (define-key tempel-map (kbd "<tab>") #'tempel-next)
    (define-key tempel-map (kbd "C-i") #'tempel-next)
    (define-key tempel-map (kbd "C-S-i") #'tempel-previous)
    (define-key tempel-map (kbd "C-<tab>") #'tempel-previous)
    ))
(use-package tempel-collection
  :after tempel)

(use-package eglot-tempel
  :after (eglot tempel)
  :hook (eglot-managed-mode . eglot-tempel-mode))

(use-package jsonrpc
  :defer t
  :config
  (setq jsonrpc-default-request-timeout 3000)
  (fset #'jsonrpc--log-event #'ignore))


(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package undo-fu
  :config
  (with-eval-after-load 'evil
    (setq evil-undo-system 'undo-fu)))
(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode 1))

(use-package undo-tree
  :ensure t
  :after evil
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history nil)
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))
(use-package vundo
  :ensure t
  )

(use-package dashboard
  :config
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package imenu-list
  :bind ( :map my-toggle-map
          ("i" . imenu-list-smart-toggle))
  :init
  (setq imenu-list-position 'left))

;; font setting
(use-package fontaine
  :config
(setq fontaine-presets
               '((regular
                  :default-family "ComicShannsMono Nerd Font"
                  :fixed-pitch-family "ComicShannsMono Nerd Font"
                  :variable-pitch-family "Sarasa Mono J Nerd Font"
                  :italic-family "Sarasa Mono J Nerd Font"
                  :default-height 240)
                 (large
                  :default-family "ComicShannsMono Nerd Font"
                  :variable-pitch-family "Sarasa Mono J Nerd Font"
                  :default-height 300)))
 (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

;; everforest theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/everforest-theme")
;; (load-theme 'everforest-hard-dark t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/tokyo-night-theme")

(use-package ef-themes
  :config
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (load-theme 'ef-melissa-light t)
  ;; (load-theme 'ef-melissa-dark t)
  ;; (load-theme 'ef-spring t)
  ;; (load-theme 'ef-dream t)
  )

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-disable-other-themes t)

  (setq modus-themes-completions
        '((t . (underline))))

  (setq modus-themes-common-palette-overrides
        '((fg-completion-match-0 blue)
          (fg-completion-match-1 magenta-warmer)
          (fg-completion-match-2 cyan)
          (fg-completion-match-3 red)
          (bg-completion-match-0 bg-blue-nuanced)
          (bg-completion-match-1 bg-magenta-nuanced)
          (bg-completion-match-2 bg-cyan-nuanced)
          (bg-completion-match-3 bg-red-nuanced)))

  ;; (load-theme 'modus-operandi-tinted t)
  )

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'latte) ;; light 
  ;;(setq catppuccin-flavor 'frappe) ;; dark
  ;;(setq catppuccin-flavor 'macchiato) ;; dark
  ;;(setq catppuccin-flavor 'mocha) ;; dark
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

(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :init
  (global-diff-hl-mode 1)
  (global-diff-hl-show-hunk-mouse-mode 1))

(use-package difftastic
  :demand t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

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
;; NOTE: evil vs meow 2023-11-20 
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

;; high light line mode
(use-package lin
  :init
  (setq lin-face 'lin-red)
  (lin-global-mode 1))

(use-package puni
  :config
  (puni-global-mode 1))

;; auto setup tree-sitter
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))


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
(use-package flymake
  :hook ((prog-mode
          conf-mode) . flymake-mode)
  :init
  (setq-default flymake-no-changes-timeout 1.0))
(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))

(use-package fish-mode
  :ensure t
)

;; eglot lsp
(use-package eglot
  :ensure t
  :init
  (setq eglot-events-buffer-config '(:size 0 :format short)
	eglot-ignored-server-capabilities '(:documentHighlightProvider)
	eglot-send-changes-idle-time 1.0)
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) .
                  ("rust-analyzer" :initializationOptions (:check
                                     (:command "clippy")))))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))) ;; python-lsp-server
  (add-to-list 'eglot-server-programs '(kotlin-mode . ("kotlin-language-server")))
  (add-to-list 'eglot-server-programs '(java-mode . ("java-language-server")))
  (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(zig-mode . ("zls")))
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
  (add-to-list 'eglot-stay-out-of 'flymake)
  ;;(add-to-list 'eglot-stay-out-of 'eldoc)
  ) ;; disabled showing minibuffers
  ;; format on save disabled 03-02-2024
  ;; (add-hook 'c-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  ;; (add-hook 'c++-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  ;; (add-hook 'python-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-format))

(use-package eglot-x
  :vc ( :fetcher github :repo "nemethf/eglot-x")
  :after eglot
  :config
  (eglot-x-setup))

(use-package lsp-snippet
  :vc ( :fetcher github :repo "svaante/lsp-snippet")
  :config
  (when (featurep 'eglot)
    (lsp-snippet-tempel-eglot-init)))

;; eglot boost (works?)
;; not working on arm mac T_T (fixed 22-08-24)
;; https://github.com/blahgeek/emacs-lsp-booster
;; build by binary
;; ~/usr/local/bin
(use-package eglot-booster
  :after eglot
  :vc ( :fetcher github :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode 1))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode))
(use-package eglot-signature-eldoc-talkative
  :after eldoc-box
  :config
  (advice-add #'eglot-signature-eldoc-function
              :override #'eglot-signature-eldoc-talkative))

;; elisp things
(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))


;; zig-mode
(use-package zig-mode
  :ensure t
  :config
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

;; go-mode
(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  ;; (setq gofmt-command "goimports")
  ;; (add-hook 'go-mode-hook (lambda()
  ;;       		    (setq-default)
  ;;       		    (setq tab-width 4)
  ;;       		    (setq standard-indent 4)
  ;;       		    (setq ident-tabs-mode nil)))
  ;; (add-hook 'before-save-hook 'gofmt-before-save)
  )

;; TODO: eglot not working well in rust-mode
;; ;; rust-mode
;; (use-package rust-mode
;;   :ensure t
;;   :init
;;   (setq rust-mode-treesitter-derive t)
;;   ;; :hook (rust-mode . (lambda ()
;;   ;;                      (setq-local tab-width 4)))
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;;   ;; (setq rust-format-on-save t)
;;   )
;; (use-package cargo-mode
;;   :ensure t
;;   :hook
;;   (rust-mode . cargo-minor-mode)
;;   :config
;;   (setq compilation-scroll-output t))
;; (use-package cargo
;;   :ensure t)
;; rustic-mode
;; 2024-08-22
(use-package rustic
  :ensure t
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))

  :hook (rust-mode . (lambda ()
                       (setq-local tab-width 4)))
  :config
  (setq rustic-lsp-client 'eglot)
  )


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

;; sql things
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))
(use-package sqlformat
  :init
  (setq sqlformat-command "sqlfluff"))

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
  ;; (setq dired-guess-shell-alist-user `(("\\.png\\'" "gimp")
  ;;                                      ("\\.jpe?g\\'" "gimp")
  ;;                                      ("\\.mp4\\'" "mpv")))
  (add-hook 'dired-load-hook (lambda () (load "dired-x"))))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package rg
  :defer t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(eglot-booster lsp-snippet eglot-x))
 '(package-vc-selected-packages
   '((vertico-truncate :vc-backend Git :url "https://github.com/jdtsmith/vertico-truncate")
     (org-modern-indent :vc-backend Git :url "https://github.com/jdtsmith/org-modern-indent")
     (vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
