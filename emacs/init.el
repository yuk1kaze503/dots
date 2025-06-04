;;; Personal configuration file.
;;; no-byte-compile
;;; https://github.com/alphapapa/ap.el
;;; https://github.com/abo-abo/oremacs
;;; https://github.com/jwiegley/dot-emacs?tab=readme-ov-file
;;; https://www.patrickdelliott.com/emacs.d/#org4234a81
;;; 14-05-25 ver @@

;; Package management 
;; trying Elpaca
;; https://github.com/progfolio/elpaca
;; FIXME elpaca version error. (need manually fix)
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; enable use-package support elpaca, and :ensure packages by default
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(setq use-package-always-ensure t)

;;
;; Variables
;;
(defvar sn0w/library-path "~/Documents/frx/library/"
  "Directory .pdf collection lives.")

(defvar sn0w/notes-path "~/Documents/frx/org/notes/"
  "Notes.")

(defvar sn0w/journal-path "~/Documents/frx/org/daily/"
  "Journal entries.")

(defvar sn0w/org-path "~/Documents/frx/org"
  "Org path.")

(defvar sn0w/global-bib-file "~/Documents/frx/bibtex/bib/master.bib"
  "Bibliography.")
;;
;; Emacs 
;;

(use-package emacs
  :demand t
  :ensure nil
  :init
  (setq tab-always-indent 'complete) ;; enable tab completion

  (setq kill-whole-line t)

  (setq enable-recursive-minibuffers t)

  (setq backup-by-copying t)

  (setq sentence-end-double-space nil)

  (setq frame-inhibit-implied-resize t) ;; useless for a tiling window manager

  (setq show-trailing-whitespace t) ;; self-explanatory

  (setq user-full-name "sn0w") ;; my details
  ;;(setq user-mail-address "-")

  (defalias 'yes-or-no-p 'y-or-n-p) ;; life is too short

  (setq indent-tabs-mode nil) ;; no tabs

  ;; keep backup and save files in a dedicated directory
  (setq backup-directory-alist
          `((".*" . ,(concat user-emacs-directory "backups")))
          auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "backups") t)))

  (setq create-lockfiles nil) ;; no need to create lockfiles

  (set-charset-priority 'unicode) ;; utf8 everywhere
  (setq locale-coding-system 'utf-8
          coding-system-for-read 'utf-8
          coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything


  ;; Don't persist a custom file
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe
  (setq enable-local-variables :all)     ; fix =defvar= warnings

  (setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  (show-paren-mode t)

  ;; Hide commands in M-x which don't work in the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (when (eq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(when (eq window-system '(mac ns))
	    (setq mac-command-modifier 'meta))

(use-package browse-url
  :demand t
  :ensure nil
  :init
  (setq browse-url-firefox-program "firefox"))

(use-package electric
  :demand t
  :ensure nil
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens
  (setq electric-pair-preserve-balance nil)) ;; more annoying than useful

(use-package ediff
  :demand t
  :ensure nil
  )

;;
;; Keybindings
;;
(global-set-key (kbd "M-:") 'comment-dwim)
(global-set-key (kbd "<f4>") 'compile)

(use-package evil
  :demand t
  :init
  (setq evil-respect-visual-line-mode t) ;; respect visual lines

  (setq evil-search-module 'isearch) ;; use emacs' built-in search functionality.

  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'

  (setq evil-want-integration t) ;; necessary for evil collection
  (setq evil-want-keybinding nil)

  (setq evil-split-window-below t) ;; split windows created below
  (setq evil-vsplit-window-right t) ;; vertically split windows created to the right

  (setq evil-want-C-i-jump nil) ;; hopefully this will fix weird tab behaviour

  (setq evil-undo-system 'undo-redo) ;; undo via 'u', and redo the undone change via 'C-r'; only available in emacs 28+.
  :config
  (evil-mode t) ;; globally enable evil mode
  ;; (define-key evil-insert-state-map (kbd "C-[") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
  ;; set the initial state for some kinds of buffers.
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; buffers in which I want to immediately start typing should be in 'insert' state by default.
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert))

(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  )

(use-package evil-collection ;; evilifies a bunch of things
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
  :config
  (evil-collection-init))

(use-package evil-commentary ;; gc for evil-commentary. gy for yank. s-/ commentary-line
  :after evil
  :config
  (evil-commentary-mode)) ;; globally enable evil-commentary

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)) ;; globally enable evil-surround

(use-package evil-goggles
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

;; TODO: General.el things
;; https://github.com/noctuid/general.el
(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer sn0w/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; set up ',' as the local leader key
  (general-create-definer sn0w/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "," ;; set local leader
    :global-prefix "M-,") ;; access local leader in insert mode

  (general-define-key
   :states 'insert
   "C-g" 'evil-normal-state) ;; don't stretch for ESC

  ;; unbind some annoying default bindings
  (general-unbind
    "C-x C-r"   ;; unbind find file read only
    "C-x C-z"   ;; unbind suspend frame
    "C-x C-d"   ;; unbind list directory
    "<mouse-2>") ;; pasting with mouse wheel click


  (sn0w/leader-keys
    "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (sn0w/leader-keys
  "w" '(:keymap evil-window-map :wk "window")) ;; window bindings

  (sn0w/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; help
  ;; namespace mostly used by 'helpful'
  (sn0w/leader-keys
    "h" '(:ignore t :wk "help"))

  ;; file
  (sn0w/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file") ;; gets overridden by consult
    "fs" '(save-buffer :wk "save file"))

  ;; buffer
  ;; see 'bufler' and 'popper'
  (sn0w/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
    "bk" '(kill-this-buffer :wk "kill this buffer")
    "br" '(revert-buffer :wk "reload buffer"))

  ;; bookmark
  (sn0w/leader-keys
    "B" '(:ignore t :wk "bookmark")
    "Bs" '(bookmark-set :wk "set bookmark")
    "Bj" '(bookmark-jump :wk "jump to bookmark"))

  ;; universal argument
  (sn0w/leader-keys
    "u" '(universal-argument :wk "universal prefix"))

  ;; notes
  ;; see 'citar' and 'org-roam'
  (sn0w/leader-keys
    "n" '(:ignore t :wk "notes")
    ;; see org-roam and citar sections
    "na" '(org-todo-list :wk "agenda todos")) ;; agenda

  ;; code
  ;; see 'flymake'
  (sn0w/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; open
  (sn0w/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar")
    "op" '(elpaca-log t :wk "elpaca"))


  ;; search
  ;; see 'consult'
  (sn0w/leader-keys
    "s" '(:ignore t :wk "search"))

  ;; templating
  ;; see 'tempel'
  (sn0w/leader-keys
    "t" '(:ignore t :wk "template"))
  ;; yasnippet
  ;; see 'yasnippet'
  (sn0w/leader-keys
    "y" '(:ignore t :wk "yasnippet"))
  )

;; Org
(use-package org
  :ensure (:wait t)
  :demand t
  :init
  ;; edit settings (recommended by org-modern)
  (setq org-auto-align-tags nil
            org-tags-column 0
            org-catch-invisible-edits 'show-and-error
            org-special-ctrl-a/e t ;; special navigation behaviour in headlines
            org-insert-heading-respect-content t)

  ;; styling, hide markup, etc. (recommended by org-modern)
  (setq org-hide-emphasis-markers t
            org-src-fontify-natively t ;; fontify source blocks natively
            org-highlight-latex-and-related '(native) ;; fontify latex blocks natively
            org-pretty-entities t)

  ;; agenda styling (recommended by org-modern)
  (setq org-agenda-tags-column 0
            org-agenda-block-separator ?─
            org-agenda-time-grid
            '((daily today require-timed)
              (800 1000 1200 1400 1600 1800 2000)
              " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
            org-agenda-current-time-string
            "⭠ now ─────────────────────────────────────────────────")

  (setq org-ellipsis "...")

  ;; todo setup
  (setq org-todo-keywords
            ;; it's extremely useful to distinguish between short-term goals and long-term projects
            '((sequence "TODO(t)" "SOMEDAY(s)" "|" "DONE(d)")
              (sequence "TO-READ(r)" "READING(R)" "|" "HAVE-READ(d)")
              (sequence "PROJ(p)" "|" "COMPLETED(c)")))


  (setq org-adapt-indentation nil) ;; interacts poorly with 'evil-open-below'

  :custom
  (org-cite-global-bibliography (list sn0w/global-bib-file))
  ;; handle citations using citar
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :general
  (sn0w/local-leader-keys
        :keymaps 'org-mode-map
        "a" '(org-archive-subtree :wk "archive")
        "c" '(org-cite-insert :wk "insert citation")
        "l" '(:ignore t :wk "link")
        "ll" '(org-insert-link t :wk "link")
        "lp" '(org-latex-preview t :wk "prev latex")
        "h" '(consult-org-heading :wk "consult heading")
        "d" '(org-cut-special :wk "org cut special")
        "y" '(org-copy-special :wk "org copy special")
        "p" '(org-paste-special :wk "org paste special")
        "b" '(:keymap org-babel-map :wk "babel")
        "t" '(org-todo :wk "todo")
        "s" '(org-insert-structure-template :wk "template")
        "e" '(org-edit-special :wk "edit")
        "i" '(:ignore t :wk "insert")
        "ih" '(org-insert-heading :wk "insert heading")
        "is" '(org-insert-subheading :wk "insert heading")
        "f" '(org-footnote-action :wk "footnote action")
        ">" '(org-demote-subtree :wk "demote subtree")
        "<" '(org-promote-subtree :wk "demote subtree"))
  (:keymaps 'org-agenda-mode-map
                "j" '(org-agenda-next-line)
                "h" '(org-agenda-previous-line))

  :hook
  (org-mode . olivetti-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . (lambda () (electric-indent-local-mode -1))) ;; disable electric indentation

  :config
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  (add-to-list 'org-latex-packages-alist '("" "braket" t))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
         (emacs-lisp . t)
         (awk . t)))
  ;; set up org paths
  (setq org-directory "~/Documents/frx/org/agenda")
  (setq org-default-notes-file (concat org-directory "/notes.org")))

;; Avy can do anything
;; https://karthinks.com/software/avy-can-do-anything/
(use-package avy
  :demand t
  ;; :bind
  ;; (("C-:" . avy-goto-char)
  ;; ("C-'" . avy-goto-char-2)
  ;; ("M-g f" . avy-goto-line)))
  :init
  (defun sn0w/avy-action-insert-newline (pt)
      (save-excursion
        (goto-char pt)
        (newline))
      (select-window
       (cdr
        (ring-ref avy-ring 0))))
    (defun sn0w/avy-action-kill-whole-line (pt)
      (save-excursion
        (goto-char pt)
        (kill-whole-line))
      (select-window
       (cdr
        (ring-ref avy-ring 0))))
    (defun sn0w/avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t) ;; adds an avy action for embark
    :general
    (general-def '(normal motion)
      "s" 'evil-avy-goto-char-timer
      "f" 'evil-avy-goto-char-in-line
      "gl" 'evil-avy-goto-line ;; this rules
      ";" 'avy-resume)
    :config
    (setf (alist-get ?. avy-dispatch-alist) 'sn0w/avy-action-embark ;; embark integration
          (alist-get ?i avy-dispatch-alist) 'sn0w/avy-action-insert-newline
          (alist-get ?K avy-dispatch-alist) 'sn0w/avy-action-kill-whole-line))

(use-package link-hint
  :demand t
  :config
  (setq browse-url-browser-function 'browse-url-firefox)
  (setq link-hint-avy-style 'pre))

(use-package which-key
  :after evil
  :demand t
  :init (which-key-mode)
  :config
  (which-key-setup-minibuffer))

;;
;; Appearance
;;

;; frame-size
(defun set-frame-size-according-to-resolution ()
 (interactive)
 (if window-system
 (progn
   ;; use 150 char wide window for largeish displays
   ;; and smaller 80 column windows for smaller displays
   ;; pick whatever numbers make sense for you
   (if (> (x-display-pixel-width) 1280)
          (add-to-list 'default-frame-alist (cons 'width 150))
          (add-to-list 'default-frame-alist (cons 'width 80)))
   (add-to-list 'default-frame-alist
        (cons 'height (/ (- (x-display-pixel-height) 200)
                            (frame-char-height)))))))
(set-frame-size-according-to-resolution)

(progn
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'relative))
(setq column-number-mode t)

(use-package dashboard
  :config
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook))

(use-package mood-line
  :disabled
  :demand t
  :config (mood-line-mode))

(use-package minions
  :demand t
  :config
  (minions-mode 1))

(use-package all-the-icons
  :demand t)


;; prettify dired with icons
(use-package all-the-icons-dired
  :demand t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :demand t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

;; A simple Emacs minor mode for a nice writing environment.
(use-package olivetti
  :demand t
  :init
  (setq olivetti-body-width 80)
  (setq olivetti-style 'fancy)
  (setq olivetti-minimum-body-width 50))

;; Font
(use-package fontaine
  :config
(setq fontaine-presets
               '((regular
                  :default-family "ComicShannsMono Nerd Font"
                  :fixed-pitch-family "ComicShannsMono Nerd Font"
                  :variable-pitch-family "HackNerdFontMono"
                  :italic-family "HackNerdFontMono"
                  :default-height 240)
                 (large
                  :default-family "ComicShannsMono Nerd Font"
                  :variable-pitch-family "HackNerdFontMono"
                  :default-height 300)))
 (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

;; Themes
(use-package solaire-mode
  :demand t
  :config
  (solaire-global-mode +1))

(use-package catppuccin-theme
  :demand t
  :config
  (setq catppuccin-height-title1 1.5)
  ;; (load-theme 'catppuccin t)
  )
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/everforest-theme")
;; (load-theme 'everforest-hard-dark t)

;; (load-theme 'modus-operandi t)

(use-package dracula-theme
  :config
  ;; (load-theme 'dracula t)
  )

(use-package ef-themes
  :config
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  ;; (load-theme 'ef-melissa-light t)
  )

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package indent-bars
  :hook
  ((python-mode yaml-mode) . indent-bars-mode)
  :custom
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.2)
   indent-bars-pattern "."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth nil
   indent-bars-highlight-current-depth nil
   indent-bars-display-on-blank-lines nil)) 

(use-package hl-todo
  :demand t
  :init
  (global-hl-todo-mode))

;;
;; Organization
;;

;; Popper
;; https://github.com/karthink/popper
(use-package popper
  :demand t
  :general
  (sn0w/leader-keys
        "bp" '(:ignore t :wk "popper")
        "bpc" '(popper-cycle t :wk "cycle")
        "bpt" '(popper-toggle-latest t :wk "toggle latest")
        "bpb" '(popper-toggle-type t :wk "toggle type")
        "bpk" '(popper-kill-latest-popup t :wk "kill latest"))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*helpful"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          magit-process-mode
          "^\\*eshell.*\\*" eshell-mode
          "\\*direnv\\*"
          "\\*elfeed-log\\*"
          "\\*Async-native-compile-log\\*"
          "\\*TeX Help\\*"
          "\\*Embark Collect Live\\*"))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Bufler
;; https://github.com/alphapapa/bufler.el
(use-package bufler
  :demand t
  :custom
  (bufler-workspace-mode t)
  ;; (bufler-workspace-tabs-mode t)
  :general
  (sn0w/leader-keys
    "bB" '(bufler :wk "bufler") ;; overrides consult
    "bf" '(bufler-workspace-frame-set :wk "bufler workspace frame set")
    "bl" '(bufler-list :wk "bufler list"))
  (:keymaps 'bufler-list-mode-map
            :states 'normal
            "?" 'hydra:bufler/body
            "RET" 'bufler-list-buffer-switch
            "SPC" 'bufler-list-buffer-peek
            "d" 'bufler-list-buffer-kill))

;; Projects
(use-package project
  :ensure nil
  :general
  (sn0w/leader-keys
    "p" '(:keymap project-prefix-map :wk "project")))

;; Dired
(use-package dired
  :ensure nil
  :general
   (sn0w/leader-keys
      "fd" '(dired :wk "dired") ;; open dired (in a directory)
      "fj" '(dired-jump :wk "dired jump")) ;; open direct in the current directory
    ;; ranger like navigation
    (:keymaps 'dired-mode-map
              :states 'normal
              "h" 'dired-up-directory
              "q" 'kill-current-buffer
              "l" 'dired-find-file)
  :hook
  (dired-mode . dired-hide-details-mode))

  ;; toggle subtree visibility with 'TAB'
  ;; makes dired a much more pleasant file manager
(use-package dired-subtree
  :demand t)

;;
;; Languages
;;

;; Go
(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook (lambda()
        		    (setq-default)
        		    (setq tab-width 4)
        		    (setq standard-indent 4)))
  )

;; Lua
(use-package lua-mode
  :mode "\\.lua\\'")

;; Lisp
;; (use-package sly) ;;FIXME: compatibility with corfu

;; JS
(use-package js2-mode)

;; Ocaml
(use-package tuareg
  :ensure t)

;; Python
;; (setq python-shell-interpreter "/opt/homebrew/bin/ipython")
(use-package python-mode
  :config
  (setq python-indent 4)
  )

(use-package blacken
    :hook (python-mode . blacken-mode)
    :config
    (setq blacken-line-length '88))

(use-package py-vterm-interaction
  :general
  (sn0w/local-leader-keys
    :keymaps 'py-vterm-interaction-mode-map
    "C-b" '(py-vterm-interaction-send-buffer :wk "send the whole content")
    "C-c" '(py-vterm-interaction-send-region-or-current-line :wk "run current line")
    "C-z" '(py-vterm-interaction-switch-to-repl-buffer :wk "switch to repl")
    "C-j" '(py-vterm-interaction-send-current-cell :wk "send the current cell")
    "C-f" '(py-vterm-interaction-run-current-function :wk "send the current function")
    "C-r" '(py-vterm-interaction-send-run-buffer-file :wk "ipython %run magic")
    "C-a" '(py-vterm-interaction-send-rerun-last :wk "rerun the last")
    "C-t" '(py-vterm-interaction-repl-copy-mode :wk "copy mode")
    "M-k" '(py-vterm-interaction-repl-clear-buffer :wk "Clear"))
            
  :hook (python-mode . py-vterm-interaction-mode)
  :config
  (setq py-vterm-interaction-repl-program "ipython -i")
  (setq py-vterm-interaction-silent-cells t)
  )
(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright") 
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))


;; Sql
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))
(use-package sqlformat
  :init
  (setq sqlformat-command "sqlfluff"))

;; Typescript
(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

;; Rust
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config (setq rustic-lsp-client 'eglot))

;; Nix
(use-package nix-mode
  :mode "\\.nix\\'")

;; Zig
(use-package zig-mode
  :ensure t
  :config
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

;; Markdown
(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . olivetti-mode))
         (markdown-mode . variable-pitch-mode)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  (setq markdown-header-scaling t))

(use-package pandoc-mode
  :after markdown-mode
  :hook (markdown-mode . pandoc-mode))

;; Latex
(use-package tex 
  :ensure (auctex :pre-build (("./autogen.sh")
                              ("./configure"
                               "--without-texmf-dir"
                               "--with-packagelispdir=./"
                               "--with-packagedatadir=./")
                              ("make"))
                  :build (:not elpaca--compile-info) ;; Make will take care of this step
                  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                  :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :init
  (setq TeX-parse-self t ; parse on load
        reftex-plug-into-AUCTeX t
        TeX-auto-save t  ; parse on save
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t
        TeX-electric-sub-and-superscript t
        TeX-engine 'luatex ;; use lualatex by default
        TeX-save-query nil
        TeX-electric-math (cons "\\(" "\\)")) ;; '$' inserts an in-line equation '\(...\)'

  (add-hook 'TeX-mode-hook #'reftex-mode)
  (add-hook 'TeX-mode-hook #'olivetti-mode)
  (add-hook 'TeX-mode-hook #'turn-on-auto-fill)
  (add-hook 'TeX-mode-hook #'prettify-symbols-mode)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'TeX-mode-hook #'outline-minor-mode))

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode)
  :general
  (:keymaps 'evil-tex-mode-map
            "M-]" 'evil-tex-brace-movement)
  :config
  (unbind-key "M-n" 'evil-tex-mode-map))

;; https://github.com/emacs-citar/citar/wiki/Indicators
(use-package citar
  :demand t
  :after all-the-icons
  :init
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "file-o"
              :face 'all-the-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (all-the-icons-octicon
              "link"
              :face 'all-the-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))

  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (all-the-icons-material
              "speaker_notes"
              :face 'all-the-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))

  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "circle-o"
              :face 'all-the-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  :hook
  ;; set up citation completion for latex, org-mode, and markdown
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  (markdown-mode . citar-capf-setup)
  :config
  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-links-icons
              citar-indicator-notes-icons
              citar-indicator-cited-icons)))

(use-package citar-embark
  :after citar embark
  :config (citar-embark-mode))

;; Uses emacs font lock to hl sc blocks in latex
(use-package engrave-faces)

;;
;; Org
;;
(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-auctex
  :ensure (:type git :host github :repo
                   "karthink/org-auctex")
  :hook (org-mode . org-auctex-mode))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package org-cliplink
  :after org)

(use-package org-modern
  :after org
  :config (global-org-modern-mode))

(use-package ox-qmd
  :defer t) ;; C-c C-e 4 export org to markdown

(use-package htmlize)

;; TODO: Org-Transclusion, Org-noter, Org-roam
;; https://nobiot.github.io/org-transclusion/
;; (use-package org-roam
;;   :demand t
;;   :general
;;   (sn0w/leader-keys
;;     "nr" '(:ignore t :wk "roam")
;;     "nri" '(org-roam-node-insert t :wk "insert node")
;;     "nrt" '(org-roam-buffer-toggle t :wk "roam buffer toggle")
;;     "nrc" '(org-roam-capture t :wk "roam capture")
;;     "nrf" '(org-roam-node-find :wk "find node")
;;     "nrd" '(:ignore t :wk "dailies")
;;     "nrdt" '(org-roam-dailies-goto-today :wk "today")
;;     "nrdt" '(org-roam-dailies-goto-yesterday :wk "today")
;;     "nrdT" '(org-roam-dailies-goto-tomorrow :wk "today")
;;     "nrdd" '(org-roam-dailies-goto-date :wk "goto date"))
;;   :config
;;   ;; org-roam-buffer
;;   (add-to-list 'display-buffer-alist
;;                '("\\*org-roam\\*"
;;                  (display-buffer-in-direction)
;;                  (direction . right)
;;                  (window-width . 0.33)
;;                  (window-height . fit-window-to-buffer)))
;;   ;; get tags to show up in 'org-roam-node-find':
;;   (setq org-roam-node-display-template
;;         (concat "${title:*} "
;;                 (propertize "${tags:10}" 'face 'org-tag)))
;;   (setq org-roam-completion-everywhere t) ;; roam completion anywhere
;;   (setq org-roam-directory patrl/notes-path)
;;   (setq org-roam-db-location (concat org-roam-directory "/.database/org-roam.db"))
;;   (unless (< emacs-major-version 29)
;;     (setq org-roam-database-connector 'sqlite-builtin))
;;   (org-roam-db-autosync-mode) ;; ensures that org-roam is available on startup


;;   ;; dailies config
;;   (setq org-roam-dailies-directory "daily/")
;;   (setq org-roam-dailies-capture-templates
;;         '(("d" "default" entry
;;            "* %?"
;;            :target (file+head "%<%Y-%m-%d>.org"
;;                               "#+title: %<%Y-%m-%d>\n#+filetags: daily\n")))))

(use-package org-noter
  :commands
  org-noter
  :general
  (sn0w/local-leader-keys
    :keymaps 'org-noter-doc-mode-map
    "i" 'org-noter-insert-note)
  :config
  (setq org-noter-notes-search-path (list sn0w/notes-path))
  (setq org-noter-default-notes-file-names '("literature-notes.org"))
  (setq org-noter-hide-other nil)
  (setq org-noter-always-create-frame nil))

;;
;; Completion
;; https://github.com/minad
(use-package vertico
  :demand t
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-multiform-categories
        '((file grid)
          (jinx grid (vertico-grid-annotate . 20))
          (citar buffer)))
  (setq vertico-cycle t) ;; enable cycling for 'vertico-next' and 'vertico-prev'
  :general
  (:keymaps 'vertico-map
            ;; keybindings to cycle through vertico results.
	    "C-j" 'vertico-next
            "C-k" 'vertico-previous
            "C-f" 'vertico-exit
            "<backspace>" 'vertico-directory-delete-char
            "C-<backspace>" 'vertico-directory-delete-word
            "C-w" 'vertico-directory-delete-word
            "RET" 'vertico-directory-enter)
  (:keymaps 'minibuffer-local-map
            "M-h" 'backward-kill-word))

(use-package orderless
  :demand t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package marginalia
  :demand t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :demand t
  :general
    (sn0w/leader-keys
    "bb" '(consult-buffer :wk "consult buffer")
    "Bb" '(consult-bookmark :wk "consult bookmark")
    "ht" '(consult-theme :wk "consult theme")
    "sr" '(consult-ripgrep :wk "consult rg")
    "sg" '(consult-grep :wk "consult grep")
    "sG" '(consult-git-grep :wk "consult git grep")
    "sf" '(consult-find :wk "consult find")
    "sF" '(consult-locate :wk "consult locate")
    "sl" '(consult-line :wk "consult line")
    "sy" '(consult-yank-from-kill-ring :wk "consult yank from kill ring")
    "i" '(consult-imenu :wk "consult imenu"))
  :config  ;;use project.el to retrieve the project root
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package affe
  :demand t
  :after orderless
  :general
  (sn0w/leader-keys
    "sa" '(affe-grep :wk "affe grep")
    "sw" '(affe-find :wk "affe find"))
  :init
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  :config
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

(use-package embark
  :demand t
  :general
  (sn0w/leader-keys
     "." 'embark-act) ;; easily accessible 'embark-act' binding.
  ("C-." 'embark-act) ;; overlaps with evil-repeat 
  ("C-;" 'embark-dwim) ;; overlaps with IEdit
  (:keymaps 'vertico-map
            "C-." 'embark-act) ;; embark on completion candidates
  (:keymaps 'embark-heading-map
            "l" 'org-id-store-link)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :demand t
  :hook
  (eval-expression-minibuffer-setup . corfu-mode)
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t) ;; allows cycling through candidates
  (corfu-auto nil) ;; disables auto-completion
  (corfu-on-exact-match nil)
  (corfu-count 8)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0)
  :bind
  :general
  (:keymaps 'corfu-map
            "SPC" 'corfu-insert-separator)) ;; for compatibility with orderless

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook
  (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package cape
  :demand t
  ;; bindings for dedicated completion commands
  :general
  ("M-p p" 'completion-at-point ;; capf
   "M-p t" 'complete-tag ;; etags
   "M-p d" 'cape-dabbrev ;; dabbrev
   "M-p h" 'cape-history
   "M-p f" 'cape-file
   "M-p k" 'cape-keyword
   "M-p s" 'cape-symbol
   "M-p a" 'cape-abbrev
   "M-p i" 'cape-ispell
   "M-p l" 'cape-line
   "M-p w" 'cape-dict
   "M-p \\" 'cape-tex
   "M-p &" 'cape-sgml
   "M-p r" 'cape-rfc1345)
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

;; Convert the company-reftex-labels backend to a capf using cape and activate in in LaTeX-mode
(use-package company-reftex
  :after cape
  :init
  (defun reftex-setup-capf ()
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-reftex-labels)))
  :hook
  (LaTeX-mode . reftex-setup-capf))

;;
;; Checkers
;;

(use-package flymake
  :ensure nil
  :general
  (sn0w/leader-keys
    :keymaps 'flymake-mode-map
    "cf" '(consult-flymake :wk "consult flymake") ;; depends on consult
    "cc" '(flymake-mode :wk "toggle flymake")) ;; depends on consult 
  :hook
  (TeX-mode . flymake-mode) 
  (emacs-lisp-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout nil)
  :general
  (general-nmap "] !" 'flymake-goto-next-error)
  (general-nmap "[ !" 'flymake-goto-prev-error))

(use-package flymake-collection)
(add-hook 'python-base-mode-hook 'flymake-mode)
(setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))

;;
;; Tools
;;

;; Recursion indicator
(use-package recursion-indicator
  :demand t
  :config
  (recursion-indicator-mode))

;; RX
(use-package re-builder
  :ensure nil
  :config (setq reb-re-syntax 'rx))

;; Spell Checker
(use-package jinx
  :demand t
  :hook (emacs-startup . global-jinx-mode)
  :general
  ("M-$" 'jinx-correct
   "C-M-$" 'jinx-languages))

;; Better helpful
(use-package helpful
  :demand t
  :bind
  (("C-h f"  . helpful-callable)
  ("C-h v"   . helpful-variable)
  ("C-h k"   . helpful-key)
  ("C-h x"   . helpful-command)
  ("C-c C-d" . helpful-at-point)
  ("C-h F"   . helpful-function))
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))


;; Better search
(use-package deadgrep
  :demand t
  :bind ("<f5>" . deadgrep))

;; Yasnippet
(use-package yasnippet
  :general
  (sn0w/leader-keys
    "ys" '(yas-insert-snippet :wk "yas-insert-snippet"))
  :config
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elpaca/builds/yasnippet-snippets/snippets")
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; Tempel
(use-package tempel
  :demand t
  :general
  ("M-p +" 'tempel-complete) ;; M-p completion prefix; see `cape'
  (sn0w/leader-keys
  "ti" '(tempel-insert :wk "tempel insert"))
  (:keymaps 'tempel-map
            "TAB" 'tempel-next) ;; progress through fields via `TAB'
  :init
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package tempel-collection :ensure t)

;; Magit
(use-package transient)
(use-package magit
  :after transient
  :general
  (sn0w/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "status"))
  :config (global-set-key (kbd "C-x g") 'magit-status))

(use-package magit-gitflow
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(use-package magit-todos
  :defer t)

;; Eshell
(use-package eshell
  :ensure nil
  :general
  (sn0w/leader-keys
    "oe" '(eshell :wk "eshell")))
;; Vterm
(use-package vterm
  :general
  (sn0w/leader-keys
    "vt" '(vterm :wk "vterm")))

;; LSP
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; use corfu!
  :init
  (defun sn0w/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . sn0w/lsp-mode-setup-completion) ;; setup orderless completion style.
  :commands
  lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
	lsp-ui-doc-use-webkit t
	lsp-ui-doc-include-signature t
        lsp-ui-sideline-show-hover t ; show hover actions in the sideline
        lsp-ui-doc-use-childframe nil ; childframe has bugs (12/2020); nil works fine
        lsp-ui-sideline-enable nil ; turn off the whole sideline (right sidebar doc & actions)
	))

;; ccls has bugs
;; (use-package ccls
;;   :hook
;;   ((c-mode c++-mode objc-mode cuda-mode) .
;;    (lambda () (require 'ccls) (lsp))))

;; Eglot
(use-package jsonrpc :ensure (:wait t) :defer t)
(use-package eglot
  :ensure nil
  :init (setq completion-category-overrides '((eglot (styles orderless))))
  (setq eglot-send-changes-idle-time 1.0)
  :commands eglot
  :config
  (add-to-list 'eglot-server-programs '(c-mode .("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode .("clangd")))
  (add-to-list 'eglot-server-programs '(objc-mode .("clangd")))
  (add-to-list 'eglot-server-programs '(cuda-mode .("clangd")))
  (add-to-list 'eglot-server-programs '(python-mode .("basedpyright-langserver" "--stdio")))
  )

;; eldoc
(use-package eldoc-box
  :ensure (:wait t)
  :after eglot
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode))

;; Ace window
(use-package ace-window
  :demand t
  :general
  ("M-o" 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Rainbow mode
(use-package rainbow-mode)

;; Tree-sitter
;; NO more! tree-sitter!
(use-package tree-sitter :disabled)
(use-package tree-sitter-langs :disabled)


;;
;; My lisp-func
;;
