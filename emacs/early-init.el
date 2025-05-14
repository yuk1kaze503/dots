;;;
;;; Early init
;;;


(setq package-enable-at-startup nil)
(menu-bar-mode -1) ;; disables menubar
(tool-bar-mode -1) ;; disables toolbar
(scroll-bar-mode -1) ;; disables scrollbar
(pixel-scroll-precision-mode 1) ;; enable smooth scrolling

(setq user-emacs-directory (file-truename "~/.emacs.d/"))
(setq inhibit-splash-screen t ;; no thanks
        use-file-dialog nil ;; don't use system file dialog
        tab-bar-new-button-show nil ;; don't show new tab button
        tab-bar-close-button-show nil ;; don't show tab close button
        tab-line-close-button-show nil) ;; don't show tab close button
