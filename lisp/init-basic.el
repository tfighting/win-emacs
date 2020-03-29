;;; init-basic.el --- Basic Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords:

(eval-when-compile
  (require 'init-custom))

;; Personal information
(setq user-full-name t_fighting-full-name)
(setq user-mail-address t_fighting-mail-address)
(setq frame-title-format '("T_Finghting Emacs - %b")
      icon-title-format frame-title-format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; System Coding UTF-8;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8 . utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inhibit load package at startup.
(setq package-enable-at-startup nil)

;; Disable some UI configuration.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Inhibit startup configuration.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Initial packages
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Use-package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify keybindings map
(setq w32-pass-lwindow-to-system nil
      w32-lwindow-modifier 'super)  ;; Left windows key
(w32-register-hot-key [s-t])  ;; lock screen

;; Cancel keybindings
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "<M-f1>") 'set-mark-command)
(global-set-key (kbd "<f9>") 'eshell)
;; Auto save
(use-package auto-save
	:demand t
	:ensure nil
	:config
	(auto-save-enable)
	(setq auto-save-silent t)   ; quietly save
	(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
	)

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :hook (after-init . show-paren-mode))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package simple
  :ensure nil
  :hook (after-init . size-indication-mode)
  :init
  (setq column-number-mode t
	line-move-visual nil
	track-eol t))        ; Keep cursor at end of lines. Require line-move-visual is nil

;; Scroll one line at a time (less "jumpy" than defaults)
(setq  mouse-wheel-scroll-amount '(1 ((shift) . 1))
       mouse-wheel-progressive-speed nil
       scroll-step 1
       scroll-margin 0
       scroll-conservatively 100000)

;; Mis
(fset 'yes-or-no-p 'y-or-n-p)

;; Set fill-column 80
(setq-default fill-column 80
	      tab-width 2
	      )

(setq visible-bell t
      inhibit-compacting-font-caches t  ;; slove disfluency on windows
      delete-by-moving-to-trash t
      make-backup-files nil
      auto-save-default nil

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      sentence-end "\\([。！？]\\|......\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Some UI Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set default theme
(load-theme 'tango-dark)

;; Hide minor mode
(use-package minions
  :hook (after-init . minions-mode))



(provide 'init-basic)

;;;; init-basic.el ends here.
