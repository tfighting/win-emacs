;;; init-company.el --- company any ivy -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords:
;; Keywords:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :hook
  ((after-init . ivy-mode)
   (ivy-mode . counsel-mode))

  :bind (("C-s" . swiper-isearch)
         ("C-." . imenu)
         ("C-x C-r" . counsel-buffer-or-recentf)
         ("C-c B" . counsel-bookmarked-directory)
  :map counsel-mode-map
         ([remap dired] . counsel-dired)
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         :map counsel-find-file-map
         ("C-u" . counsel-up-directory)
         ("C-h" . counsel-goto-home-directory))


  :custom
  (enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (ivy-use-virtual-buffer nil)
  (ivy-use-selectable-prompt t)
  (iny-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "[%d/%d]")
  (ivy-wrap t)
  (counsel-find-file-at-point t)
  (counsel-yank-pop-separator "\n————————\n")

  :config
  (defun counsel-goto-home-directory ()
    "Go to the $HOME of the local machine."
    (interactive)
    (ivy--cd "e:/"))

   ;; Integrate yasnippet
  (use-package ivy-yasnippet
    :commands ivy-yasnippet--preview
    :bind ("C-c C-y" . ivy-yasnippet)
    :config (advice-add #'ivy-yasnippet--preview :override #'ignore)))

;; Complete everything
(use-package company
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("M-/" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :init
  (defun my-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0.1
        company-show-numbers t
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
	;; Forbid company mode in some mode.
        company-global-modes '(not erc-mode message-mode help-mode gud-mode shell-mode)
        company-backends '(company-capf)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))

  ;; Better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1)))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (use-package yasnippet-snippets))

(provide 'init-company)

;;;;;init-company.el ends here
