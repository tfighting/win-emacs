;;; init-edit.el --- Better Edit -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: Edit


(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :diminish
  :bind
  (:map smartparens-mode-map
				("C-M-f" . sp-forward-sexp)
				("C-M-b" . sp-backward-sexp))
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "[" nil :actions nil))

;; Delete whole line
(global-set-key (kbd "C-k") 'kill-whole-line)

;; If mark active, copy region otherwise,whole line.
(global-set-key (kbd "M-w")
                (lambda ()
                  (interactive)
                  (if mark-active
                      (kill-ring-save (region-beginning)
                                      (region-end))
                    (progn
                      (kill-ring-save (line-beginning-position)
                                      (line-end-position))
                      (message "copied line")))))

;; If mark active,kill region otherwise, whole line.
(global-set-key (kbd "C-w")
                (lambda ()
                  (interactive)
                  (if mark-active
                      (kill-region (region-beginning)
                                   (region-end))
                    (progn
                      (kill-region (line-beginning-position)
                                   (line-end-position))
                      (message "killed line")))))



;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; Mark to everywhere.
(defun t_fighting-mark-to-char ()
  (interactive)
  (call-interactively #'set-mark-command)
  (call-interactively #'avy-goto-char))
(global-set-key (kbd "C-z m") 't_fighting-mark-to-char)


;; Kill text between the point and the character CHAR
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))


;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)
	)

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2)) ;

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Hideshow
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind ("C-`" . hs-toggle-hiding))




(provide 'init-edit)

;;; init-edit.el ends here.
