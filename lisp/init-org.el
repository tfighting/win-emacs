;;; init-org.el --- org configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: Org

(use-package org
  :ensure nil
	:bind
  ("C-c a" . org-agenda)

	:hook (org-mode . (lambda () (setq truncate-lines nil)))

  :custom
  (org-tags-column -80)
  (org-catch-invisible-edits 'smart)
  (org-startup-indented t) ;; auto indent.
  (org-log-done 'time) ;; display DONE time
  (org-src-fontify-natively t) ;;synax highlight
  (org-confirm-babel-evaluate nil) ;; don't ask to comfirm
  (org-src-tab-acts-natively t)

  ;; TODO keywords
  (org-todo-keywords
   '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "CANCEL(c)")))

	:config
	(require 'org-tempo)
	(when (file-directory-p "~/org/agenda/")
    (setq org-agenda-files (list "~/org/agenda/")))

	(defun org-export-turn-on-syntax-highlight ()
    "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'."
    (interactive)
    (setq org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-pdf-process
          '("pdflatex -shelnl-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

  (defun org-export-as-pdf-and-open ()
    "Run `org-latex-export-to-pdf', delete the tex file and open pdf in a new buffer."
    (interactive)
    (save-buffer)
    (let* ((pdf-path (org-latex-export-to-pdf))
           (pdf-name (file-name-nondirectory pdf-path)))
      (if (try-completion pdf-name (mapcar #'buffer-name (buffer-list)))
          (progn
            (kill-matching-buffers (concat "^" pdf-name) t t)
            (find-file-other-window pdf-name))
        (find-file-other-window pdf-name))
      (delete-file (concat (substring pdf-path 0 (string-match "[^\.]*\/?$" pdf-path)) "tex"))))

	;; Code block suport run python file.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
		 (latex . t)
		 (emacs-lisp . t)))

  ;; switch markdown
  (use-package ox-gfm)
  (add-to-list 'org-export-backends 'md)
  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

	;; Bind f6 to org-gfm-export-to-markdown.
	(with-eval-after-load 'org
		(define-key org-mode-map (kbd "<f6>") 'org-gfm-export-to-markdown ))

  ;; Generate table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Preview
  (use-package org-preview-html
    :diminish)

	;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-agenda-mode-map
								("P" . org-pomodoro))))



(provide 'init-org)

;;; init-org.el ends here.
