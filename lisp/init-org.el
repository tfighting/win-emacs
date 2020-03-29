;;; init-org.el --- org configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: Org

(use-package org
  :ensure nil
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
   '((sequence "TODO(t)" "PROCESS(p)" "|" "DONE(d)" "CANCEL(c)"))))






(provide 'init-org)

;;; init-org.el ends here.
