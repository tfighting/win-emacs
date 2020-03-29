;;; init-search.el --- Better query and replace -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: search replace

;; "M-%" "C-M-%"
(use-package anzu
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(require 'color-rg)
(global-set-key (kbd "C-M-s") 'color-rg-search-input-in-project)


(provide 'init-search)

;;; init-search.el ends here.
