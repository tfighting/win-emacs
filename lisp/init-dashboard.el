;;; init-dashboard.el --- dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: Dashboard

(eval-when-compile
  (require 'init-custom))

(use-package dashboard
    :diminish (dashboard-mode page-break-lines-mode)
    :bind (("<f2>" . open-dashboard)
           :map dashboard-mode-map
           ("H" . browse-homepage)
           ("S" . open-custom-file)
           ("U" . update-all-packages)
           ("q" . quit-dashboard)
	   :map dashboard-mode-map
	   ("j" . dashboard-next-line)
	   ("k" . dashboard-previous-line))
    
    :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
    :init (dashboard-setup-startup-hook)
    :config
    (setq dashboard-banner-logo-title "T_Fighting EMACS - STAY HUNGRY & STAY FOOLISH"
          dashboard-startup-banner t_fighting-logo
          dashboard-center-content t
          dashboard-items '((recents  . 5)
                            (bookmarks . 5)
                            (agenda . 5))

          dashboard-set-footer t ;; wheather to display footer
          dashboard-footer-message (format "T_Fighting, %s" (format-time-string "%Y"))
                                
          dashboard-set-navigator t
          dashboard-navigator-buttons
          `(((,""
              "Homepage"
              "Browse homepage"
              (lambda (&rest _) (browse-url t_fighting-homepage)))

             ;; display custom file
             (,""
              "Settings"
              "Open custom file"
              (lambda (&rest _) (find-file custom-file)))

             ;; display update packages
             (,""
              "Update"
              "Update T_fighting Emacs"
              (lambda (&rest _) (t_fighting-update-all-packages)))
             )))

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")
    
    (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name)
      (goto-char (point-min))
      (delete-other-windows))
    
    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (when (and dashboard-recover-layout-p
                 (bound-and-true-p winner-mode))
        (winner-undo)
        (setq dashboard-recover-layout-p nil)))
 
    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (funcall (local-key-binding "r")))

    (defun dashboard-goto-bookmarks ()
      "Go to bookmarks."
      (interactive)
      (funcall (local-key-binding "m")))

    (defun dashboard-goto-agenda ()
      "Go to agenda."
      (interactive)
      (funcall (local-key-binding "a"))))


(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
