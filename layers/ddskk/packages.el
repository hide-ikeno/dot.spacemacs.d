;;; packages.el --- DDSKK Layer packages File for Spacemacs
;;
;; Copyright (c) 2016 Hidekazu Ikeno
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ddskk-packages
  '(
    ddskk
    ))

(defun ddskk/init-ddskk ()
  (use-package skk-autoloads
    :bind ( ;;("C-x C-j" . skk-mode)
           ("C-x j" . skk-auto-fill-mode))
    ;;(global-set-key "\C-xt" 'skk-tutorial)
    :defines skk-init-file skk-isearch-start-mode
    :init
    (add-hook 'isearch-mode-hook
              (lambda () (and (boundp 'skk-mode) skk-mode
                              (skk-isearch-mode-setup))))
    (add-hook 'isearch-mode-end-hook
              (lambda ()
                (and (featurep 'skk-isearch) (skk-isearch-mode-cleanup))))
    :config
    (progn
      (use-package skk-leim)
      (setq default-input-method 'japanese-skk)
      (setq skk-init-file (locate-user-emacs-file ".skk"))
      (setq skk-isearch-start-mode 'latin)
      ))
  )
