;;; funcs.el --- C/C++ Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//clang-format-on-save ()
  "Format buffers with ClangFormat when they get saved."
  (when c-c++-ide-enable-clang-format-on-save
    (clang-format-buffer)))

(defun spacemacs/clang-format-on-save ()
  "Add auto-save hook for ClangFormat."
  (add-hook 'before-save-hook 'spacemacs//clang-format-on-save nil t))

;; realgud

(defun spacemacs//short-key-state (modeon)
  "Set evil-evilified-state explicitly."
  (if modeon
      (evil-evilified-state)
    (evil-normal-state)))
