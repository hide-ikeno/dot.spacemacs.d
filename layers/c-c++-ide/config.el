;;; config.el --- C/C++ Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defconst c-c++-modes '(c-mode c++-mode)
  "Primary major modes of the `c-c++' layer.")

(defconst c-c++-mode-hooks '(c-mode-hook c++-mode-hook)
  "Primary hooks of the `c-c++' layer.")

(defvar c-c++-ide-enable-clang-support nil
  "If non nil Clang related packages and configuration are enabled.")

(defvar c-c++-ide-enable-clang-format-on-save nil
  "If non-nil, automatically format code with ClangFormat on
  save. Clang support has to be enabled for this to work.")

(spacemacs|define-jump-handlers c++-mode)
(spacemacs|define-jump-handlers c-mode)

(defvar c-c++-ide-default-mode-for-headers 'c-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

(defvar c-c++-ide-irony-user-dir (concat dotspacemacs-directory "irony/")
  "Default parent directory to run cmake.")

(defvar c-c++-ide-cmake-ide-build-pool-dir nil
  "Default parent directory to run cmake.")

(defvar c-c++-ide-cmake-ide-build-pool-use-persistent-naming nil)
