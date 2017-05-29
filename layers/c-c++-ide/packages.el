;;; packages.el --- C/C++ IDE Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq c-c++-ide-packages
      '(
        cc-mode
        disaster
        clang-format
        cmake-ide
        cmake-mode
        company
        irony
        (company-irony :toggle (configuration-layer/package-usedp 'company))
        (company-irony-c-headers :toggle (configuration-layer/package-usedp 'company))
        flycheck
        flycheck-irony
        flycheck-rtags
        gdb-mi
        modern-cpp-font-lock
        realgud
        rtags
        (ivy-rtags :toggle (configuration-layer/package-usedp 'ivy))
        ))

(defun c-c++-ide/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (add-to-list 'auto-mode-alist `("\\.h\\'" . ,c-c++-ide-default-mode-for-headers))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1)
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window)))
  )

(defun c-c++-ide/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "D" 'disaster)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "D" 'disaster))))

(defun c-c++-ide/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))

(defun c-c++-ide/init-realgud()
  (use-package realgud
    :defer t
    :commands (realgud:gdb)
    :init
    (progn
      (dolist (mode '(c-mode c++-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "dd" 'realgud:gdb
          "de" 'realgud:cmd-eval-dwim))
      (advice-add 'realgud-short-key-mode-setup
                  :before #'spacemacs//short-key-state)
      (evilified-state-evilify-map realgud:shortkey-mode-map
        :eval-after-load realgud
        :mode realgud-short-key-mode
        :bindings
        "s" 'realgud:cmd-next
        "i" 'realgud:cmd-step
        "b" 'realgud:cmd-break
        "B" 'realgud:cmd-clear
        "o" 'realgud:cmd-finish
        "c" 'realgud:cmd-continue
        "e" 'realgud:cmd-eval
        "r" 'realgud:cmd-restart
        "q" 'realgud:cmd-quit
        "S" 'realgud-window-cmd-undisturb-src))))

(defun c-c++-ide/init-clang-format ()
  (use-package clang-format
    :init
    (when c-c++-ide-enable-clang-format-on-save
      (spacemacs/add-to-hooks 'spacemacs/clang-format-on-save
                              '(c-mode-hook c++-mode-hook)))))

(defun c-c++-ide/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))
  )

(defun c-c++-ide/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-c-headers
            :modes c-mode-common)))

(defun c-c++-ide/init-irony ()
  (use-package irony))

(defun c-c++-ide/init-company-irony ()
  (use-package company-irony))

(defun c-c++-ide/init-company-irony-c-headers ()
  (use-package company-irony-c-headers))

(defun c-c++-ide/post-init-company ()
  (when (configuration-layer/package-usedp 'cmake-mode)
    (spacemacs|add-company-backends :backends company-cmake :modes cmake-mode))
  (spacemacs|add-company-backends :backends company-irony-c-headers company-irony
                                  :modes c-mode-common))


(defun c-c++-ide/init-irony-eldoc ()
  (use-package irony-eldoc)
  :commands (irony-eldoc)
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

(defun c-c++-ide/post-init-flycheck ()
  (dolist (mode '(c-mode c++-mode))
    (spacemacs/enable-flycheck mode)))

(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun c-c++-ide/init-flycheck-irony ()
    (use-package flycheck-irony
      :if (configuration-layer/package-usedp 'flycheck)
      :commands (flycheck-irony-setup)
      :defer t
      :init (add-hook 'irony-mode-hook 'flycheck-irony-setup))))

(defun c-c++-ide/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :defer t
    :init
    (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
    ))

(defun c-c++-ide/init-rtags ()
  (use-package rtags
    :defer t
    :init
    (progn
      (require 'rtags)
      (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
      (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
      (setq rtags-autostart-diagnostics t)
      ))
  )

(defun c-c++-ide/init-ivy-rtags ()
  (use-package ivy-rtags
    :if (configuration-layer/package-usedp 'ivy)
    :ensure rtags)
  )

(defun c-c++-ide/init-flycheck-rtags ()
  (use-package flycheck-rtags
    :ensure rtags)
  )

(defun c-c++-ide/init-cmake-ide ()
  (use-package cmake-ide
    :ensure rtags
    :config
    (setq cmake-ide-build-pool-dir c-c++-ide-cmake-ide-build-pool-dir)
    (setq cmake-ide-build-pool-use-persistent-naming c-c++-ide-cmake-ide-build-pool-use-persistent-naming)
    (cmake-ide-setup)))

