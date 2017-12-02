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
        (clang-format :toggle c-c++-ide-enable-clang-support)
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
        semantic
        srefactor
        stickyfunc-enhance
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
      (dolist (mode c-c++-modes)
        (spacemacs/declare-prefix-for-mode mode "mc" "compile")
        (spacemacs/declare-prefix-for-mode mode "mg" "goto")
        (spacemacs/declare-prefix-for-mode mode "mp" "project")
        (spacemacs/set-leader-keys-for-major-mode mode
          "ga" 'projectile-find-other-file
          "gA" 'projectile-find-other-file-other-window)))
    ))

(defun c-c++-ide/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (dolist (mode c-c++-modes)
      (spacemacs/set-leader-keys-for-major-mode mode
        "D" 'disaster)
      )))
  )

(defun c-c++-ide/init-clang-format ()
  (use-package clang-format
    :if c-c++-ide-enable-clang-support
    :init
    (when c-c++-ide-enable-clang-format-on-save
      (spacemacs/add-to-hooks 'spacemacs/clang-format-on-save
                              c-c++-mode-hooks))))

(defun c-c++-ide/init-cmake-ide ()
  (use-package cmake-ide
    :config
    (setq cmake-ide-build-pool-dir c-c++-ide-cmake-ide-build-pool-dir)
    (setq cmake-ide-build-pool-use-persistent-naming c-c++-ide-cmake-ide-build-pool-use-persistent-naming)
    (progn
      (cmake-ide-setup)
      (dolist (mode c-c++-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "cc" 'cmake-ide-compile
          "pc" 'cmake-ide-run-cmake
          "pC" 'cmake-ide-maybe-run-cmake
          "pd" 'cmake-ide-delete-file)))
    ))

(defun c-c++-ide/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))
  )

(defun c-c++-ide/post-init-company ()
  (when (configuration-layer/package-used-p 'cmake-mode)
    (spacemacs|add-company-backends :backends company-cmake :modes cmake-mode))
  )

(defun c-c++-ide/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-c-headers
            :modes c-mode-common)))

(defun c-c++/post-init-flycheck ()
  (dolist (mode c-c++-modes)
    (spacemacs/enable-flycheck mode)))

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

(defun c-c++-ide/post-init-semantic ()
  (spacemacs/add-to-hooks 'semantic-mode c-c++-mode-hooks))

(defun c-c++-ide/post-init-srefactor ()
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode "r" 'srefactor-refactor-at-point))
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-srefactor c-c++-mode-hooks))

(defun c-c++-ide/post-init-stickyfunc-enhance ()
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-stickyfunc-enhance c-c++-mode-hooks))

(defun c-c++-ide/init-irony ()
  (use-package irony
    :config (setq irony-user-dir c-c++-ide-irony-user-dir
                  irony-server-install-prefix c-c++-ide-irony-user-dir)))

(defun c-c++-ide/init-company-irony ()
  (use-package company-irony))

(defun c-c++-ide/init-company-irony-c-headers ()
  (use-package company-irony-c-headers))

(defun c-c++-ide/post-init-company ()
  (when (configuration-layer/package-usedp 'cmake-mode)
    (spacemacs|add-company-backends :backends company-cmake :modes cmake-mode))
  (spacemacs|add-company-backends :backends company-irony-c-headers company-irony
                                  :modes company-backend-c-mode-common))

(defun c-c++-ide/init-irony-eldoc ()
  (use-package irony-eldoc)
  :commands (irony-eldoc)
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

(defun c-c++-ide/init-flycheck-irony ()
  (use-package flycheck-irony
    :if (configuration-layer/package-usedp 'flycheck)
    :commands (flycheck-irony-setup)
    :defer t
    :init (add-hook 'irony-mode-hook 'flycheck-irony-setup)))

(defun c-c++-ide/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :defer t
    :init
    (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
    ))

(defun c-c++-ide/init-rtags ()
  (use-package rtags
    :defer t
    :config
    (setq rtags-autostart-diagnostics t)
    (if (configuration-layer/package-usedp 'ivy)
        (setq rtags-display-result-backend 'ivy))
          ;; rtags-completions-enabled t
          ;; rtags-use-helm t)
    :init
    (spacemacs|add-company-backends :backends company-rtags
                                    :modes company-backends-c-mode-common)
    (add-hook 'c-mod512e-common-hook 'rtags-start-process-unless-running))
  (use-package flycheck-rtags
    :ensure rtags)
  (use-package ivy-rtags
    :ensure rtags)
  )


