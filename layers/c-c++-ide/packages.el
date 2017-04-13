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
      (c-toggle-auto-newline 1)
      ;; (add-hook 'c++-mode-hook 'c-c++-ide-cc-mode--add-keywords-for-c++11())
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window))))

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

(defun c-c++-ide/init-clang-format ()
  (use-package clang-format
    :commands (clang-format-buffer)
    :if c-c++-ide-enable-clang-support
    :init
    (defun clang-format-save-hook ()
      "Run clang-format on save when in c or c++ mode."
      (interactive)
      (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (clang-format-buffer)))
    (add-hook 'before-save-hook 'clang-format-save-hook)
    ))

(defun c-c++-ide/init-cmake-ide ()
  (cmake-ide-setup))

(defun c-c++-ide/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init (push 'company-cmake company-backends-cmake-mode)))

(defun c-c++-ide/post-init-company ()
  (spacemacs|add-company-hook c-mode-common)
  (spacemacs|add-company-hook cmake-mode))

(defun c-c++-ide/init-irony ()
  (use-package irony
    :defer t
    :commands (irony-mode irony-install-server)
    :init
    (progn
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'c++-mode-hook 'irony-mode))
    :config
    (progn
      (setq irony-user-dir c-c++-ide-irony-user-dir)
      (setq irony-server-install-prefix irony-user-dir)
      (defun irony/irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))

      (add-hook 'irony-mode-hook 'irony/irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun c-c++-ide/init-company-irony ()
    (use-package company-irony
      :if (configuration-layer/package-usedp 'company)
      :ensure company
      :commands (company-irony)
      :defer t
      :init
      (progn
        (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
        (push 'company-irony company-backends-c-mode-common)))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun c-c++-ide/init-company-irony-c-headers ()
    (use-package company-irony-c-headers
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :commands (company-irony-c-headers)
      :init
      (push 'company-irony-c-headers company-backends-c-mode-common))))

(defun c-c++-ide/init-irony-eldoc ()
  (use-package irony-eldoc
    :commands (irony-eldoc)
    :init
    (add-hook 'irony-mode-hook 'irony-eldoc)))

(defun c-c++-ide/post-init-flycheck ()
  (dolist (mode '(c-mode c++-mode))
    (spacemacs/add-flycheck-hook mode)))

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
      (dolist (mode '(c-mode c++-mode))
        (evil-leader/set-key-for-mode mode
          "mg." 'rtags-find-symbol-at-point
          "mg," 'rtags-find-references-at-point
          "mgv" 'rtags-find-virtuals-at-point
          "mgV" 'rtags-print-enum-value-at-point
          "mg/" 'rtags-find-all-references-at-point
          "mgY" 'rtags-cycle-overlays-on-screen
          "mg>" 'rtags-find-symbol
          "mg<" 'rtags-find-references
          "mg[" 'rtags-location-stack-back
          "mg]" 'rtags-location-stack-forward
          "mgD" 'rtags-diagnostics
          "mgG" 'rtags-guess-function-at-point
          "mgp" 'rtags-set-current-project
          "mgP" 'rtags-print-dependencies
          "mge" 'rtags-reparse-file
          "mgE" 'rtags-preprocess-file
          "mgR" 'rtags-rename-symbol
          "mgM" 'rtags-symbol-info
          "mgS" 'rtags-display-summary
          "mgO" 'rtags-goto-offset
          "mg;" 'rtags-find-file
          "mgF" 'rtags-fixit
          "mgL" 'rtags-copy-and-print-current-location
          "mgX" 'rtags-fix-fixit-at-point
          "mgB" 'rtags-show-rtags-buffer
          "mgI" 'rtags-imenu
          "mgT" 'rtags-taglist
          "mgh" 'rtags-print-class-hierarchy
          "mga" 'rtags-print-source-arguments))
      ))
  )

(defun c-c++-ide/init-ivy-rtags ()
  (use-package ivy-rtags
    :if (and (configuration-layer/package-usedp 'ivy)
             (configuration-layer/package-usedp 'rtags))
    ))

(defun c-c++-ide/init-flycheck-rtags ()
  (use-package flycheck-rtags
    :if (and (configuration-layer/package-usedp 'flycheck)
             (configuration-layer/package-usedp 'rtags))
  ))

(defun c-c++-ide/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))

