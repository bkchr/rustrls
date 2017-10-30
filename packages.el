;;; packages.el --- Rust Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Chris Hoeppner <me@mkaito.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(add-to-list 'load-path "/home/bastian/.spacemacs.d/plugins/lsp-rust")
(add-to-list 'load-path "/home/bastian/.spacemacs.d/plugins/lsp-mode")

(setq rustrls-packages
  '(
    rust-mode
    (lsp-mode :location "/home/bastian/.spacemacs.d/plugins/lsp-mode")
    lsp-mode
    (lsp-rust :location "/home/bastian/.spacemacs.d/plugins/lsp-rust")
    cargo
    company-lsp
    flycheck
    ggtags
    helm-gtags
    toml-mode
    ))

(defun rustrls/init-rust-mode ()
  (use-package rust-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "=" 'rust-format-buffer
        "q" 'spacemacs/rust-quick-run)
      )
  )
)

(defun rustrls/init-cargo ()
  (use-package cargo
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'rust-mode "mc" "cargo")
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "c." 'cargo-process-repeat
        "cC" 'cargo-process-clean
        "cX" 'cargo-process-run-example
        "cc" 'cargo-process-build
        "cd" 'cargo-process-doc
        "ce" 'cargo-process-bench
        "cf" 'cargo-process-current-test
        "cf" 'cargo-process-fmt
        "ci" 'cargo-process-init
        "cn" 'cargo-process-new
        "co" 'cargo-process-current-file-tests
        "cs" 'cargo-process-search
        "cu" 'cargo-process-update
        "cx" 'cargo-process-run
        "t" 'cargo-process-test))))

(defun rustrls/post-init-flycheck ()
  (spacemacs/enable-flycheck 'rust-mode))

(defun rustrls/post-init-ggtags ()
  (add-hook 'rust-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun rustrls/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'rust-mode))

(defun rustrls/init-toml-mode ()
  (use-package toml-mode
    :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'"))

(defun rustrls/init-company-lsp ()
  (use-package company-lsp
    :init (progn
            (spacemacs|add-company-backends
              :backends company-lsp
              :modes rust-mode
              :variables company-tooltip-align-annotations t
              )
            (setq company-lsp-cache-candidates t)
            (setq company-lsp-async t)
            )
  )
)

(defun rustrls/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; Don't pair lifetime specifiers
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

(defun rustrls/init-lsp-mode ()
  (use-package lsp-mode
    :config
      (use-package lsp-flycheck
        :ensure f ; comes with lsp-mode
        :after flycheck
      )
  )
)

(defun rustrls/init-lsp-rust ()
  (use-package lsp-rust
    :init
    (progn
      ;; call the configured hook
      (spacemacs/add-to-hook 'rust-mode-hook '((lambda () (funcall rustrls-lsp-mode-hook))))
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "r" 'lsp-rename)
    )
    :after lsp-mode))
