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

(setq rustrls-packages '(rust-mode lsp-mode
                                   lsp-rust
                                   lsp-ui
                                   cargo
                                   company-lsp
                                   flycheck
                                   evil-surround
                                   ggtags
                                   helm-gtags
                                   toml-mode))

(defun rustrls/init-rust-mode ()
  (use-package rust-mode
    :defer t
    :init (progn
            (spacemacs/set-leader-keys-for-major-mode
              'rust-mode "q" 'spacemacs/rust-quick-run))))

(defun rustrls/init-cargo ()
  (use-package cargo
    :defer t
    :init (progn
            (spacemacs/declare-prefix-for-mode 'rust-mode
              "mc" "cargo")
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
  (spacemacs/helm-gtags-define-keys-for-mode
   'rust-mode))

(defun rustrls/init-toml-mode ()
  (use-package toml-mode :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'"))

(defun rustrls/init-company-lsp ()
  (use-package company-lsp
    :defer t
    :init (progn
            (spacemacs|add-company-backends :backends company-lsp
                                            :modes rust-mode
                                            :variables company-tooltip-align-annotations
                                            t)
            (setq company-lsp-cache-candidates t)
            (setq company-lsp-async t))
    :after lsp-mode))

(defun rustrls/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; Don't pair lifetime specifiers
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

(defun rustrls/init-lsp-mode ()
  (use-package lsp-mode
    :defer t))

(defun rustrls/init-lsp-ui ()
  (use-package lsp-ui
    :commands lsp-ui-mode
    :init (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    :config (progn (evil-define-minor-mode-key '(normal motion) 'lsp-xref-mode
                     (kbd "j") 'lsp-xref--select-next
                     (kbd "k") 'lsp-xref--select-prev
                     (kbd "<return>") (lambda () (lsp-xref--goto-xref) (lsp-xref--abort))
                     (kbd "<tab>") 'lsp-xref--toggle-file
                     (kbd "<esc>") 'lsp-xref--abort
                     (kbd "q") 'lsp-xref--abort)

                   (spacemacs/set-leader-keys-for-major-mode 'rust-mode
                     "g" 'lsp-xref-find-definitions
                     "f" 'lsp-xref-find-references))
    :after lsp-mode))

(defun rustrls/init-lsp-rust ()
  (use-package lsp-rust
    :config (progn
            ;; call the configured hook
            (spacemacs/add-to-hook 'rust-mode-hook
                                   '((lambda ()
                                       (progn
                                         (setq tab-width 4)
                                         (funcall rustrls-lsp-mode-hook)))))
            (spacemacs/set-leader-keys-for-major-mode 'rust-mode
              "=" 'lsp-format-buffer
              "r" 'lsp-rename
	      "e" 'lsp-rust-explain-error-at-point))
    :after lsp-mode
    :defer t))

;; don't insert tag with evil-surround and '<'
(defun rustrls/post-init-evil-surround ()
  (add-hook 'rust-mode-hook (lambda ()
                              (push '(?< . ("< " . " >")) evil-surround-pairs-alist))))
