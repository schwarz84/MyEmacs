;;; languages.el --- Archivo de configuración de los lenguajes -*- lexical-binding: t -*-

;; Autor: Carlos Schwarz
;; Mantenimiento: Carlos Schwarz
;; Versión: 0.2

;; Este archivo no forma parte del proyecto Emacs

;; Este archivo es software libre; puedes redistribuirlo y/o modificarlo
;; bajo los términos de la GNU General Public License como está publicado en
;; la Free Software Foundation

;; Este programa se distribuye con la esperanza de que sea de utilidad,
;; pero SIN NINGUNA GARANTÍA; sin siquiera la garantía implícita de
;; COMERCIABILIDAD o APTITUD PARA UN PROPÓSITO EN PARTICULAR. Ver la
;; GNU General Public License para más detalles.

;; Para una copia completa de la GNU General Public License
;; ir a <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Este archivo contiene mis configuraciones de los lenguajes que uso y sus ajustes.

;;; Code:

;; HTML
(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.blade\\.php\\'" "\\.vue\\'")
  :config
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t))

;; CSS
(use-package css-mode
  :ensure t
  :mode "\\.css\\'")

;; JavaScript
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2))

;; PHP
(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

;; Vue.js
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'")

;; Flutter
(use-package flutter
  :ensure t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/home/Charly/fvm/versions/3.19.6"))  

(use-package flutter-l10n-flycheck
  :ensure t
  :after dart-mode
  :hook (dart-mode . flutter-l10n-flycheck-setup))  

;; Dart
(use-package dart-mode
  :ensure t
  :hook (dart-mode . lsp)
  :custom
  (dart-format-on-save t)
  :config
  (setq dart-sdk-path "/home/Charly/fvm/versions/3.19.6/bin/cache/dart-sdk"))

(use-package dart-server
  :ensure t
  :hook (dart-mode . dart-server-start)
  :config
  (setq dart-server-sdk-path "/home/Charly/fvm/versions/3.19.6/bin/cache/dart-sdk"))

;; Flutter
(use-package lsp-dart
  :ensure t
  :config
  (setq lsp-dart-sdk-dir "/home/Charly/fvm/versions/3.19.6/bin/cache/dart-sdk"
        lsp-dart-flutter-sdk-dir "/home/Charly/fvm/versions/3.19.6"))

;; LSP Mode
(use-package lsp-mode
  :ensure t
  :hook ((prog-mode . lsp) ; Activar en todos los modos de programación
         (lsp-mode . lsp-ui-mode))
  :commands lsp
  :config
  (setq lsp-enable-snippet t
        lsp-prefer-flymake nil)) ; Usa flycheck en lugar de flymake

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(global-set-key (kbd "C-c l") 'lsp-command-map)

;; Ensure lsp-mode integrates with neotree, ivy, helm, and which-key
(add-hook 'lsp-mode-hook
          (lambda ()
            (lsp-enable-which-key-integration)))

;; Optional: Dap-mode for debugging
(use-package dap-mode
  :ensure t
  :hook (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  (dart-mode . (lambda () (require 'dap-dart))))

(provide 'languages)
;;; languages.el ends here
