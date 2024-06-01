;;; languajes.el --- Archivo de carga de los distintos lenguajes que uso -*- lexical-binding: t -*-

  ;; Autor: Carlos Schwarz
  ;; Mantenimiento: Carlos Schwarz
  ;; Version: 0.1

  ;; Este archivo no forma parte del proyecto Emacs

  ;; Este archivo es software libre, podes redistribuirlo y/o modificarlo
  ;; bajo los terminso de GNU General Public License como esta publicado en
  ;; La Free Software Foundation

  ;; Este programa se distribuye con la esperanza de que sea de utilidad,
  ;; pero SIN NINGUNA GARANTÍA; sin siquiera la garantía implícita de
  ;; COMERCIABILIDAD o APTITUD PARA UN PROPÓSITO EN PARTICULAR. Ver la
  ;; GNU General Public License para más detalles.

  ;; Para una copia completa de la GNU General Public License
  ;; ir a <http://www.gnu.org/licenses/>.


  ;;; Commentary:

  ;; Este archivo contiene mis configuraciones de los lenguajes que uso y sus seteos.

  ;;; Code:

  ;;; HTML
  ;; Paquetes para hacer parrafos lorem ipsum
  (use-package lorem-ipsum
   :ensure t
   :init)

;; LSP Mode
(use-package lsp-mode
  :ensure t
  :hook ((prog-mode . lsp) ; Activar en todos los modos de programación
         (lsp-mode . lsp-ui-mode))
  :commands lsp
  :config
  (setq lsp-enable-snippet t
        lsp-prefer-flymake nil)) ; Usa flycheck en lugar de flymake

  (use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)


(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

  ;; Global key bindings for LSP
(global-set-key (kbd "C-c l") lsp-command-map)

;; Ensure lsp-mode integrates with neotree, ivy, helm, and which-key
(add-hook 'lsp-mode-hook
          (lambda ()
            (add-to-list 'lsp-after-open-hook 'lsp-enable-which-key-integration)))

(provide 'init-lsp)

;; LSP UI
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

    ;; LSP Dart
    (use-package lsp-dart
      :ensure t
      :config
      (setq lsp-dart-sdk-dir "/home/Charly/fvm/versions/3.19.6/bin/cache/dart-sdk")
      (setq lsp-dart-flutter-sdk-dir "/home/Charly/fvm/versions/3.19.6"))    

    ;; Optional: Dap-mode for debugging
    (use-package dap-mode
      :ensure t
      :hook
      (lsp-mode . dap-mode)
      (lsp-mode . dap-ui-mode)
      (dart-mode . (lambda () (require 'dap-dart))))
    
    ;; Configura la ruta de Flutter y Dart SDK (ajusta según tu sistema)
    (setq dart-sdk-path "/home/Charly/fvm/versions/3.19.6/bin/cache/dart-sdk")
    (setq lsp-dart-flutter-sdk-dir "/home/Charly/fvm/versions/3.19.6")      
   
  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
	    ("\\.md\\'" . markdown-mode)
	    ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "markdown"))

  (use-package web-mode
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
    (setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
      ("blade"  . "\\.blade\\.")))
    (setq web-mode-ac-sources-alist
    '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t))

  ;; Instalar el paquete de soporte de PHP
  (unless (package-installed-p 'php-mode)
    (package-refresh-contents)
    (package-install 'php-mode))

  ;; Cargar el paquete de soporte de PHP
  (require 'php-mode)


  ;; Instalar el paquete de soporte de Laravel
  ;; (unless (package-installed-p 'laravel-mode)
  ;;   (package-refresh-contents)
  ;;   (package-install 'laravel-mode))

  ;; ;; Cargar el paquete de soporte de Laravel
  ;; (require 'laravel-mode)


  ;; Instalar el paquete de soporte de JS
  (unless (package-installed-p 'js2-mode)
    (package-refresh-contents)
    (package-install 'js2-mode))

  ;; Cargar el paquete de soporte de JS
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  ;; Instalar el paquete de soporte de Vue
  (unless (package-installed-p 'vue-mode)
    (package-refresh-contents)
    (package-install 'vue-mode))

  ;; Cargar el paquete de soporte de Vue
  (require 'vue-mode)

  ;; Dart Mode
  (use-package dart-mode
    :ensure t
    :hook (dart-mode . lsp)
    :custom
    (dart-format-on-save t)
    :config
    ;; Define el path de Flutter (ajusta según tu sistema)
    (setq dart-sdk-path "/home/Charly/fvm/versions/3.19.6/bin/cache/dart-sdk"))
