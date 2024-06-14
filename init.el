;;; init.el --- Archivo de Configuración Personal de Emacs -*- lexical-binding: t -*-

;; Autor: Carlos Schwarz
;; Mantenimiento: Carlos Schwarz
;; Versión: 1.0

;; Este archivo no forma parte del proyecto Emacs

;; Este archivo es software libre; puedes redistribuirlo y/o modificarlo
;; bajo los términos de la GNU General Public License como está publicado en
;; la Free Software Foundation

;; Este programa se distribuye con la esperanza de que sea de utilidad,
;; pero SIN NINGUNA GARANTÍA; sin siquiera la garantía implícita de
;; COMERCIABILIDAD o APTITUD PARA UN PROPÓSITO EN PARTICULAR. Ver la
;; GNU General Public License para más detalles.

;; Para una copia completa de la GNU General Public License
;; ve a <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Esta es mi configuración creada después de años de pruebas y dedicación.
;; No todo el crédito es mío, sino también de la increíble comunidad de Emacs.

;;; Code:

;; Ajustar el límite del recolector de basura a 50MB para mejorar el rendimiento inicial.
(setq gc-cons-threshold (* 50 1000 1000))

;; Calcular el tiempo de arranque con detalles sobre el recolector de basura.
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restablecer gc-cons-threshold después del inicio.
            (setq gc-cons-threshold (* 8 1000 1000))
            (message "*** Emacs se cargó en %s con %d recolecciones de basura."
                     (format "%.2f segundos"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Hacer que los errores de compilación trabajen en segundo plano.
(setq comp-async-report-warnings-errors nil)

;; Inicializar los paquetes.
(require 'package)
(setq package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Configurar e instalar `use-package` si no está ya instalado.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Siempre usar straight para instalar paquetes, excepto en GNU/Linux.
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Usar straight.el con use-package.
(straight-use-package 'use-package)

;; Cargar el helper package `straight-x`.
(require 'straight-x)

;; Personalización de los archivos temporales.
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Cargar archivos org, verificando su existencia.
(dolist (file '("~/.emacs.d/config/packages.org"
                "~/.emacs.d/config/ui_theme.org"
                "~/.emacs.d/config/keys_and_functions.org"
                "~/.emacs.d/config/languages.org"
                ))
  (when (file-exists-p (expand-file-name file))
    (org-babel-load-file (expand-file-name file))))

;; Habilitar la función `narrow-to-region`.
(put 'narrow-to-region 'disabled nil)

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
