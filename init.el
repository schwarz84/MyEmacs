;;; init.el --- Archivo de Configuracion Personal de Emacs -*- lexical-binding: t -*-

;; Autor: Carlos Schwarz
;; Mantenimiento: Carlos Schwarz
;; Version: 1.0

;; Este archivo no forma parte del proyecto Emacs

;; Este archivo es software libre, podes redistribuirlo y/o modificarlo
;; bajo los términos de GNU General Public License como esta publicado en
;; La Free Software Foundation

;; Este programa se distribuye con la esperanza de que sea de utilidad,
;; pero SIN NINGUNA GARANTÍA; sin siquiera la garantía implícita de
;; COMERCIABILIDAD o APTITUD PARA UN PROPÓSITO EN PARTICULAR. Ver la
;; GNU General Public License para más detalles.

;; Para una copia completa de la GNU General Public License
;; ir a <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Esta es mi Setup creada después de años de pruebas y dedicación.
;; No todo el credito es mio sino a la increible comunidad de Emacs que existe.

;;; Code:

;; Por defecto se establece en 800 kilobytes. Medida en Bytes
(setq gc-cons-threshold (* 50 1000 1000))

;; Calculo de tiempo de arranque don datos de la basura
(add-hook 'emacs-startup-hook
          (lambda ()
          ;; Restablecer gc-cons-threshold después del inicio
            (setq gc-cons-threshold (* 800 1024))
            (message "*** Emacs se cargo en %s con %d basura."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Que el compilado de errores trabaja en segundo plano
(setq comp-async-report-warnings-errors nil)

;; Incio las fuentes de paquetes
(require 'package)

(setq package-archives '(
    ("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("elpa" . "https://elpa.gnu.org/packages/")
  )
)

;; Condicion si existe el paquete ya esta instalado actualizarlo
(package-initialize)

(unless package-archive-contents
(package-refresh-contents))

;; Configuracion si se instala en un SO no Linux
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let (
  (bootstrap-file
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

;; Siempre usar straight para instalar en sistemas no linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Usar straight.el para expresiones de use-package
(straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)

;; Cargar el helper package para estos comandos `straight-x-clean-unused-repos'
(require 'straight-x)

;; Perzonalizacion de los archivos temporales
(setq custom-file
  (if (boundp 'server-socket-dir)
    (expand-file-name "custom.el" server-socket-dir)
    (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Carga de org files
(org-babel-load-file (expand-file-name "~/.emacs.d/config/packages.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/config/ui_theme.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/config/keys_and_functions.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/config/languajes.org"))
(put 'narrow-to-region 'disabled nil)