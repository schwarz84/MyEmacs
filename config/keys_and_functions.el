;;; keys_and_functions.el --- Archivo de carga de atajos de teclados personalizados -*- lexical-binding: t -*-

;; Autor: Carlos Schwarz
;; Mantenimiento: Carlos Schwarz
;; Versión: 0.1

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

;; Este archivo contiene mis configuraciones de atajos de teclado personalizados.

;;; Code:

;; Configurar el undo normal
(global-set-key (kbd "C-z") 'undo)
;; Copiar 
(global-set-key (kbd "C-c c") 'clipboard-kill-ring-save)
;; Cortar
(global-set-key (kbd "C-c x") 'clipboard-kill-region)
;; Pegar
(global-set-key (kbd "C-c v") 'clipboard-yank) 
;; Seleccionar el área o función y trabajarla en un contenedor
(global-set-key (kbd "C-x n n") 'narrow-or-widen-dwim)

;; Pasar a scratch
(global-set-key (kbd "C-0") 'cambia-a-búfer-para-notas)
(defun cambia-a-búfer-para-notas ()
  "Va al búfer *scratch*"
  (interactive)
  (switch-to-buffer "*scratch*"))

;; Volver al buffer anterior donde estaba trabajando
(global-set-key (kbd "C-1") 'cambia-al-búfer-anterior)
(defun cambia-al-búfer-anterior ()
  "Vuelve al último búfer en el que se estaba trabajando antes de cambiar de búfer"
  (interactive) 
  (switch-to-buffer (other-buffer)))

;; Función para que se iluminen los pares de paréntesis cuando estoy dentro de ellos
(define-advice show-paren-function (:around (fn) fix)
  "Highlight enclosing parens."
  (cond ((looking-at "\\s(")(funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))

;; (define-key mc/keymap (kbd "<return>") nil)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; Bookmark
(global-set-key (kbd "C-/") 'bookmark-set)

;; Función para duplicar una línea de código
(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let ((line-contents (buffer-substring (line-beginning-position) (line-end-position))))
    (end-of-line)
    (newline)
    (insert line-contents)))

;; Asignar atajo de teclado para duplicar línea
(global-set-key (kbd "M-d") 'duplicate-line)

;; Run Programa
(defun run-dart-application ()
 "Run Dart or Flutter application."
 (interactive)
 (let ((default-directory (if (projectile-project-p)
                              (projectile-project-root)
                              default-directory)))
     (compile "flutter run")))

 (global-set-key (kbd "C-c r") 'run-dart-application)

 (defun run-dart-file ()
  "Run the current Dart file."
  (interactive)
  (let ((default-directory (if (projectile-project-p)
                               (projectile-project-root)
                             default-directory))
        (file (buffer-file-name)))
    (if file
        (compile (format "dart run %s" file))
      (message "No file is currently being edited."))))

 (global-set-key (kbd "C-c d") 'run-dart-file) 

 ;; Configurar la depuración para Dart y Flutter
 (defun debug-dart-application ()
   "Debug the current Dart file."
   (interactive)
   (let ((default-directory (if (projectile-project-p)
                                (projectile-project-root)
                              default-directory))
         (file (buffer-file-name)))
     (if file
         (dap-debug (list :type "dart"
                          :request "launch"
                          :name "Dart Debug"
                          :program file
                          :cwd default-directory))
       (message "No file is currently being edited."))))

  ;; Asignar atajo de teclado para depurar la aplicación Dart
  (global-set-key (kbd "C-c D") 'debug-dart-application)
