;;; keys.el --- Archivo de carga de atajos de teclados personalizados-*- lexical-binding: t -*-

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

    ;; Este archivo contiene mis configuraciones de atajos de teclado personalizados.

    ;;; Code:
   ;;Configurar el undo normal
   (global-set-key (kbd "C-z") 'undo)
   ;; Copiar 
    (global-set-key (kbd "C-c c") 'clipboard-kill-ring-save)
    ;;Cortar
    (global-set-key (kbd "C-c x") 'clipboard-kill-region)
    ;;Pegar
    (global-set-key (kbd "C-c v") 'clipboard-yank) 
    ;;Seleccionar el area o funcion y trabajarla en un contenedor
   (global-set-key (kbd "C-x n n") 'narrow-or-widen-dwim)

   
       ;;Que escape elimine cualquier comando
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
    
        ;; Para reiniciar el buffer
    (global-set-key (kbd "<f5>") 'revert-buffer)
    
        (global-set-key (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
    (global-set-key (kbd "<left-margin> <mouse-1>") 'mu-select-linum)
    (global-set-key (kbd "<left-margin> <drag-mouse-1>") 'mu-select-linum)
    
    ;; Pasar a Scrach de una
    (global-set-key (kbd "C-0") 'cambia-a-búfer-para-notas)
    (defun cambia-a-búfer-para-notas ()
        "Va al búfer *scratch*"
        (interactive) ; no sé qué hace
        (switch-to-buffer "*scratch*"))
    (buffer-name)
    
    ;; Volver al buffer anterior donde estaba trabajando
    (global-set-key (kbd "C-1") 'cambia-al-búfer-anterior)
    (defun cambia-al-búfer-anterior ()
        "Vuelve al último búfer en el que se estaba trabajando antes de cambiar de búfer"
        (interactive) 
        (switch-to-buffer (other-buffer)))
    
        ;; Funcion para que se iluminen los pares de parentesis cuando estoy dentro de ello#+BEGIN_SRC 
    (define-advice show-paren-function (:around (fn) fix)
    "Highlight enclosing parens."
    (cond ((looking-at "\\s(")(funcall fn))
    (t (save-excursion
      (ignore-errors (backward-up-list))
      (funcall fn)))))
      

        ;(define-key mc/keymap (kbd "<return>") nil)
        (global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;;Bookmark
(global-set-key (kbd "C-/") 'bookmark-set)
