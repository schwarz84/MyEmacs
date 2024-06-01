;;; interface.el --- Archivo de carga de tema e interfaz y thema de Emacs -*- lexical-binding: t -*-

    ;; Autor: Carlos Schwarz
    ;; Mantenimiento: Carlos Schwarz
    ;; Version: 1.0

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

    ;; Este archivo contiene mis configuraciones personales para la vista y tema del programa

    ;;; Code:
    ;; Insatalar all the icons
    (use-package all-the-icons
      :if (display-graphic-p))
    
    ;; No mostrar el mensaje de bienvenida
    (setq inhibit-startup-message t)

    (scroll-bar-mode -1) ; Borrar la barra de desplazamiento
    (tool-bar-mode -1)   ; Borra barra de Heeramientas rapidas
    (tooltip-mode -1)    ; Borra las pistas de herramientas
    (set-fringe-mode 10) ; Da un poco mas de espacion en el frame
    (menu-bar-mode -1)   ; Borra la barra de menu

    ;; Mejorar el uso del mouse
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; Que el scroll se mueva una linea a la vez
    (setq mouse-wheel-progressive-speed nil) ;; que el scroll no acelere
    (setq mouse-wheel-follow-mouse 't) ;; Que el scrolle en el frame bajo el mouse
    (setq scroll-step 1) ;; Scroll del teclado mueva una linea por vez
    (setq use-dialog-box nil) ;; Deshabilitar los cuadros de dialogos si no estamos en Mac

    ;; Pone la pantalla completa
    (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
    (add-to-list 'default-frame-alist '(fullscreen . maximized))

    ;; Agrega el numero de columna
    (column-number-mode)

    ;; Ressalta la linea que se esta trabajando
    (global-hl-line-mode 1)

    ;; Tabulaciones a cuatro espacios
    (setq-default tab-width 4)
    (setq-default evil-shift-width tab-width)

    ;;Usar espacios envez de tabulaciones para indentar
    (setq-default indent-tabs-mode nil)

    ;; Que me permitar responder las preguntas criticas con 'y' o 'n'
    (fset 'yes-or-no-p 'y-or-n-p)

    ;; Configuracion UTF8
    (set-default-coding-systems 'utf-8)
    (set-language-environment "UTF-8")

    ;; Listar ibuffer por defecto
    (defalias 'list-buffers 'ibuffer) 
    (defalias 'list-buffers 'ibuffer-other-window) 

    ;; Agregar los numero solo en el texto TODO si agrego treemacs agregarlo
    (dolist (mode '(text-mode-hook
              prog-mode-hook
              conf-mode-hook))
     (add-hook mode (lambda () (display-line-numbers-mode 1))))

    ;; Guarda en el Killer Ring cosas de otros programas sin remplazar lo de Emacs
    (setq save-interprogram-paste-before-kill t)

    ;;Configuracio que auto carga un archivo modificado fuera de emacs
    (global-auto-revert-mode 1)
    (setq auto-revert-verbose nil)
    (global-set-key (kbd "<f5>") 'revert-mode)  
  
  (use-package ligature
    :load-path "~/.emacs.d/config/ligature.el"
    :config
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    ;; Enable all Cascadia Code ligatures in programming modes
    (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                        ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                        "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                        "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                        "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                        "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                        "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                        "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                        ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                        "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                        "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                        "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                         "\\\\" "://"))
    ;; Enables ligature checks globally in all buffers. You can also do it
    ;; per mode with `ligature-mode'.
    (global-ligature-mode t))
    
    (set-frame-font "CaskaydiaCove Nerd Font" t)
   

    
    (whitespace-mode t)
    (global-whitespace-newline-mode t)

   (setq whitespace-display-mappings '((space-mark 32 [183] [46])
   (space-mark 160 [164] [95])
   (tab-mark 9 [187 9] [92 9])))
 
    (setq whitespace-style '(face trailing tabs spaces newline missing-newline-at-eof empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark))
 
 ;;Bookmark color
  
 
 
 (defun my-compilation-mode-hook ()
  (setq truncate-lines nil) ;; automatically becomes buffer local
    (set (make-local-variable 'truncate-partial-width-windows) nil))
    (add-hook 'compilation-mode-hook 'my-compilation-mode-hook)
    
  ;; ;;Tema
  ;;   (use-package gruvbox-theme
  ;;     :ensure t
  ;;     :config
  ;;     (load-theme 'gruvbox-dark-hard t))

      ;; Instalar y cargar el tema Cyberpunk
(use-package cyberpunk-theme
  :ensure t
  :config
  (load-theme 'cyberpunk t))
    
    ;; Modeline de Doom
    (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 13)))
    
      ;; para poder personalizar el modeline
    (use-package minions
    :hook (doom-modeline-mode . minions-mode))
    
    (use-package all-the-icons
  :ensure t)
  
  (setq visible-bell nil
    ring-bell-function 'double-flash-mode-line)
  (defun double-flash-mode-line ()
    (let ((flash-sec (/ 1.0 20)))
      (invert-face 'mode-line)
      (run-with-timer flash-sec nil #'invert-face 'mode-line)
      (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
      (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))

  ;; Cerrar los buffers no deseados en el inicio
(defun close-unwanted-buffers ()
  "Cerrar los buffers no deseados al iniciar Emacs."
  (when (get-buffer "*Messages*")
    (kill-buffer "*Messages*"))
  (when (get-buffer "*straight-process*")
    (kill-buffer "*straight-process*")))

;; Agregar el hook para cerrar los buffers después de la inicialización
(add-hook 'emacs-startup-hook 'close-unwanted-buffers)

;; Iniciar Emacs solo con el buffer *scratch*
(setq initial-buffer-choice t)
(setq inhibit-startup-screen t)

;; Opcional: Desactivar los mensajes en el minibuffer
(setq inhibit-startup-echo-area-message t)

;; Asegurarse de que Emacs no abra otros buffers automáticamente
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message nil)    
      
  ;;    (use-package dashboard
  ;;  :ensure t
  ;;  :config
  ;;  (dashboard-setup-startup-hook))

;; Configuración del dashboard de Emacs

;; ;; Especificar los elementos a mostrar en el dashboard y su cantidad
;; (setq dashboard-items '((recents  . 7)       ; Archivos recientes
;;                         (bookmarks . 7)     ; Marcadores
;;                         (projects . 7)      ; Proyectos
;;                         (agenda . 7)        ; Agenda
;;                         (registers . 7)))   ; Registros

;; ;; Establecer el título del banner
;; (setq dashboard-banner-logo-title "Welcome to my world!!")

;; ;; Establecer la imagen del banner
;; (setq dashboard-startup-banner (concat user-emacs-directory "config/logo/emacs.png"))

;; ;; Mostrar íconos en los encabezados
;; (setq dashboard-set-heading-icons t)

;; ;; Mostrar íconos en los archivos
;; (setq dashboard-set-file-icons t)

;; ;; Configurar la función de cambio de proyecto para que use counsel-projectile
;; (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

;; ;; Activar el navegador en el dashboard
;; (setq dashboard-set-navigator t)

;; ;; Configurar los botones del navegador en el dashboard
;; (setq dashboard-navigator-buttons
;;       `((;; Línea 1
;;          ((,(all-the-icons-faicon "gitlab" :height 1.1 :v-adjust 0.0)
;;            "GitLab" "Mi GitLab"
;;            (lambda (&rest _) (browse-url "https://gitlab.com/csf84")))

;;           (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
;;            "GitHub" "Mi GitHub"
;;            (lambda (&rest _) (browse-url "https://github.com/schwarz84")))

;;           (,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
;;            "LinkedIn" "Mi LinkedIn"
;;            (lambda (&rest _) (browse-url "https://www.linkedin.com/in/carlos-enrique-schwarz-fischer-69a683ba/")))

;;           (,(all-the-icons-fileicon "arch-linux" :height 1.1 :v-adjust 0.0)
;;            "Arch Linux" "HomePage"
;;            (lambda (&rest _) (browse-url "https://archlinux.org/")))))))

;; ;; Mostrar información de inicialización
;; (setq dashboard-set-init-info t)

;; ;; Configurar colores personalizados para los marcadores
;; (custom-set-faces
;;  '(bookmark-face ((t (:foreground "dark olive green")))))

;; ;; Cargar y activar el dashboard
;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook))
