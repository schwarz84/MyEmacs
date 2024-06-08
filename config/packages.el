;;; packages.el --- Archivo de carga de los paquetes de Emacs -*- lexical-binding: t -*-

;; Autor: Carlos Schwarz
;; Mantenimiento: Carlos Schwarz
;; Versión: 1.0

;; Este archivo no forma parte del proyecto Emacs

;; Este archivo es software libre, puedes redistribuirlo y/o modificarlo
;; bajo los términos de la GNU General Public License como está publicado en
;; la Free Software Foundation

;; Este programa se distribuye con la esperanza de que sea de utilidad,
;; pero SIN NINGUNA GARANTÍA; sin siquiera la garantía implícita de
;; COMERCIABILIDAD o APTITUD PARA UN PROPÓSITO EN PARTICULAR. Ver la
;; GNU General Public License para más detalles.

;; Para una copia completa de la GNU General Public License
;; ve a <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Este archivo contiene mis configuraciones de los paquetes que uso.

;;; Code:

;; Asegurarse de que use-package esté instalado y disponible.
(require 'use-package)
(setq use-package-always-ensure t)

;; Paquete que permite probar paquetes sin esfuerzo.
(use-package try)

;; Muestra las teclas disponibles después de una combinación de teclas.
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Mejora la visualización de org-mode.
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Permite cambiar de frame con Shift y las flechas.
(windmove-default-keybindings)

;; Controlador de buffer numérico.
(use-package ace-window
  :init
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

;; Agrega información a los comandos compuestos.
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Instalación y configuración de Ivy para búsquedas mejoradas.
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Para mostrar todas las opciones en columnas.
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ;("C-x b" . counsel-projectile-switch-to-buffer)
         ("C-x C-f" . counsel-find-file)  
         ("M-y" . counsel-yank-pop)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))

;; Integración de Projectile con Counsel.
(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

;; Información adicional sobre los comandos.
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Smex para guardar las opciones más usadas.
(use-package smex)

;; Abre el árbol de directorios con iconos.
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump-other-window))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Búsqueda rápida por letras.
(use-package avy
  :bind ("M-s" . avy-goto-char))

;; Autocompletado en todos los buffers.
(use-package auto-complete
  :init
  (ac-config-default)
  (global-auto-complete-mode t))

;; Mostrar comandos utilizados en la sesión.
(use-package command-log-mode)

;; Flycheck para verificación de sintaxis en tiempo real.
(use-package flycheck
  :hook (after-init . global-flycheck-mode))

;; Snippets para todo.
(use-package yasnippet
  :init
  (yas-global-mode 1))
(use-package yasnippet-snippets)

;; Autocierre de paréntesis.
(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; Notificaciones.
(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

;; Guardar el historial de comandos.
(use-package savehist
  :config
  (setq history-length 50)
  (savehist-mode 1))

;; Comentar líneas completas o seleccionadas.
(use-package evil-nerd-commenter
  :bind ("M-/" . my/comment-and-move-down))

(defun my/comment-and-move-down ()
  "Commentar linea"
  (interactive)
  (evilnc-comment-or-uncomment-lines 1)
  (next-line 1))

;; Resaltar pares de paréntesis, corchetes o llaves.
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a" :weight 'extra-bold)
  (set-face-attribute 'show-paren-match nil :background "#363e4a" :foreground "#ffffff" :weight 'extra-bold)
  (show-paren-mode 1))

;; Eliminar espacios en blanco.
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Colorear pares de paréntesis, corchetes o llaves.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Instalar y configurar undo-tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :bind (("C-z" . undo-tree-undo)       ; Ctrl + z para deshacer
         ("C-S-z" . undo-tree-redo)))   ; Ctrl + Shift + z para rehacer

;; Borrar espacios en blanco.
(use-package hungry-delete
  :config
  (global-hungry-delete-mode)
  (setq hungry-delete-join-reluctantly t))

;; Expansión de selección de región.
(use-package expand-region
  :config
  (global-set-key (kbd "M-w") 'er/expand-region))

;; Para poder separar y modificar fácilmente un fragmento.
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; Configuración de vterm-toggle
(use-package vterm-toggle
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.25)))
  (global-set-key (kbd "C-t") 'vterm-toggle))

;; Integración con projectile para abrir en el directorio raíz del proyecto
(defun open-vterm-in-project-root ()
  "Abrir vterm en el directorio raíz del proyecto."
  (interactive)
  (let ((default-directory (if (projectile-project-p)
                               (projectile-project-root)
                             default-directory)))
    (vterm-toggle)))

;; Modificar atajo de teclado para abrir/ocultar vterm en el directorio raíz del proyecto
(global-set-key (kbd "C-t")
                (lambda ()
                  (interactive)
                  (if (get-buffer-window "*vterm*")
                      (delete-window (get-buffer-window "*vterm*"))
                    (open-vterm-in-project-root))))

;; Configuración de multi-vterm para múltiples terminales
(use-package multi-vterm
  :config
  (global-set-key (kbd "C-x t") 'multi-vterm)
  (global-set-key (kbd "C-x T") 'multi-vterm-dedicated-toggle)
  (global-set-key (kbd "C-x p") 'multi-vterm-prev))

;; Configuración de projectile
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("/home/Charly/proyectos/" "/home/Charly/sandbox/"))
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (projectile-discover-projects-in-search-path))

;; Atajo para invalidar el caché de projectile manualmente
(global-set-key (kbd "C-c p I") 'projectile-invalidate-cache-on-project-path-change)

(define-key projectile-mode-map (kbd "C-c u") 'helm-projectile)    

;; Configuración de helm
(use-package helm
  :config
  (setq helm-display-buffer-height 45 ;; Ajusta el tamaño del buffer de Helm (25% de la altura del frame)
        helm-autoresize-min-height 45 ;; Tamaño mínimo del buffer de Helm
        helm-autoresize-max-height 45 ;; Tamaño máximo del buffer de Helm
        helm-split-window-inside-p t  ;; Para abrir Helm en una ventana vertical
        helm-split-window-default-side 'below) ;; Opción para ventana vertical
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  :bind (("C-x b" . helm-projectile-switch-to-buffer)))

;; Configuración de helm-rg
(use-package helm-rg
  :ensure t
  :config
  (setq helm-rg-default-extra-args "--hidden"))

(use-package helm-ls-git
  :ensure t
  :commands (helm-ls-git-ls))  

;; Configura un atajo de teclado para abrir helm-ls-git
(global-set-key (kbd "C-x C-g") 'helm-ls-git-ls)  

;; Función para buscar en todo el proyecto usando ripgrep y mostrar vista previa
(defun my/helm-projectile-rg-with-preview ()
  "Buscar en todo el proyecto usando ripgrep y mostrar la vista previa."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (helm-rg (read-string "Buscar: ") (projectile-project-root))))

;; Atajo de teclado para la función de búsqueda con vista previa
(global-set-key (kbd "C-c y") 'my/helm-projectile-rg-with-preview)

;; Función para ignorar buffers no deseados en helm-projectile
(defun my/helm-projectile-ignore-buffer (buffer)
  "Ignora buffers que no pertenecen al proyecto actual."
  (let ((bufname (buffer-name buffer)))
    (or (string-match-p "^\\*" bufname) ;; Ignorar buffers internos de Emacs
        (string-match-p "^magit" bufname) ;; Ignorar buffers de Magit
        (string-match-p "^LSP" bufname))))

(setq helm-boring-buffer-regexp-list '("\\*.*\\*" "magit.*" "LSP.*"))

;; Asegúrate de tener instalados los paquetes necesarios
(use-package all-the-icons)
(use-package neotree
  :config
  (setq neo-theme 'icons)
  (setq neo-smart-open t)
  (setq neo-vc-integration '(face))  ;; Agrega esta línea para resaltar archivos modificados
  (global-set-key (kbd "C-M-1") 'neotree-find))

(defun neotree-find-current-file ()
  "Abre Neotree y selecciona el archivo actual."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (neotree-find current-file)))

;; Asigna la función a la tecla C-M-1
(global-set-key (kbd "C-M-1") 'neotree-find-current-file)

(defun neotree-projectile-action ()
  "Abrir Neotree utilizando la raíz del proyecto con projectile."
  (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
      (message "No estás en un proyecto reconocido por Projectile."))))

(global-set-key (kbd "M-1") 'neotree-projectile-action)

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key neotree-mode-map (kbd "M-1") 'neotree-hide)
            (setq-local neo-buffer--unlock-width nil)))

;; Añade los iconos de all-the-icons para Neotree
(use-package all-the-icons
  :if (display-graphic-p))

;; Instalación de los iconos (solo es necesario hacer esto una vez)
;; (all-the-icons-install-fonts)


;; Configuración de backups y undo history.
(defvar --backup-directory (concat user-emacs-directory "backups"))
(unless (file-exists-p --backup-directory)
  (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 0
      kept-new-versions 7
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

(defvar --undo-history-directory (concat user-emacs-directory "undos/")
  "Directory to save undo history files.")
(unless (file-exists-p --undo-history-directory)
  (make-directory --undo-history-directory t))
(setq undo-tree-history-directory-alist `(("." . ,--undo-history-directory)))

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Configuración de bookmarks
(use-package bm
  :init
  (setq bm-restore-repository-on-load t)
  :config
  (setq bm-cycle-all-buffers t)
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  (setq-default bm-buffer-persistence t)
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook (lambda nil
                               (bm-buffer-save-all)
                               (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  :bind (("C-<right>" . bm-next)
         ("C-<left>" . bm-previous)
         ("C-b" . bm-toggle)
         ("C-a" . bm-show-all))
  :custom-face
  (bm-fringe-persistent-face ((t (:background "dark red" :foreground "smoke white"))))
  (setq bm-highlight-style 'bm-highlight-only-fringe))

;; Multi cursor
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("M-n" . mc/mark-next-word-like-this)
         ("M-p" . mc/mark-previous-word-like-this)
         ("C-M-n" . mc/mark-all-like-this)))

;; IDO mode
(use-package ido
  :init
  (ido-mode))

;; Ver cambios en los buffers.
(use-package git-gutter
  :init
  (global-git-gutter-mode +1))

;; Emmet para autocompletar en HTML y CSS.
(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

;; Company mode para autocompletado.
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 0
        company-idle-delay 0
        company-selection-wrap-around t))

;; Configuración adicional de Company.
(use-package company
  :config
  (setq company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil
        company-show-numbers t
        company-transformers '(company-sort-by-occurrence)
        company-selection-wrap-around t
        completion-ignore-case t)
  (global-company-mode)
  (add-to-list 'company-backends 'company-php)
  (add-to-list 'company-backends 'company-javascript)
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim)
  (add-to-list 'company-backends 'company-css)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))    

(setq lsp-completion-provider :capf)  ;; Usar company-capf para la autocompletación

;; Hook para esconder las sugerencias al perder el foco
(add-hook 'focus-out-hook 'company-abort)

;; Función para esconder las sugerencias con Escape
(defun my/company-hide-suggestions ()
  (interactive)
  (company-abort))

;; Mapea la tecla Escape para esconder las sugerencias
(define-key company-active-map (kbd "<escape>") 'my/company-hide-suggestions)

;; Mapea la tecla Enter para no seleccionar ninguna sugerencia
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map (kbd "<return>") nil)
(define-key company-active-map (kbd "M-RET") 'company-complete-selection)  ;; Si quieres, puedes usar M-RET para seleccionar una sugerencia)



;; Personalizar el menú de autocompletado para que sea más amigable
(use-package company-box
  :hook (company-mode . company-box-mode)
  :ensure t)  

;; Instalar y configurar exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  ;; Asegúrate de que Emacs cargue las variables de entorno de la shell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    ;; Carga variables específicas del archivo .bashrc
    (exec-path-from-shell-copy-envs '("PATH" "ANDROID_HOME" "ANDROID_SDK_ROOT" "CHROME_EXECUTABLE"))))  

(provide 'packages)
;;; packages.el ends here
