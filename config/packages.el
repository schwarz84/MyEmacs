;;; packages.el --- Archivo de carga de los paquetes de Emacs -*- lexical-binding: t -*-

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

   ;; Este archivo contiene mis configuracion de los paquetes que uso
   
   ;;; Code:

   ;; Paquete que permite probar paquetes
   (require 'use-package)
   (setq use-package-always-ensure t)

   ;; Paquete que permite probara paquetes(verso sin esfuerzo)
     (use-package try
        :ensure t)

      ;; Muestra que tecla hace que cuando apreto C-x
      (use-package which-key
        :config
        (which-key-mode)
        (setq which-key-idle-delay 1))

    ;; Org mode para ver archivos markdown de diez
    (use-package org-bullets
      :ensure t
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

    ;;Para cambiar de frame con shift y la flecha
    (windmove-default-keybindings)

    ;; Usar un controlador de buffer numerico muy copadao
    (use-package ace-window
      :ensure t
      :init
      (progn
        (global-set-key [remap other-window] 'ace-window)
        (custom-set-faces
        '(aw-leading-char-face
        ((t (:inherit ace-jump-face-foreground :height 3.0)))))
      ))

    ;; Agrega informacion a los comandos compuestos
    (use-package ivy-rich
      :init
      (ivy-rich-mode 1))

          ;; Instalo Ivy para hacer busquedas mas copadas
    (use-package ivy
      :init (ivy-mode)
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


    ;; Para que aparezca todas las opciones en columna
    (use-package counsel
    :bind (
      ("M-x" . counsel-M-x)
      ("C-x b" . counsel-ibuffer)
      ("C-x C-f" . counsel-find-file)
      ("M-y" . counsel-yank-pop)
      :map minibuffer-local-map
        ("C-r" . 'counsel-minibuffer-history)))
    (use-package counsel-projectile
    :ensure t
    :config
      (counsel-projectile-mode))


    ;; Da info adicional de los comandos
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

    ;; Smex para guardar las opciones mas usadas
    (use-package smex)

    ;; abre el arbol de directorios con iconos bien bonito
    (use-package dired
      :ensure nil
      :commands (dired dired-jump)
      :bind (("C-x C-j" . dired-jump-other-window))
      :custom ((dired-listing-switches "-agho --group-directories-first")))

    (use-package dired-single)

    (use-package all-the-icons-dired
      :hook (dired-mode . all-the-icons-dired-mode))

    ;; Hacer una busqueda por letra para ir mas rapido la use alguna vez en vim
    (use-package avy
      :ensure t
      :bind ("M-s" . avy-goto-char))

    ;; Auto complete de todos los buffers
    (use-package auto-complete
      :ensure t
      :init
      (progn
        (ac-config-default)
        (global-auto-complete-mode t)
      ))

    ;; Paquete para mostra los comandos utilizados en la sesion
    (use-package command-log-mode)

   ;; Flycheck for on-the-fly syntax checking
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

    ;; Snippet pa todo
    (use-package yasnippet
      :ensure t
      :init
      (yas-global-mode 1))
    (add-to-list 'load-path
      "~/.emacs.d/config/plugins/yasnippet")
    (require 'yasnippet)
    (yas-global-mode 1)


    (use-package yasnippet-snippets)

    ;; Autoparentesis
    (use-package smartparens
      :hook (prog-mode . smartparens-mode))

    ;; Alerta
    (use-package alert
      :commands alert
      :config
      (setq alert-default-style 'notifications))

    ;; Guardar el hisotrial de comando por las porcias
    (use-package savehist
      :config
      (setq history-length 50)
      (savehist-mode 1))

    ;; Comentar lineas completas o seleccionadas
    (use-package evil-nerd-commenter
      :bind ("M-/" . evilnc-comment-or-uncomment-lines))

    ;; Resaltar par de corchetes, llaves o parentesis
    (use-package paren
      :config
      (set-face-attribute 'show-paren-match-expression nil :background "#363e4a" :weight 'extra-bold)
      (set-face-attribute 'show-paren-match  nil :background "#363e4a" :foreground "#ffffff" :weight 'extra-bold)
      (show-paren-mode 1))

    ;; Elminar espacios en blanco
    (use-package ws-butler
      :hook ((text-mode . ws-butler-mode)
        (prog-mode . ws-butler-mode)))

    ;; Colorear los pares de parentesis, corchetes o llaves
    (use-package rainbow-delimiters
      :hook (prog-mode . rainbow-delimiters-mode))

    (use-package undo-tree
      :ensure t
      :init
      (global-undo-tree-mode))

    ;; Saltea los espacios blancos al borrar
    (use-package hungry-delete
      :ensure t
      :config
      (global-hungry-delete-mode)
      (setq hungry-delete-join-reluctantly t))

    ;; Expande la seleccion de region en region
     (use-package expand-region
       :ensure t
       :config
       (global-set-key (kbd "M-w") 'er/expand-region)
     )

    ;; Esto sirve para poder separar de forma facil un fragmento y modificarlo a mi antojo
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
      ;; `org-edit-src-code' is not a real narrowing
      ;; command. Remove this first conditional if
      ;; you don't want it.
      (cond ((ignore-errors (org-edit-src-code) t)
        (delete-other-windows))
      ((ignore-errors (org-narrow-to-block) t))
        (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
        (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

   ;; Configuración existente de vterm-toggle
   (use-package vterm-toggle
     :ensure t
     :config
     (setq vterm-toggle-fullscreen-p nil)
     (add-to-list 'display-buffer-alist
                  '((lambda (buffer-or-name _)
                      (let ((buffer (get-buffer buffer-or-name)))
                        (with-current-buffer buffer
                          (or (equal major-mode 'vterm-mode)
                              (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                    (display-buffer-reuse-window display-buffer-at-bottom)
                    ;;(display-buffer-reuse-window display-buffer-in-direction)
                    ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                    ;;(direction . bottom)
                    ;;(dedicated . t) ;dedicated is supported in emacs27
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
  :ensure t
  :config
  (global-set-key (kbd "C-x t") 'multi-vterm) ;; Abrir una nueva terminal
  (global-set-key (kbd "C-x T") 'multi-vterm-dedicated-toggle) ;; Alternar terminal dedicada
  (global-set-key (kbd "C-x p") 'multi-vterm-prev) ;; Ir a la terminal anterior
  )                   
   

;; Configuración de projectile
(use-package projectile
  :ensure t
  :pin melpa-stable
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  ;; Establecer la ruta de búsqueda de proyectos
  (setq projectile-project-search-path '("/home/Charly/proyectos/" "/home/Charly/sandbox/"))
  ;; Activar el caching de projectile
  (setq projectile-enable-caching t)
  ;; Método de indexación
  (setq projectile-indexing-method 'alien)
  ;; Iniciar la búsqueda de proyectos al iniciar
  (projectile-discover-projects-in-search-path))

;; Forzar a projectile a actualizar el caché
(defun projectile-invalidate-cache-on-project-path-change ()
  "Invalidar el caché de projectile cuando cambie la ruta del proyecto."
  (interactive)
  (projectile-invalidate-cache nil)
  (projectile-discover-projects-in-search-path))

;; Atajo para invalidar el caché de projectile manualmente
(global-set-key (kbd "C-c p I") 'projectile-invalidate-cache-on-project-path-change)

;; Configuración de neotree
(use-package neotree
  :ensure t
  :config
  ;; Usa el ícono de la flecha en lugar del símbolo "+", si prefieres.
  (setq neo-theme 'arrow)
  ;; Hace que neotree siga al buffer actual
  (setq neo-smart-open t))

(defun neotree-projectile-action ()
  "Abrir Neotree utilizando la raíz del proyecto con projectile."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
        (neotree-dir project-root)
      (message "No estás en un proyecto reconocido por Projectile."))))

;; Atajo global para abrir neotree con la raíz del proyecto
(global-set-key (kbd "M-1") 'neotree-projectile-action)

;; Para que `neo-buffer--unlock-width` no dé error al cerrar
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key neotree-mode-map (kbd "M-1") 'neotree-hide)
            (setq-local neo-buffer--unlock-width nil)))

            


    (defvar --backup-directory (concat user-emacs-directory "backups"))
      (if (not (file-exists-p --backup-directory))
      (make-directory --backup-directory t))
      (setq backup-directory-alist `(("." . ,--backup-directory)))
      (setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 0               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 7               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
    )

    (defvar --undo-history-directory (concat user-emacs-directory "undos/")
    "Directory to save undo history files.")
    (unless (file-exists-p --undo-history-directory)
      (make-directory --undo-history-directory t))
    ;; stop littering with *.~undo-tree~ files everywhere
    (setq undo-tree-history-directory-alist `(("." . ,--undo-history-directory)))

    (setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))


;;    (use-package treemacs
;;      :ensure t
;;      :defer t
;;      :init
;;      (with-eval-after-load 'winum
;;        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;      :config
;;      (progn
;;        (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
;;              treemacs-deferred-git-apply-delay        0.5
;;              treemacs-directory-name-transformer      #'identity
;;              treemacs-display-in-side-window          t
;;              treemacs-eldoc-display                   'simple
;;              treemacs-file-event-delay                5000
;;              treemacs-file-extension-regex            treemacs-last-period-regex-value
;;              treemacs-file-follow-delay               0.2
;;              treemacs-file-name-transformer           #'identity
;;              treemacs-follow-after-init               t
;;              treemacs-expand-after-init               t
;;              treemacs-find-workspace-method           'find-for-file-or-pick-first
;;              treemacs-git-command-pipe                ""
;;              treemacs-goto-tag-strategy               'refetch-index
;;              treemacs-header-scroll-indicators        '(nil . "^^^^^^")
;;              treemacs-hide-dot-git-directory          t
;;              treemacs-indentation                     1
;;              treemacs-indentation-string              " "
;;              treemacs-is-never-other-window           nil
;;              treemacs-max-git-entries                 5000
;;              treemacs-missing-project-action          'ask
;;              treemacs-move-forward-on-expand          nil
;;              treemacs-no-png-images                   nil
;;              treemacs-no-delete-other-windows         t
;;              treemacs-project-follow-cleanup          nil
;;              treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;              treemacs-position                        'left
;;              treemacs-read-string-input               'from-child-frame
;;              treemacs-recenter-distance               0.1
;;              treemacs-recenter-after-file-follow      nil
;;              treemacs-recenter-after-tag-follow       nil
;;              treemacs-recenter-after-project-jump     'always
;;              treemacs-recenter-after-project-expand   'on-distance
;;              treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
;;              treemacs-show-cursor                     nil
;;              treemacs-show-hidden-files               t
;;              treemacs-silent-filewatch                nil
;;              treemacs-silent-refresh                  nil
;;              treemacs-sorting                         'alphabetic-asc
;;              treemacs-select-when-already-in-treemacs 'move-back
;;              treemacs-space-between-root-nodes        t
;;              treemacs-tag-follow-cleanup              t
;;              treemacs-tag-follow-delay                1.5
;;              treemacs-text-scale                      nil
;;              treemacs-user-mode-line-format           nil
;;              treemacs-user-header-line-format         nil
;;              treemacs-wide-toggle-width               70
;;              treemacs-width                           25
;;              treemacs-width-increment                 1
;;              treemacs-width-is-initially-locked       t
;;              treemacs-workspace-switch-cleanup        nil)
;;
;;        ;; The default width and height of the icons is 22 pixels. If you are
;;        ;; using a Hi-DPI display, uncomment this to double the icon size.
;;        (treemacs-resize-icons 13)
;;
;;        (treemacs-follow-mode t)
;;        (treemacs-filewatch-mode t)
;;        (treemacs-fringe-indicator-mode 'always)
;;        (when treemacs-python-executable
;;          (treemacs-git-commit-diff-mode t))
;;
;;        (pcase (cons (not (null (executable-find "git")))
;;                     (not (null treemacs-python-executable)))
;;          (`(t . t)
;;           (treemacs-git-mode 'deferred))
;;          (`(t . _)
;;           (treemacs-git-mode 'simple)))
;;
;;        (treemacs-hide-gitignored-files-mode nil))
;;      :bind
;;      (:map global-map
;;            ("M-0"       . treemacs-select-window)
;;            ("M-q"   . treemacs-delete-other-windows)
;;            ("M-1"   . treemacs)
;;            ("C-x t d"   . treemacs-select-directory)
;;            ("C-x t B"   . treemacs-bookmark)
;;            ("C-x t C-t" . treemacs-find-file)
;;            ("C-x t M-t" . treemacs-find-tag)))
;;
;;    (use-package treemacs-evil
;;      :after (treemacs evil)
;;      :ensure t)
;;
;;    (use-package treemacs-projectile
;;      :after (treemacs projectile)
;;      :ensure t)
;;
;;    (use-package treemacs-icons-dired
;;      :hook (dired-mode . treemacs-icons-dired-enable-once)
;;      :ensure t)
;;
;;    (use-package treemacs-magit
;;      :after (treemacs magit)
;;      :ensure t)
;;
;;    (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;      :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;      :ensure t
;;      :config (treemacs-set-scope-type 'Perspectives))

    (use-package bm
      :ensure t
      :demand t
      :init
       ;; restore on load (even before you require bm)
       (setq bm-restore-repository-on-load t)
          :config
            ;; Allow cross-buffer 'next'
            (setq bm-cycle-all-buffers t)
           ;; where to store persistant files
            (setq bm-repository-file "~/.emacs.d/bm-repository")
            ;; save bookmarks
             (setq-default bm-buffer-persistence t)
            ;; Loading the repository from file when on start up.
             (add-hook 'after-init-hook 'bm-repository-load)
             ;; Saving bookmarks
             (add-hook 'kill-buffer-hook #'bm-buffer-save)
             ;; Saving the repository to file when on exit.
             ;; kill-buffer-hook is not called when Emacs is killed, so we
             ;; must save all bookmarks first.
             (add-hook 'kill-emacs-hook #'(lambda nil
                                              (bm-buffer-save-all)
                                              (bm-repository-save)))
             ;; The `after-save-hook' is not necessary to use to achieve persistence,
             ;; but it makes the bookmark data in repository more in sync with the file
             ;; state.
             (add-hook 'after-save-hook #'bm-buffer-save)
             ;; Restoring bookmarks
             (add-hook 'find-file-hooks   #'bm-buffer-restore)
             (add-hook 'after-revert-hook #'bm-buffer-restore)
             ;; The `after-revert-hook' is not necessary to use to achieve persistence,
             ;; but it makes the bookmark data in repository more in sync with the file
             ;; state. This hook might cause trouble when using packages
             ;; that automatically reverts the buffer (like vc after a check-in).
             ;; This can easily be avoided if the package provides a hook that is
             ;; called before the buffer is reverted (like `vc-before-checkin-hook').
             ;; Then new bookmarks can be saved before the buffer is reverted.
             ;; Make sure bookmarks is saved before check-in (and revert-buffer)
             (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
               :bind (
               :map global-map
    ;              ("C-\ <right>" . bm-next)
     ;             ("C-\ <left>"  . bm-previous)
      ;            ("C-\ b"       . bm-toggle)
       ;           ("C-\ a"       . bm-show-all ))
               )
                  :custom-face
                  (bm-fringe-persistent-face ((t (:background "dark red" :foreground "smoke white"))))
        )



        (setq bm-highlight-style 'bm-highlight-only-fringe)

        ;;Multi cursor
        (use-package multiple-cursors
          :ensure t
          :bind (("C-S-c C-S-c" . mc/edit-lines)
                 ("M-n" . mc/mark-next-word-like-this)
                 ("M-p" . mc/mark-previous-word-like-this)
                 ("C-M-n" . mc/mark-all-like-this)))

                 ; Content is not centered by default. To center, set
    (setq dashboard-center-content t)


    (use-package ido
      :init (ido-mode))

      ;;Ver cambios en el buffers
      (use-package git-gutter
        :ensure t
        :init
        (global-git-gutter-mode +1))

      ;; El infaltable emmet
      (use-package emmet-mode
        :ensure t
        :config
        (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
        (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
        (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
      )
    ;; Company
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0))

    (use-package lsp-mode
      :init
      ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
      (setq lsp-keymap-prefix "C-l")
      :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
            ; (XXX-mode . lsp)
             ;; if you want which-key integration
             (lsp-mode . lsp-enable-which-key-integration))
      :commands lsp)

    ;; optionally
    (use-package lsp-ui :commands lsp-ui-mode)
    ;; if you are helm user
    (use-package helm-lsp :commands helm-lsp-workspace-symbol)
    ;; if you are ivy user
    (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
    (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

    ;;Java
    (use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
    (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
    (use-package dap-java :ensure nil)
    (use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration)))

    (use-package helm-lsp)
    (use-package helm
      :config (helm-mode))
    (use-package lsp-treemacs)

    ;; optionally if you want to use debugger
    (use-package dap-mode)
    ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

    (setq read-process-output-max (* 1024 1024)) ;; 1mb


    ;; html auto close
    (setq lsp-html-auto-closing-tags t)


      (use-package company
        :ensure t
        :config
        (setq company-tooltip-align-annotations t)
        (setq company-minimum-prefix-length 1)
        (setq company-idle-delay 0.0)
        (setq company-dabbrev-downcase nil)
        (setq company-dabbrev-ignore-case nil)
        (setq company-dabbrev-code-ignore-case nil)
        (setq company-show-numbers t)
        (setq company-transformers '(company-sort-by-occurrence))
        (setq company-selection-wrap-around t)
        (setq completion-ignore-case t)

        ;; Habilitar company-mode para todos los buffers
        (global-company-mode)

        ;; Configurar backends para PHP, JS y Vue
        (add-to-list 'company-backends 'company-php)
        (add-to-list 'company-backends 'company-javascript)
        (add-to-list 'company-backends 'company-web-html)
        (add-to-list 'company-backends 'company-web-jade)
        (add-to-list 'company-backends 'company-web-slim)
        (add-to-list 'company-backends 'company-css)

        ;; Habilitar autocompletado con TAB
        (define-key company-active-map [tab] 'company-complete-common-or-cycle)
        (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
        (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
        (define-key company-active-map (kbd "C-n") 'company-select-next)
        (define-key company-active-map (kbd "C-p") 'company-select-previous))