;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "emPeeGee"
      user-mail-address "")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-gruvbox-dark-variant "soft")
(setq doom-theme 'doom-gruvbox)
(setq solarized-scale-org-headlines nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (set-face-attribute 'default nil :height 145)

(setq evil-want-fine-undo t ; By default while in insert all changes are one big blob. Be more granular
  auto-save-default t ; Nobody likes to loose work, I certainly don't
  truncate-string-ellipsis "…" ; Unicode ellipsis are nicer than "...", and also save /precious/ space
  scroll-preserve-screen-position 'always ; Don't have `point' jump around
  display-fill-column-indicator-column 80
  scroll-margin 10 ; It's nice to maintain a little margin
  display-time-24hr-format t
  display-time-interval 1)


(global-subword-mode 1) ; Iterate through CamelCase words

;; TODO: ???
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; I don't use daemon on mac
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(height . 54))
  (add-to-list 'default-frame-alist '(width . 125))
  (add-to-list 'default-frame-alist '(left . 0))
  (add-to-list 'default-frame-alist '(top . 0))
  (add-to-list 'default-frame-alist '(undecorated . t))
  (set-face-attribute 'default nil :font "JetBrains Mono-18")
  (setq all-the-icons-scale-factor 1))

(when (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(height . 57))
  (add-to-list 'default-frame-alist '(width . 100))
  (set-face-attribute 'default nil :font "JetBrains Mono-14")
  (setq all-the-icons-scale-factor 1))

;; https://github.com/termitereform/JunkPile/blob/master/emacs-on-windows.md#creating-a-safe-start-shortcut
;; https://emacs.stackexchange.com/questions/46541/running-emacs-as-a-daemon-does-not-load-custom-set-faces
(defun windows-face()
  "Setup specific windows settings"
  (when (eq system-type 'windows-nt)
    (message "Setting windows")
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
    (setq-default ispell-program-name "C:/msys64/ucrt64/bin/aspell.exe")
    (setq ispell-extra-args '("--encoding=utf-8" "--sug-mode=ultra" "--lang=en" "--run-together" "--camel-case"))
    ;; (set-face-attribute 'default nil :font "JetBrains Mono-12")
    (setq doom-font (font-spec :family "JetBrains Mono" :size 12.0))
    (setq all-the-icons-scale-factor 1.2)))

(defun my-frame-tweaks (&optional frame)
  "My personal frame tweaks."
  (unless frame
    (setq frame (selected-frame)))
  (when frame
    (with-selected-frame frame
      (when (display-graphic-p)
        (tool-bar-mode -1))))
    (windows-face))

(defun my/set-up-doom-modeline-font ()
  "Set up doom modeline font"
  (after! doom-modeline
    (when (eq system-type 'windows-nt)
      (set-face-attribute 'mode-line nil :font "JetBrains Mono-10"))
    (when (eq system-type 'darwin)
      (custom-set-faces!
        '(mode-line :family "JetBrains Mono" :height 150)
        '(mode-line-inactive :family "JetBrains Mono" :height 150)))))



;; For the case that the init file runs after the frame has been created. Call of emacs without --daemon option.
(my-frame-tweaks)
;; For the case that the init file runs before the frame is created. Call of emacs with --daemon option.
(add-hook 'after-make-frame-functions #'my-frame-tweaks t)
;; Works in both emacs client and emacs
(setq initial-buffer-choice (lambda ()
    (org-agenda-list 7)
    (get-buffer "*Org Agenda*")))


(add-hook 'emacs-startup-hook  #'my/set-up-doom-modeline-font)
(add-hook! org-mode 'rainbow-mode)
(add-hook! prog-mode 'rainbow-mode)

(custom-set-faces!
  ;; TODO: Another color and for light too
  `(org-level-6 :inherit outline-4 :extend t :height 1.05)
  `(org-level-5 :inherit outline-4 :extend t :height 1.10)
  `(org-level-4 :inherit outline-4 :extend t :height 1.15)
  `(org-level-3 :inherit outline-3 :extend t :height 1.20)
  `(org-level-2 :inherit outline-2 :extend t :height 1.25)
  `(org-level-1 :inherit outline-1 :extend t :height 1.30)
  '(hl-line :background "#293b52" :extend t)
  '(region :background "#3e4e63") ;; selected
  '(demap-minimap-font-face :family "minimap" :height 15)
  '(flyspell-incorrect :underline (:color "green" :style wave))
  '(flyspell-duplicate :underline (:color "cyan" :style wave))
  '(font-lock-comment-face :slant normal)
  '(sp-show-pair-match-face :foreground "#FFFFFF" :background "#FF00FF")
  '(treemacs-git-modified-face :foreground "#9d47ff")
  '(treemacs-git-untracked-face :foreground "red" :weight normal)
  '(show-paren-match :foreground "#FFFFFF" :background "#FF00FF")
  '(show-paren-match-expression :foreground "#FFFFFF" :background "#FF00FF")
  '(idle-highlight :background "#0d6156"))

;; NOTE: `rainbow-mode' uses text properties to highlight.
;; `hl-line-mode' uses an overlay to highlight.
;; Overlay highlighting overrides text-property highlighting.

;; For gruvbox colors: https://github.com/morhetz/gruvbox
(custom-theme-set-faces!
  'doom-gruvbox
  '(hl-line :background "#665c54")
  '(region :background "#665c54") ;; selected
  '(demap-visible-region-face :background "#665c54")
  '(demap-current-line-face :background "yellow")
  '(org-agenda-diary :foreground "yellow")
  '(idle-highlight :background "#8f3f71" :foreground "#fbf1c7"))
  ;; '(idle-highlight :background "#7c6f64" :foreground "#fbf1c7"))

;; For solarized colors: https://en.wikipedia.org/wiki/Solarized
(custom-theme-set-faces!
  'doom-solarized-light
  '(hl-line :background "#ede9ab" :extend t)
  '(region :background "#ede9ab") ;; selected
  '(shadow :foreground "#6c71c4")
  '(demap-visible-region-face :background "#ede9ab")
  '(demap-current-line-face :background "firebrick")
  '(org-agenda-diary :foreground "firebrick")
  '(idle-highlight :background "#edc2ae")
  '(git-gutter-fr:modified :foreground "#3c98e0"))

(defun load-light-theme()
  "Setup colorscheme, hl-line and cursor according to light theme"
  (interactive)
  (setq doom-theme 'doom-solarized-light)
  (load-theme 'doom-solarized-light)
  (setq evil-emacs-state-cursor '("firebrick" box))
  (setq evil-normal-state-cursor '("firebrick" box)))

(defun load-dark-theme()
  "Setup colorscheme, hl-line and cursor according to dark theme"
  (interactive)
  (setq doom-theme 'doom-gruvbox)
  (load-theme 'doom-gruvbox)
  (setq evil-emacs-state-cursor '("white" box))
  (setq evil-normal-state-cursor '("white" box)))

(map! :leader "t L" #'load-light-theme)
(map! :leader "t D" #'load-dark-theme)


(add-hook 'after-init-hook #'global-prettier-mode)

;; Don't create new workspace on new frame
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))


(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)


(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-headerline-breadcrumb-enable t))

;; Treemacs
(after! treemacs
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq treemacs-width 25)
  (setq treemacs-indentation 1)
  (lsp-treemacs-sync-mode 1)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred))

;; TODO: colors for now
(defface custom-line-highlight '((t (:background "#454545" :extend t))) "")

(add-hook
 'treemacs-mode-hook
 (defun change-hl-line-mode ()
   (setq-local hl-line-face 'custom-line-highlight)
   (overlay-put hl-line-overlay 'face hl-line-face)
   (treemacs--setup-icon-background-colors)))



;; Modeline
(require 'doom-modeline)

(doom-modeline-def-segment evil-state
  "The current evil state.  Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (s-trim-right (evil-state-property evil-state :tag t))))

;; NOTE: The modeline looks good only after repeated eval
;; modals
(after! doom-modeline
(doom-modeline-def-modeline 'main
  '(evil-state follow window-number matches buffer-info remote-host buffer-position selection-info)
  '(compilation objed-state misc-info persp-name irc debug minor-modes mu4e github input-method buffer-encoding lsp major-mode process vcs checker battery time "  " bar))
  (setq
    doom-modeline-bar-width 5
    doom-modeline-buffer-file-name-style 'truncate-with-project
    doom-modeline-buffer-encoding nil
    display-time-default-load-average nil ;; FIXME: What does it show ?
    mode-line-in-non-selected-windows nil ;; FIXME: Does not work
    doom-modeline-time-icon nil
    display-time-string-forms '((propertize (concat  24-hours ":" minutes))))
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
  (remove-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline
  (line-number-mode -1)
  (display-battery-mode 1)
  (display-time-mode t)
  (nyan-mode))

;; TODO: spelling history word
; (use-package dashboard
;   :init      ;; tweak dashboard config before loading it
;   (setq dashboard-banner-logo-title nil)
;   (setq dashboard-set-file-icons t)
;   (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
;   (setq dashboard-center-content t)
;   (setq dashboard-set-navigator t)
;   (setq dashboard-items '((recents . 5)
;                           (agenda . 5 )
;                           (bookmarks . 5)
;                           (projects . 5)
;                           (registers . 5)))
;   :config
;   (dashboard-setup-startup-hook)
;   (dashboard-modify-heading-icons '((recents . "file-text")
;                                     (bookmarks . "book"))))

; (setq doom-fallback-buffer-name "*dashboard*")


;; for tsx doesn't work at all
;; ;; Config spell
;; (after! spell-fu
;;   (setq spell-fu-idle-delay 1)  ; default is 0.25

;; (add-hook 'spell-fu-mode-hook
;;   (lambda ()
;;     (spell-fu-dictionary-add
;;       (spell-fu-get-personal-dictionary "en-personal" "./spell/en.utf-8.add"))))

;;   (setq-default spell-fu-word-regexp
;;     (rx
;;      (or
;;       ;; lowercase
;;       (seq
;;        (one-or-more lower)
;;        (opt
;; 	(any "'’")
;; 	(one-or-more lower)
;; 	word-end))
;;       ;; capitalized
;;       (seq
;;        upper
;;        (zero-or-more lower)
;;        (opt
;; 	(any "'’")
;; 	(one-or-more lower)
;; 	word-end))
;;       ;; uppercase
;;       (seq
;;        (one-or-more upper)
;;        (opt
;; 	(any "'’")
;; 	(one-or-more upper)
;; 	word-end)))))

;; (defun cs/spell-fu-check-range (pos-beg pos-end)
;;   (let (case-fold-search)
;;   (spell-fu-check-range-default pos-beg pos-end)))
;; (setq-default spell-fu-check-range #'cs/spell-fu-check-range))
;; (global-spell-fu-mode)

;; NOTE: To make flyspell save personal words:  $ mkdir -p ~/.emacs.d/.local/etc/ispell && touch ~/.emacs.d/.local/etc/ispell/.pws
(after! flyspell
  (setq ispell-dictionary "en")
  (setq flyspell-lazy-idle-seconds 1))
  (setq flyspell-lazy-window-idle-seconds 1)

;; Remove mouse action on misspelled words
(defun make-flyspell-overlay-return-mouse-stuff (overlay)
  (overlay-put overlay 'help-echo nil)
  (overlay-put overlay 'keymap nil)
  (overlay-put overlay 'mouse-face nil))
(advice-add 'make-flyspell-overlay :filter-return #'make-flyspell-overlay-return-mouse-stuff)


(add-hook 'find-file-hook 'flyspell-on-for-buffer-type)

(defun flyspell-on-for-buffer-type ()
  (interactive)
  ;; if flyspell mode is not already on, turn it on
  (if (not (symbol-value flyspell-mode))
      (if (derived-mode-p 'prog-mode)
      ;; (progn
      ;;   (message "Flyspell on (code)")
      ;;   (flyspell-prog-mode))
    (progn
      (message "Flyspell on (text)")
      (flyspell-mode 1)))))

;; NOTE: discarding code from magit doesn't work
(setq magit-diff-refine-hunk 'all)
(use-package! magit-delta
  :after magit
  :config
  (setq
    magit-delta-default-dark-theme "gruvbox-dark"
    magit-delta-default-light-theme "Solarized (light)"
    magit-delta-hide-plus-minus-markers nil)
  (magit-delta-mode +1))

(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 10)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 100
                    :italic t))))
(setq blamer-max-commit-message-length 60)

(map! :leader
  "t B" #'blamer-mode)

(global-set-key (kbd "M-j") #'drag-stuff-down)
(global-set-key (kbd "M-k") #'drag-stuff-up)


(map! :leader
      "g p" #'git-gutter:popup-hunk
      "y" (cmd! (message "Hello world")))

(setq evil-goggles-duration 0.500) ;; default is 0.200
(custom-set-faces
  '(evil-goggles-default-face ((t (:inherit 'highlight :background "orange" :foreground "white"))))) ;; default is to inherit 'region


(after! which-key
  (setq which-key-side-window-max-height 0.5))

;; TODO: As an experiment, because I don't use 't' at all, 'T' now is free
(evil-define-key 'normal 'global (kbd "t") 'avy-goto-char-2)

;; Org mode
(after! org-mode
  (setq org-agenda-files '("~/org")) ;; Must do this so the agenda knows where to look for my files
  (setq org-log-done 'time) ;; When a TODO is set to a done state, record a timestamp
  (setq org-return-follows-link  t)
  ;; FIXME: Hide the markers so you just see bold text as BOLD-TEXT and not *BOLD-TEXT*
  (setq org-hide-emphasis-markers t)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  ;; Remap the change priority keys to use the UP or DOWN key
  (global-set-key (kbd "C-c <up>") #'org-priority-up)
  (global-set-key (kbd "C-c <down>") #'org-priority-down))

(after! git-gutter-fringe
    ;; standardize default fringe width
  (if (fboundp 'fringe-mode) (fringe-mode '(6 . 8)))
    ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
    [#b00111111] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified
    [#b00111111] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [#b00111111] nil nil '(center repeated)))

(setq lsp-enable-symbol-highlighting nil)

(use-package idle-highlight-mode
  :config
    (setq idle-highlight-exceptions-face nil)
    (idle-highlight-global-mode))


(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 1)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:separate
    company-ispell
    company-files
    company-yasnippet))


;; Indent guide
;; NOTE: Performance issues when using builtin doom emacs indent-guides
(setq indent-guide-delay 0.1)
(setq indent-guide-char "|")
(indent-guide-global-mode)

;; (setq idle-highlight-exceptions-face '(font-lock-keyword-face font-lock-string-face font-lock-function-name-face font-lock-variable-name-face font-lock-type-face font-lock-reference-face))
;; NOTE: gr or gR go eval
;; NOTE: SPC u g a to know the current face under cursor
;; NOTE: C-z, switch between emacs vanilla and evil mode
;; NOTE: C-h l or SPC h l. view history
;; NOTE: C-o, jump back after definition. C-i forward
;; NOTE: eval function are under SPC m e
;; NOTE: Windows installation:
;; ONLY using pacman, first install mingw and all related stuff.
;; Be sure that you delete all the mingw from PC so the only left is pacman. Like choco or scoop
;; Install emacs itself using pacman.


;; NOTE: C-h k, 'SPC h k'  method of describing key-binds will also tell you which keymap the key was found in.
;; NOTE: personal dictionary on ~/.emacs.d/.local/etc/ispell/
;; (setq ispell-personal-dictionary "C:/path/to/your/.ispell")
;; C:\Users\mipopovici and on in roaming personal dir location


;; TODO: modeline is going from screen in right part
;; TODO: flyspell warning duplicated

;; TODO: add-hook vs add-hook!
;; TODO: Flycheck spell https://github.com/leotaku/flycheck-aspell
;; TODO: imenu, consult-imenu


;; (global-set-key [8]  'delete-backward-char);;; Ctrl-h = Backspace
;; (blink-cursor-mode 1)

;; NOTE: ctrl-h to backspace
(define-key evil-insert-state-map (kbd "C-h") #'backward-delete-char)
;; (setq ns-auto-hide-menu-bar t)

(global-visual-line-mode)

;; NOTE:  use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
;; Bind to web-mode because by default it binds M-/ to comment-or-uncomment, wtf
(with-eval-after-load 'web-mode
  (define-key web-mode-map (kbd "M-/") 'hippie-expand))



;; Visual bell. Why? audible beeps are annoying. Flash only modeline
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)

(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


;; FIXME: Not sure if it has effect
(require 'ansi-color)
(require 'eshell)

(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
                              eshell-last-output-end))
(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

;; NOTE: flycheck
;;https://www.flycheck.org/en/latest/user/error-reports.html
;;https://github.com/doomemacs/doomemacs/issues/4052
;;https://emacs.stackexchange.com/questions/36363/how-to-change-flycheck-symbol-like-spacemacs

(setq-default flycheck-indication-mode 'right-fringe)

(define-fringe-bitmap 'flycheck-fringe-bitmap-ball
  [#b11111111] nil nil '(center repeated))

(flycheck-define-error-level 'error
  :severity 100
  :compilation-level 2
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-ball
  :fringe-face 'flycheck-fringe-error
  :error-list-face 'flycheck-error-list-error)

(flycheck-define-error-level 'warning
  :severity 10
  :compilation-level 1
  :overlay-category 'flycheck-warning-overlay
  ;; :margin-spec (flycheck-make-margin-spec margin-str 'flycheck-fringe-warning)
  :fringe-bitmap 'flycheck-fringe-bitmap-ball
  :fringe-face 'flycheck-fringe-warning
  :error-list-face 'flycheck-error-list-warning)

(flycheck-define-error-level 'info
  :severity -10
  :compilation-level 0
  :overlay-category 'flycheck-info-overlay
  ;; :margin-spec (flycheck-make-margin-spec margin-str 'flycheck-fringe-info)
  :fringe-bitmap 'flycheck-fringe-bitmap-ball
  :fringe-face 'flycheck-fringe-info
  :error-list-face 'flycheck-error-list-info)


(after! evil
  (setq evil-emacs-state-cursor '("white" box)
    evil-normal-state-cursor '("green" box)
    evil-visual-state-cursor '("orange" box)
    evil-insert-state-cursor '("red" bar)
    ;; use emacs-28 undo system
    evil-undo-system 'undo-redo)

  (setq evil-normal-state-tag   (propertize "[NRM]" 'face '((:background "#98971a" :foreground "#ebdbb2")))
    evil-insert-state-tag   (propertize "[INS]" 'face '((:background "#fb4934") :foreground "#ebdbb2"))
    evil-visual-state-tag   (propertize "[VSL]" 'face '((:background "#d65d0e" :foreground "#1d2021")))
    evil-emacs-state-tag    (propertize "[EMS]" 'face '((:background "white" :foreground "#id2021")))
    evil-motion-state-tag   (propertize "[MTN]" 'face '((:background "blue") :foreground "#ebdbb2"))
    evil-operator-state-tag (propertize "[OPT]" 'face '((:background "purple")))))

(after! minimap
  (setq minimap-disable-mode-line t)
  (setq minimap-update-delay 0.3))

(after! demap
  (map! :leader "t M" #'demap-toggle)
  (add-hook! 'demap-minimap-window-set-hook 'hide-mode-line-mode)
  (setq demap-minimap-window-width 12))


;; Custom
(defun my/copy-absolute-path-name-clipboard ()
  "Copy the absolute current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun my/copy-relative-path-name-clipboard ()
  "Copy the current buffer file name to the clipboard relative to project"
  (interactive)
  (let ((filename (file-relative-name buffer-file-name (projectile-project-root))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


;; Dired
(add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)
(setq dired-listing-switches "-l --all --group-directories-first --human-readable --no-group")

(map! :leader
  (:prefix ("d" . "dired")
  :desc "Open dired" "d" #'dired
  :desc "Dired jump to current" "j" #'dired-jump))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "TAB") 'dired-subtree-toggle
  (kbd "z") 'dired-collapse-mode
  (kbd "b") 'dired-subtree-beginning
  (kbd "e") 'dired-subtree-end
  (kbd "p") 'dired-view-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)

;; NOTE: org mode, tags, priority, checkboxes with [] summary with / and %
;; NOTE: speedbar builtin file tree
;; NOTE: icomplete instead of vertico ???
;; NOTE: zone (zone-when-idle 12)
;; NOTE: Butterfly


;; TODO: Capture to current day and work
;; https://www.reddit.com/r/emacs/comments/7zqc7b/share_your_org_capture_templates/
;; TODO: what is clocking ???

;; FIXME: on startup C-c C-c and other tips are not shown and doesn't work
;; https://github.com/doomemacs/doomemacs/issues/5714
;; https://github.com/org-roam/org-roam/issues/420
 (after! org
  (defadvice! dan/+org--restart-mode-h-careful-restart (fn &rest args)
    :around #'+org--restart-mode-h
    (let ((old-org-capture-current-plist (and (bound-and-true-p org-capture-mode)
                                              (bound-and-true-p org-capture-current-plist))))
      (apply fn args)
      (when old-org-capture-current-plist
        (setq-local org-capture-current-plist old-org-capture-current-plist)
        (org-capture-mode +1)))))

;; NOTE Agenda adds in it every entry with timestamp
(setq org-datetree-add-timestamp t) ;; Add timestamp on every date tree entry, it will be visible in agenda

(setq org-agenda-include-diary t) ;; Include entries from the Emacs diary into Org mode's agenda
(setq diary-file "~/org/diary") ;; Change location of diary

;; TODO: labels
;; (setq org-tag-alist (quote (("scrum"    . ?b)
;;   ("COMP"    . ?c)
;;   ("EMACS"   . ?e)
;;   ("FOOD"    . ?f))))

(setq calendar-mark-diary-entries-flag t)
(setq calendar-mark-holidays-flag t)
(setq calendar-mark-today-flag t)
(setq calendar-mark-ring t)
(setq calendar-week-start-day 1)

;; NOTE: dired with all icons
;; NOTE: Agenda, diary and calendar
;; NOTE: org refile
;; TODO: clocking

;; Set default column view headings: Task Total-Time Time-Stamp
;; (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

;; prOcess manager,
;; NOTE: Only exe are listed
(use-package proced
  :commands proced
  :bind (("C-c C-p" . proced))
  :config
  (setq-default proced-auto-update-flag nil)
  (setq proced-goal-attribute nil
        proced-enable-color-flag t)) ;; Emacs 29


;; TODO: Dirvish vs Dired
