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
(setq doom-theme 'doom-solarized-dark-high-contrast)
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

(setq display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; I don't use daemon on mac
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :font "JetBrains Mono-16"))

;; https://github.com/termitereform/JunkPile/blob/master/emacs-on-windows.md#creating-a-safe-start-shortcut
;; https://emacs.stackexchange.com/questions/46541/running-emacs-as-a-daemon-does-not-load-custom-set-faces
(defun windows-face()
  "Setup specific windows settings"
  (when (eq system-type 'windows-nt)
    (message "Setting windows")
    (setq-default ispell-program-name "C:/msys64/ucrt64/bin/aspell.exe")
    (setq ispell-extra-args '("--encoding=utf-8" "--sug-mode=ultra" "--lang=en" "--run-together" "--camel-case"))
    (set-face-attribute 'default nil :font "JetBrains Mono-12")
    (setq all-the-icons-scale-factor 1.0)
    (custom-set-faces  ;; TODO: font family
      '(mode-line ((t (:family "Noto Sans" :height 0.9))))
      '(mode-line-active ((t (:family "Noto Sans" :height 0.9)))) ; For 29+
      '(mode-line-inactive ((t ( :family "Noto Sans" :height 0.9))))))
)

(defun my-frame-tweaks (&optional frame)
  "My personal frame tweaks."
  (unless frame
    (setq frame (selected-frame)))
  (when frame
    (with-selected-frame frame
      (when (display-graphic-p)
        (tool-bar-mode -1))))
    (windows-face)
)


;; For the case that the init file runs after the frame has been created. Call of emacs without --daemon option.
(my-frame-tweaks)
;; For the case that the init file runs before the frame is created. Call of emacs with --daemon option.
(add-hook 'after-make-frame-functions #'my-frame-tweaks t)

(add-hook! org-mode 'rainbow-mode)
(add-hook! prog-mode 'rainbow-mode)

(custom-set-faces!
  `(org-level-4 :inherit outline-4 :extend t :height 1.1)
  `(org-level-3 :inherit outline-3 :extend t :height 1.2)
  `(org-level-2 :inherit outline-2 :extend t :height 1.3)
  `(org-level-1 :inherit outline-1 :extend t :height 1.4)
  '(hl-line :background "#293b52" :extend t)
  '(region :background "#3e4e63") ;; selected
  '(flyspell-incorrect :underline (:color "#FF00FF" :style wave))
  '(flyspell-duplicate :underline (:color "#9400D3" :style wave))
  '(font-lock-comment-face :slant normal)
  '(sp-show-pair-match-face :foreground "#FFFFFF" :background "#FF00FF")
  '(treemacs-git-modified-face :foreground "#9d47ff")
  '(show-paren-match :foreground "#FFFFFF" :background "#FF00FF")
  '(show-paren-match-expression :foreground "#FFFFFF" :background "#FF00FF"))

(custom-theme-set-faces!
 'doom-solarized-light
  '(hl-line :background "#d9d266" :extend t)
  '(flyspell-incorrect :underline (:color "green" :style wave))
  '(flyspell-duplicate :underline (:color "green" :style wave)))

(defun load-light-theme()
  "Setup colorscheme, hl-line and cursor according to light theme"
  (interactive)
  (setq doom-theme 'doom-solarized-light)
  (load-theme 'doom-solarized-light)
  (setq evil-emacs-state-cursor '("firebrick" box))
  (setq evil-normal-state-cursor '("firebrick" box))
)

(defun load-dark-theme()
  "Setup colorscheme, hl-line and cursor according to dark theme"
  (interactive)
  (setq doom-theme 'doom-solarized-dark-high-contrast)
  (load-theme 'doom-solarized-dark-high-contrast)
  (setq evil-emacs-state-cursor '("#3c98e0" box))
  (setq evil-normal-state-cursor '("#3c98e0" box))
)

(map! :leader "t L" #'load-light-theme)
(map! :leader "t D" #'load-dark-theme)


(add-hook 'after-init-hook #'global-prettier-mode)

;; Don't create new workspace on new frame
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))


(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)


(setq scroll-margin 10)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Treemacs
(after! treemacs
  (setq doom-themes-treemacs-theme "doom-colors")
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred))

;; TODO: colors for now
(defface custom-line-highlight '((t (:background "#616161" :foreground "#9CCC65" :extend t))) "")

(add-hook
 'treemacs-mode-hook
 (defun channge-hl-line-mode ()
   (setq-local hl-line-face 'custom-line-highlight)
   (overlay-put hl-line-overlay 'face hl-line-face)
   (treemacs--setup-icon-background-colors)))


;; Modeline
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)
(setq doom-modeline-buffer-encoding nil)
(display-battery-mode 1)
(display-time-mode t)
(setq display-time-default-load-average nil) ;; FIXME: What does it show ?
(setq mode-line-in-non-selected-windows nil) ;; FIXME: Does not work

;; TODO: spelling history word

;; (setq centaur-tabs-set-bar 'under)
;; (setq x-underline-at-descent-line t)
;; (setq centaur-tabs-set-close-button nil)

(use-package centaur-tabs
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  )

(map! :leader
  (:prefix ("e". "evaluate/ERC/EWW")
   :desc "Evaluate elisp in buffer" "b" #'eval-buffer
   :desc "Evaluate defun" "d" #'eval-defun
   :desc "Evaluate elisp expression" "e" #'eval-expression
   :desc "Evaluate last sexpression" "l" #'eval-last-sexp
   :desc "Evaluate elisp in region" "r" #'eval-region))

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



;; ;; NOTE: discarding code from magit doesn't work
;; (setq magit-diff-refine-hunk 'all)
;; (use-package! magit-delta
;;   :after magit
;;   :config
;;   (setq
;;     magit-delta-default-dark-theme "OneHalfDark"
;;     magit-delta-default-light-theme "Github"
;;     magit-delta-hide-plus-minus-markers nil)
;;   (magit-delta-mode))

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

;; enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)

(after! which-key
  (setq which-key-side-window-max-height 0.5))

;; TODO: As an experiment, because I don't use 't' at all, 'T' now is free
(evil-define-key 'normal 'global (kbd "t") 'avy-goto-char-2)

;; Org mode
(setq org-agenda-files '("~/org")) ;; Must do this so the agenda knows where to look for my files
(setq org-log-done 'time) ;; When a TODO is set to a done state, record a timestamp
(setq org-return-follows-link  t)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Remap the change priority keys to use the UP or DOWN key
(global-set-key (kbd "C-c <up>") #'org-priority-up)
(global-set-key (kbd "C-c <down>") #'org-priority-down)

;; FIXME: Hide the markers so you just see bold text as BOLD-TEXT and not *BOLD-TEXT*
(setq org-hide-emphasis-markers t)


;; NOTE: SPC u g a to know the current face under cursor
;; NOTE: C-z, switch between emacs vanilla and evil mode
;; NOTE: C-h l or SPC h l. view history
;; NOTE: C-o, jump back after definition. C-i forward
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
