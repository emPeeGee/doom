;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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
(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-solarized-light)

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
(set-face-attribute 'default nil :font "JetBrains Mono-11")
(set-face-attribute 'mode-line nil :font "JetBrains Mono-6")

(setq doom-modeline-height 1) ; optional
(custom-set-faces
  '(mode-line ((t (:family "JetBrains Mono" :height 0.9))))
  '(mode-line-active ((t (:family  "JetBrains Mono" :height 0.9)))) ; For 29+
  '(mode-line-inactive ((t (:family "JetBrains Mono" :height 0.9)))))

;; (set-face-attribute 'mode-line nil :height 80)

;; (setq doom-modeline-height 0.2)
;; (set-face-attribute 'mode-line nil :height 10)
;; (set-face-attribute 'mode-line-inactive nil :height 10)

(add-hook 'after-init-hook #'global-prettier-mode)

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)


(setq scroll-margin 10)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq treemacs-follow-mode t)
(setq treemacs-filewatch-mode t)

(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; Doesn't work
(set-face-background show-paren-match-expression "#FF00FF")
(set-face-background show-paren-match "#FF00FF")
(set-face-background sp-show-pair-match-face "#FF00FF")
(set-face-background rainbow-delimiters-base-error-face "yellow")
(set-face-background hl-line "yellow")

;; (set-face-attribute 'show-paren-match nil :background "yellow")
;; (set-face-attribute 'show-paren-match-expression nil :background "yellow")


;; (add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
;; (add-to-list 'default-frame-alist '(background-color . "#102372"))
