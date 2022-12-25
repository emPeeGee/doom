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
(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-solarized-dark-high-contrast)
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

(setq doom-themes-treemacs-theme "doom-colors")

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(with-system darwin
  (set-face-attribute 'default nil :font "JetBrains Mono-16"))


(with-system windows-nt
  (set-face-attribute 'default nil :font "JetBrains Mono-11")
  (custom-set-faces
    '(mode-line ((t (:family "JetBrains Mono" :height 0.9))))
    '(mode-line-active ((t (:family  "JetBrains Mono" :height 0.9)))) ; For 29+
    '(mode-line-inactive ((t (:family "JetBrains Mono" :height 0.9))))))



(add-hook 'after-init-hook #'global-prettier-mode)

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)


(setq scroll-margin 10)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(after! treemacs
  ;; (treemacs-follow-mode 1)
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



(setq doom-modeline-buffer-file-name-style 'truncate-with-project)
(setq doom-modeline-buffer-encoding nil)


;; (set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-19")

(custom-set-faces!
  ;; press SPC u g a to know the current face under cursor
  ;; '(hl-line :background "#96B6B4")
  '(font-lock-comment-face :slant normal)
  '(sp-show-pair-match-face :foreground "#FFFFFF" :background "#FF00FF")
  '(treemacs-git-modified-face :foreground "#9d47ff")
  '(show-paren-match :foreground "#FFFFFF" :background "#FF00FF")
  '(show-paren-match-expression :foreground "#FFFFFF" :background "#FF00FF"))
  ;; '(org-ellipsis :foreground "#FFFFFF"))


(setq centaur-tabs-set-bar 'under)
(setq x-underline-at-descent-line t)
(setq centaur-tabs-set-close-button nil)

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

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-center-content t)
  (setq dashboard-set-navigator t)
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))

(setq doom-fallback-buffer-name "*dashboard*")


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
;; baaaad

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
;; new rodr here

;; (add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

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

;; (setq flyspell-mouse-map nil)
;; (setq flyspell-mouse-map (make-sparse-keymap))
;; CamelCase camelCase baaad quickTast quickTest
