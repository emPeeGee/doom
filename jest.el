;;; learn.el -*- lexical-binding: t; -*-


(progn
  (switch-to-buffer-other-window "*elisp-output*")
  (erase-buffer) ;; This is a dangerous function, use it with care.

  ;; Now do whatever you want.

  (insert "Hello, world!\n")
  (insert (format "123 + 546 = %S\n" (+ 123 456)))

  ;; Prompt the user for input:
  (let ((name (read-from-minibuffer "What is your name? ")))
    (insert (format "Hello, %s, nice to meet you.\n" name)))

  ;; Build data structures, prompt the user:
  ;; (let*((colors
  ;;        (apply #'vector
  ;;               (split-string
  ;;                "red orange yellow green blue indigo violet" " ")))

  ;;       (num (read-minibuffer "Type any number: "))

  ;;       (lucky (elt colors (% num (length colors)))))

  ;;   (insert (format "Your lucky color of the day is %s.\n" lucky))
  ;;   (insert (format "Your lucky number today is %i.\n\n" (1+ (random 100))))
  ;;   (insert "-------------\n")
  ;;   (insert "Other colors could have been:\n")
  ;;   (mapc ; This is like a "foreach" loop
  ;;    (lambda (color)
  ;;      (insert (format "%s\n" color)))
  ;;    colors))

  ;; Programatically find and replace text
  (search-backward "Hello")
  (replace-match "Goodbye")
  (search-forward "nice to meet")
  (replace-match "it was nice meeting")

  ;; Apply colors and styles (i.e. "faces") to text
  (let ((line-start (move-beginning-of-line 1))
        (line-end (move-end-of-line 1)))
    (set-text-properties
     line-start line-end
     '(face (bold :foreground "blue")))))
