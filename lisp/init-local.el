(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))
(global-set-key (kbd "C-x p") 'select-previous-window)


(global-set-key (kbd "M-v") 'cua-scroll-down)
(global-set-key (kbd "M-o") 'occur)

(global-set-key (kbd "C-x C-g") 'magit-status)
(global-set-key (kbd "C-x .") 'align-regexp)

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-'") 'switch-to-other-buffer)
(global-set-key (kbd "C-|") 'switch-to-other-buffer)


;; Dired
(setq dired-dwim-target t)
;; e no longer does RET, instead does C-c C-x or whatever
(add-hook 'dired-mode-hook
  '(lambda ()
     (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
     (define-key dired-mode-map "/" 'dired-isearch-filenames)))


;; Pretty mode

(require  'pretty-mode)
(add-hook 'ruby-mode 'turn-on-pretty-mode)
(add-hook 'clojure-mode 'turn-on-pretty-mode)

;; TODO/FIXME/BUG
;;
;; Highlight TODOs everywhere

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'esk-add-watchwords)
(defun esk-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))
(add-hook 'find-file-hooks 'esk-prog-mode-hook)


;; Map Modifier-CyrillicLetter to the underlying Modifier-LatinLetter, so that
;; control sequences can be used when keyboard mapping is changed outside of
;; Emacs.
;;
;; For this to work correctly, .emacs must be encoded in the default coding
;; system.
;;
;; Non-empty sub-sets of S
(defun nepowerset (S)
  (let ((x (car S)))
    (if (cdr S)
   (let ((y (nepowerset (remove x S))))
     (append (list (list x))
        (mapcar (lambda (e) (cons x e)) y)
        y))
      (list (list x)))))

(mapcar*
 (lambda (r e) ;; R and E are matching Russian and English keysyms
   ;; iterate over modifier subsets
   (mapc (lambda (mod)
      (define-key input-decode-map
        (vector (append mod (list r))) (vector (append mod (list e)))))
    (nepowerset '(control meta super hyper)))
   ;; finally, if Russian key maps nowhere, remap it to the English key without
   ;; any modifiers
   (define-key local-function-key-map (vector r) (vector e)))
   "йцукенгшщзхъфывапролджэячсмитьбю"
   "qwertyuiop[]asdfghjkl;'zxcvbnm,.")


;; White spaces
(add-hook 'before-save-hook
  '(lambda()
     (save-excursion
       (untabify (point-min) (point-max))
       (delete-trailing-whitespace))))


(require 'elixir-mode)

(provide 'init-local) ;;;
