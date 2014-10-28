;;; acme-theme.el
;; Author: Semyon Novikov <me@sdfgh153.ru>

(deftheme acme
	"I miss Plan 9 Acme text editor so much")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'acme
   `(cursor ((,class (:background "#00BBFF"))))
   `(border-color ((,class (:background "#87893B"))))
   `(default ((,class (:background "#FFFFE4" :foreground "#2E3436"))))
   `(fringe ((,class (:background "#FFFFE4"))))
   `(mode-line ((,class (:box (:line-width 1 :style 'released-button)
			 		 	 :background "#E5FFFE" :foreground "#7471C0"))))
   `(mode-line-inactive ((,class (:foreground "#747170" :background ,"#E5FFFE"))))
   `(header-line ((,class (:foreground "#E5FFFE" :background "black"))))
   `(minibuffer-prompt ((,class (:foregrond "#0084C8" :bold nil))))
   `(region ((,class (:foreground unspecified :background "#C2D5E9"))))
   `(dired-header ((,class (:bold t :foreground "#0084C8"))))
   `(widget-button ((,class (:bold t :foreground "#0084C8"))))

   `(success ((,class (:bold t :foreground "#4E9A06"))))
   `(warning ((,class (:foreground "#CE5C00"))))
   `(error ((,class (:foreground "#B50000"))))))
