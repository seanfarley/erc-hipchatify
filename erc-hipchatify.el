;;; erc-hipchatify.el --- Provide emoticons and html rendering for HipChat

;; Copyright (C) 2015  Sean Farley

;; Author: Sean Farley <sean@farley.io>
;; Version: 0.1
;; Keywords: hipchat multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Show hipchat emoticons and render html (along with images) in erc buffers.
;; Requires Emacs 24.2
;;
;; (require 'erc-hipchatify)
;; (add-to-list 'erc-modules 'hipchatify)
;; (erc-update-modules)
;;
;;
;; Since this plugin wraps `shr-render-region', it benefits from asynchronous
;; downloading.  To rescale images, set `shr-max-image-proportion'.
;;
;; To keep rendered html on one line as much as possible, two shr functions
;; need to be patched, `shr-insert' and `shr-tag-img'. I would like to fix this
;; upstream in the future but patch them here as an experiment.
;;
;;; Code:


(require 'alert) ;; TODO: figure out how to use native erc notifications
(require 'company)
(require 'erc)
(require 'request)
(require 's)
(require 'shr)

(defgroup erc-hipchatify nil
  "Enable hipchatify."
  :group 'erc)

(defcustom erc-hipchatify-token nil
  "The token to which we make api calls, created at
https://atlassian.hipchat.com/account/api"
  :group 'erc-hipchatify
  :type 'string)

(defcustom erc-hipchatify-server "localhost"
  "The name of the HipChat BitlBee server"
  :group 'erc-hipchatify
  :type 'string)

(defvar erc-hipchatify--icons nil
  "Private hash table of HipChat emoticons")

;; Monkey patch shr.el

(defun shr-insert (text)
  (when (and (eq shr-state 'image)
	     (not (bolp))
	     (not (string-match "\\`[ \t\n]+\\'" text)))
    ;; (insert "\n")
    (setq shr-state nil))
  (cond
   ((eq shr-folding-mode 'none)
    (insert text))
   (t
    (when (and (string-match "\\`[ \t\n ]" text)
	       (not (bolp))
	       (not (eq (char-after (1- (point))) ? )))
      (insert " "))
    (dolist (elem (split-string text "[ \f\t\n\r\v ]+" t))
      (when (and (bolp)
		 (> shr-indentation 0))
	(shr-indent))
      ;; No space is needed behind a wide character categorized as
      ;; kinsoku-bol, between characters both categorized as nospace,
      ;; or at the beginning of a line.
      (let (prev)
	(when (and (> (current-column) shr-indentation)
		   (eq (preceding-char) ? )
		   (or (= (line-beginning-position) (1- (point)))
		       (and (shr-char-breakable-p
			     (setq prev (char-after (- (point) 2))))
			    (shr-char-kinsoku-bol-p prev))
		       (and (shr-char-nospace-p prev)
			    (shr-char-nospace-p (aref elem 0)))))
	  (delete-char -1)))
      ;; The shr-start is a special variable that is used to pass
      ;; upwards the first point in the buffer where the text really
      ;; starts.
      (unless shr-start
	(setq shr-start (point)))
      (insert elem)
      (setq shr-state nil)
      (let (found)
	(while (and (> (current-column) shr-width)
		    (> shr-width 0)
		    (progn
		      (setq found (shr-find-fill-point))
		      (not (eolp))))
	  (when (eq (preceding-char) ? )
	    (delete-char -1))
	  (insert "\n")
	  (unless found
	    ;; No space is needed at the beginning of a line.
	    (when (eq (following-char) ? )
	      (delete-char 1)))
	  (when (> shr-indentation 0)
	    (shr-indent))
	  (end-of-line))
	(if (<= (current-column) shr-width)
	    (insert " ")
	  ;; In case we couldn't get a valid break point (because of a
	  ;; word that's longer than `shr-width'), just break anyway.
	  (insert "\n")
	  (when (> shr-indentation 0)
	    (shr-indent)))))
    (unless (string-match "[ \t\r\n ]\\'" text)
      (delete-char -1)))))

(defun shr-tag-img (cont &optional url)
  (when (or url
	    (and cont
		 (> (length (cdr (assq :src cont))) 0)))
    ;; (when (and (> (current-column) 0)
	       ;; (not (eq shr-state 'image)))
      ;; (insert "\n"))
    (let ((alt (cdr (assq :alt cont)))
	  (url (shr-expand-url (or url (cdr (assq :src cont))))))
      (let ((start (point-marker)))
	(when (zerop (length alt))
	  (setq alt "*"))
	(cond
	 ((or (member (cdr (assq :height cont)) '("0" "1"))
	      (member (cdr (assq :width cont)) '("0" "1")))
	  ;; Ignore zero-sized or single-pixel images.
	  )
	 ((and (not shr-inhibit-images)
	       (string-match "\\`data:" url))
	  (let ((image (shr-image-from-data (substring url (match-end 0)))))
	    (if image
		(funcall shr-put-image-function image alt)
	      (insert alt))))
	 ((and (not shr-inhibit-images)
	       (string-match "\\`cid:" url))
	  (let ((url (substring url (match-end 0)))
		image)
	    (if (or (not shr-content-function)
		    (not (setq image (funcall shr-content-function url))))
		(insert alt)
	      (funcall shr-put-image-function image alt))))
	 ((or shr-inhibit-images
	      (and shr-blocked-images
		   (string-match shr-blocked-images url)))
	  (setq shr-start (point))
	  (let ((shr-state 'space))
	    (if (> (string-width alt) 8)
		(shr-insert (truncate-string-to-width alt 8))
	      (shr-insert alt))))
	 ((and (not shr-ignore-cache)
	       (url-is-cached (shr-encode-url url)))
	  (funcall shr-put-image-function (shr-get-image-data url) alt))
	 (t
	  (insert alt " ")
	  (when (and shr-ignore-cache
		     (url-is-cached (shr-encode-url url)))
	    (let ((file (url-cache-create-filename (shr-encode-url url))))
	      (when (file-exists-p file)
		(delete-file file))))
	  (url-queue-retrieve
	   (shr-encode-url url) 'shr-image-fetched
	   (list (current-buffer) start (set-marker (make-marker) (1- (point))))
	   t t)))
	(when (zerop shr-table-depth) ;; We are not in a table.
	  (put-text-property start (point) 'keymap shr-map)
	  (put-text-property start (point) 'shr-alt alt)
	  (put-text-property start (point) 'image-url url)
	  (put-text-property start (point) 'image-displayer
			     (shr-image-displayer shr-content-function))
	  (put-text-property start (point) 'help-echo
			     (or (cdr (assq :title cont))
				 alt)))
	(setq shr-state 'image)))))
;; End monkey patch

(defun erc-hipchatify--process-request (data)
  (let ((startIndex (assoc-default 'startIndex data))
        (maxResults (assoc-default 'maxResults data))
        (nextUrl    (assoc-default 'next (assoc-default 'links data))))
    (mapcar
     (lambda (x)
       (puthash (assoc-default 'shortcut x) (assoc-default 'url x) erc-hipchatify--icons))
     (assoc-default 'items data))
    (message "Finished downloading HipChat emoticons starting from index %d" startIndex)
    (when nextUrl
      (erc-hipchatify--request-icons nextUrl))))

(defun erc-hipchatify--request-icons (&optional url)
  (request
   (or url "https://api.hipchat.com/v2/emoticon")
   :params `(("auth_token" . ,erc-hipchatify-token)
             ("max-results" . "500"))
   :parser 'json-read
   :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                       (message "erc-hipchatify error: %S" error-thrown)))
   :status-code '((500 . (lambda (&rest _) (message "erc-hipchatify got an internal server error (500) from HipChat."))))
   :success (function*
             (lambda (&key data &allow-other-keys)
               (when data
                 (erc-hipchatify--process-request data))))))

(defun erc-hipchatify-connect (server nick)
  (when (and erc-hipchatify-token (string-equal server erc-hipchatify-server))
      (setq erc-hipchatify--icons (make-hash-table :test 'equal))
      (erc-hipchatify--request-icons)))

(defun erc-hipchatify-pre-hook (string)
  "Doesn't display anything from <Link> since it's mostly
garabled html; we'll be rendering most of that stuff ourselves"
  (if (s-starts-with? "<Link>" string)
      (setq erc-insert-this nil)))

(defun erc-hipchatify-notify-here ()
  "Check for '@here' in the message and alert the user if the
window isn't in focus / visible"
  (save-excursion
    ;; use the fact that erc leaves the buffer narrowed so we can extract the
    ;; string, we substract 1 from point-max so we don't get an extra newline
    (let* ((origmsg (buffer-substring-no-properties (point-min) (1- (point-max)))))
      (if (s-starts-with? "<" origmsg)
          ;; now, search for the first "> " which indicates the end of the nickname
          ;; and start of the message (adding two which is the length of "> ")
          (let* ((startPos (+ 2 (s-index-of "> " origmsg)))
                 (newStart (+ (point-min) startPos))
                 (msg (substring origmsg startPos)))
            ;; notify for @here
            ;; TODO: figure out how to use erc notify natively
            (when (and (s-contains? "@here" msg)
                     ;; only alert if not in focus
                     (not (eq (current-buffer) (window-buffer (selected-window)))))
                (alert msg)))))))

(defun erc-hipchatify-render-html ()
  "Modify the buffer to replace (icon) with an html img tag, then
render the whole message. For some text emoticons, such
as (shrug) we just use the actual text-based representation.

Also, skip messages that don't begin with '<' since those are irc
messages."
  (save-excursion
    ;; use the fact that erc leaves the buffer narrowed so we can extract the
    ;; string, we substract 1 from point-max so we don't get an extra newline
    (let* ((origmsg (buffer-substring-no-properties (point-min) (1- (point-max)))))
      (when (s-starts-with? "<" origmsg)
        ;; now, search for the first "> " which indicates the end of the nickname
        ;; and start of the message (adding two which is the length of "> ")
        (let* ((startPos (+ 2 (s-index-of "> " origmsg)))
               (newStart (+ (point-min) startPos))
               (msg (substring origmsg startPos)))
          ;; replace image looking links with an img tag
          (goto-char (1- newStart))
          (while (re-search-forward "[^\"]\\(http[^\s]+\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\)\\)" nil t)
            (replace-match
             (format " <img alt=\"%s\" src=\"%s\"/>"
                     (match-string-no-properties 1)
                     (match-string-no-properties 1))))
          ;; replace hipchat emoticons contained in parentheses
          (when erc-hipchatify--icons
            (goto-char newStart)
            (while (re-search-forward "(\\([a-zA-Z0-9]+\\))" nil t)
              (let* ((hp-shortcut (match-string-no-properties 1))
                     (hp-link (gethash hp-shortcut erc-hipchatify--icons)))
                (cond
                 ((string-equal hp-shortcut "shrug")
                  (replace-match "¯\\\\_(ツ)_/¯"))
                 ((string-equal hp-shortcut "tableflip")
                  (replace-match "(╯°□°）╯︵ ┻━┻"))
                 ((string-equal hp-shortcut "owlflip")
                  (replace-match "(ʘ∇ʘ)ク 彡 ┻━┻"))
                 (hp-link
                  (replace-match
                   (format "<img alt=\"(%s)\" src=\"%s\" />" hp-shortcut hp-link)))))))
          (shr-render-region newStart (1- (point-max)))
          ;; rendering the region adds two lines before and after?
          (goto-char newStart)
          (when (char-equal (following-char) ?\n)
            (delete-char 1)
            (when (char-equal (following-char) ?\n)
              (delete-char 1)))
          ;; go to new point-max
          (goto-char (- (point-max) 3))
          (if (char-equal (following-char) ?\n)
              (delete-char 1)
            (forward-char))
          (when (char-equal (following-char) ?\n)
            (delete-char 1)))))))

(defun erc-hipchatify-icon-company-backend (command &optional arg &rest ignored)
  "A company backend that uses the keys from the icon hash table
and appends ')'"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'erc-hipchatify-icon-company-backend))
    (prefix (and (eq major-mode 'erc-mode)
                 (company-grab-symbol-cons "(" 2))) ;; trigger when typing parenthesis
    (candidates
     (all-completions arg
                      (mapcar
                       (lambda (x) (concat x ")"))
                       (hash-table-keys erc-hipchatify--icons))))))

(defun erc-hipchatify-nick-company-backend (command &optional arg &rest ignored)
  "A company backend that triggers nick completion with '@'"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'erc-hipchatify-icon-company-backend))
    (prefix (and (eq major-mode 'erc-mode)
                 (company-grab-symbol-cons "@" 2))) ;; trigger when typing parenthesis
    (candidates
     (all-completions arg (smf/user-keys erc-channel-users)))))

(defun erc-hipchatify-mode-hook ()
  "Turn on company mode and register our backend"
  (add-to-list 'company-backends 'erc-hipchatify-icon-company-backend)
  (add-to-list 'company-backends 'erc-hipchatify-nick-company-backend)
  (company-mode-on))

(defun erc-cmd-ANIM (&rest msg)
  (when msg
      (erc-send-message (concat "/anim " (mapconcat 'identity msg " ")))))

(defun erc-cmd-GIF (&rest msg)
  (when msg
      (erc-send-message (concat "/gif " (mapconcat 'identity msg " ")))))

(defun erc-cmd-GIPHY (&rest msg)
  (when msg
      (erc-send-message (concat "/giphy " (mapconcat 'identity msg " ")))))

(defun erc-cmd-IMG (&rest msg)
  (when msg
      (erc-send-message (concat "/img " (mapconcat 'identity msg " ")))))

(defun erc-cmd-MEME (&rest msg)
  (when msg
      (erc-send-message (concat "/meme " (mapconcat 'identity msg " ")))))

(defun erc-cmd-CODE (&rest msg)
  (when msg
      (erc-send-message (concat "/code " (mapconcat 'identity msg " ")))))

(defun erc-cmd-QUOTE (&rest msg)
  (when msg
      (erc-send-message (concat "/quote " (mapconcat 'identity msg " ")))))

;;;###autoload
(eval-after-load 'erc
  '(define-erc-module hipchatify nil
     "Show hipchat emoticons and render html"
     ((add-hook 'erc-after-connect 'erc-hipchatify-connect t)
      (add-hook 'erc-insert-pre-hook 'erc-hipchatify-pre-hook)
      (add-hook 'erc-insert-modify-hook 'erc-hipchatify-notify-here)
      (add-hook 'erc-insert-modify-hook 'erc-hipchatify-render-html)
      (add-hook 'erc-send-modify-hook 'erc-hipchatify-render-html)
      (add-hook 'erc-mode-hook 'erc-hipchatify-mode-hook))
     ((remove-hook 'erc-after-connect 'erc-hipchatify-connect)
      (remove-hook 'erc-insert-pre-hook 'erc-hipchatify-pre-hook)
      (remove-hook 'erc-insert-modify-hook 'erc-hipchatify-notify-here)
      (remove-hook 'erc-insert-modify-hook 'erc-hipchatify-render-html)
      (remove-hook 'erc-send-modify-hook 'erc-hipchatify-render-html)
      (remove-hook 'erc-mode-hook 'erc-hipchatify-mode-hook))
     t))

(provide 'erc-hipchatify)
;;; erc-hipchatify.el ends here
