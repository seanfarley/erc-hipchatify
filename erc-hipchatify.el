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

;;;###autoload
(eval-after-load 'erc
  '(define-erc-module hipchatify nil
     "Show hipchat emoticons and render html"
     ((add-hook 'erc-after-connect 'erc-hipchatify-connect t))
     ((remove-hook 'erc-after-connect 'erc-hipchatify-connect))
     t))

(provide 'erc-hipchatify)
;;; erc-hipchatify.el ends here
