;;; weathers.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: weathers.el <gadmyth@gmail.com>
;; Version: 1.0.8
;; Package-Version: 20240812.001
;; Package-Requires: request, hmac-sha1, dates
;; Keywords: weathers.el
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; weathers's code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/

;;; Commentary:
;;; Code:

(require 'request)
(require 'dates)
(require 'hmac-sha1)
(require 'q)

(defconst +weather-api-url+ "http://api.seniverse.com/v3/weather/now.json")

(defcustom *weather-api-uid* nil
  "The seniverse api's public key.")

(defcustom *weather-api-key* nil
  "The seniverse api's private key.")

(defcustom *weather-location* nil
  "The location for weather api query, for example: shanghai, beijin.")

(defvar *weather-api-result* ""
  "The formated short weather api result, for example: shanghai: Sun(29).")

(defvar *weather-fetch-timer* nil)

(defcustom weather-fetched-hook nil
  "Hook when weather fetch finished."
  :type 'hook
  :group 'weather)

(define-debug-message weathers)

(defun refresh-weather ()
  "."
  (interactive)
  (message "*** now refresh-weather: %s ***" (current-time-normal-string))
  (when (> (length *weather-api-result*) 0)
    (weathers-debug-message "** *weather-api-result* is: [%S]" *weather-api-result*)
    (setq *weather-api-result* "")
    (weathers-debug-message "*weather-api-result* set as: [%S]" *weather-api-result*))
  (weathers-fetcher-weather))

(defun weathers-fetcher-weather ()
  "."
  (interactive)
  (when (and *weather-api-uid*
             *weather-api-key*
             *weather-location*)
    (let* ((api +weather-api-url+)
           (uid *weather-api-uid*)
           (key *weather-api-key*)
           (location *weather-location*)
           (ts (current-timestamp))
           (str (format "ts=%S&uid=%s" ts uid))
           (signature (base64-encode-string
                       (hmac-sha1
                        (encode-coding-string key 'utf-8 t)
                        (encode-coding-string str 'utf-8 t)))))
      (let ((request-backend 'curl))
        (request
          api
          :params `((ts . ,ts)
                    (uid . ,uid)
                    (sig . ,signature)
                    (location . ,location))
          :parser 'json-read
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (when (string-equal "results" (car (car data)))
                        (let* ((results (cdr (assoc 'results data)))
                               (result (and (vectorp results)
                                            (> (length results) 0)
                                            (aref results 0)))
                               (location (assoc 'location result))
                               (loc-name (cdr (assoc 'name (cdr location))))
                               (now (assoc 'now result))
                               (weather (cdr (assoc 'text (cdr now))))
                               (temperature (cdr (assoc 'temperature (cdr now))))
                               (format-result (format "%s:%s(%s)" loc-name weather temperature)))
                          (weathers-debug-message "weather result fetched: %s" format-result)
                          (setq *weather-api-result* format-result)
                          (run-hooks 'weather-fetched-hook)))))
          :error (cl-function
                  (lambda (&key error-thrown &allow-other-keys&rest _)
                    (message "error: %S" error-thrown))))))))

(defun fetched-weather ()
  "."
  (when (or (null *weather-api-result*)
            (zerop (length *weather-api-result*)))
    (when (not *weather-fetch-timer*)
      (refresh-weather)
      (setq *weather-fetch-timer*
            (run-with-idle-timer 300 300 #'refresh-weather))))
  *weather-api-result*)

(defun reset-weather-fetch-timer ()
  "."
  (interactive)
  (when *weather-fetch-timer*
    (cancel-timer *weather-fetch-timer*)
    (weathers-debug-message "*weather-fetch-timer* cancelled")
    (setq *weather-api-result* "")
    (weathers-debug-message "*weather-api-result* set to [%S]" *weather-api-result*)
    (setq *weather-fetch-timer* nil)
    (weathers-debug-message "*weather-fetch-timer* set to [%S]" *weather-fetch-timer*)
    ))

(provide 'weathers)
;;; weathers.el ends here
