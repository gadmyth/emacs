;;; codec.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: codec.el <gadmyth@gmail.com}>
;; Version: 1.1
;; Package-Version: 20211122.001
;; Package-Requires:
;; Keywords: codec, md5, base64, encode, decode
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/codec.el

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
;; codec's code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/codec.el

;;; Commentary:
;;; Code:


(defun url-encode-region (beg end)
  "Url encode the string of region from BEG to END."
  (interactive "r")
  (let ((url-str (delete-and-extract-region beg end)))
    (insert (url-hexify-string url-str))))

(defun url-decode-region (beg end)
  "Url decode the string of region from BEG to END."
  (interactive "r")
  (let ((url-str (delete-and-extract-region beg end)))
    (insert (decode-coding-string (url-unhex-string url-str) 'utf-8))))

(defun md5-encode-region (beg end)
  "MD5 encode the string of region from BEG to END."
  (interactive "r")
  (let ((str (delete-and-extract-region beg end)))
    (insert (md5 str))))

(defun make-base64-url-safe (base64-string)
  "Make BASE64-STRING url safe."
  (let ((str base64-string))
    (setq str (replace-regexp-in-string "\\+" "-" str))
    (setq str (replace-regexp-in-string "/" "_" str))
    (setq str (replace-regexp-in-string "=" "" str))
    str))

(defun make-base64-normal (base64-string)
  "Make BASE64-STRING normal."
  (let* ((str base64-string)
         (remain-len (% (length str) 4))
         (padding-number (if (zerop remain-len)
                             0 (- 4 remain-len))))
    (setq str (replace-regexp-in-string "-" "+" str))
    (setq str (replace-regexp-in-string "_" "/" str))
    (if (> padding-number 0)
        (setq str (concat str (make-string padding-number ?=))))
    str))

(defun base64-encode-string-of-multibyte (string &optional coding-system line-break)
  "Base64 encode the multibyte STRING, LINE-BREAK mean weather the encoded string has link-break."
  (base64-encode-string (encode-coding-string string (or coding-system 'utf-8)) (not line-break)))

(defun base64-decode-string-as-multibyte (string &optional coding-system)
  "Base64 encode the multibyte STRING."
  (decode-coding-string (base64-decode-string string) (or coding-system 'utf-8)))

(defun base64-encode-region-of-multibyte (beg end)
  "Base64 encode multibyte string with region from BEG to END."
  (interactive "r")
  (when (use-region-p)
    (let* ((line-break (yes-or-no-p "Encode string has line break?"))
           (origin (delete-and-extract-region beg end))
           (normal (base64-encode-string-of-multibyte origin line-break))
           (content (if (yes-or-no-p "Encode with safe mode?")
                        (make-base64-url-safe normal) normal)))
      (insert content))))

(defun base64-decode-region-as-multibyte (beg end)
  "Base64 decode the base64 STRING of region from BEG to END, to multibyte string."
  (interactive "r")
  (let* ((url-safe (delete-and-extract-region beg end))
         (normal (make-base64-normal url-safe))
         (content (base64-decode-string-as-multibyte normal)))
    (insert content)))

(defun encode-file-as-base64-uri (file)
  "FILE is the png or jpeg to encode."
  (interactive "ffile: ")
  (let* ((buffer-content (with-temp-buffer (insert-file-contents file) (buffer-string)))
         (base64-content (base64-encode-string buffer-content))
         (uri (concat "data:" "image/png" ";base64," base64-content)))
    (message "encode-file-as-base-64-uri: %s" uri)
    uri))

(provide 'codec)
;;; codec.el ends here
