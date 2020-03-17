;;; codec.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: codec.el <gadmyth@gmail.com}>
;; Version: 1.0
;; Package-Version: 20200317.001
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
  (interactive)
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

(defun base64-encode-string-of-multibyte (string)
  "Base64 encode the multibyte STRING."
  (base64-encode-string (encode-coding-string string 'utf-8)))

(defun base64-encode-region-of-multibyte (beg end)
  "Base64 encode multibyte string with region from BEG to END."
  (interactive "r")
  (let ((content (delete-and-extract-region beg end)))
    (insert (base64-encode-string-of-multibyte content))))

(defun base64-decode-string-as-multibyte (string)
  "Base64 encode the multibyte STRING."
  (decode-coding-string (base64-decode-string string) 'utf-8))

(defun base64-decode-region-as-multibyte (beg end)
  "Base64 decode the base64 STRING of region from BEG to END, to multibyte string."
  (interactive "r")
  (let ((content (delete-and-extract-region beg end)))
    (insert (base64-decode-string-as-multibyte content))))

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
