(defpackage :http-utils
  (:use :cl)
  (:export #:http-post-json #:http-get-json))

(in-package :http-utils)

;;; --- thin JSON-over-HTTP helpers ---------------------------------------
;;; Every agent's CALL-MODEL and LIST-MODELS do the same three things:
;;; build a JSON body, POST/GET it with DEXADOR, and parse the JSON reply.
;;; Only the URL, the headers and the body differ per provider, so those
;;; stay in each agent; the plumbing lives here.

(defun http-post-json (url headers body &key read-timeout)
  "POST BODY as JSON to URL with HTTP HEADERS; return the parsed JSON response.
READ-TIMEOUT, when given, is passed through to DEXADOR."
  (shasht:read-json
   (apply #'dex:post url
          :headers headers
          :content (shasht:write-json body nil)
          (when read-timeout (list :read-timeout read-timeout)))))

(defun http-get-json (url &key headers)
  "GET URL with HTTP HEADERS; return the parsed JSON response."
  (shasht:read-json
   (dex:get url :headers headers)))
