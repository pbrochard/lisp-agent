(defpackage :http-utils
  (:use :cl)
  (:export #:http-post-json #:http-get-json #:proxy-url))

(in-package :http-utils)

;;; --- keyproxy -------------------------------------------------------------
;;; No agent holds a real provider API key: EVAL is a tool handed to an
;;; unsupervised model, so anything in this process's environment is
;;; something that model can read and try to exfiltrate. Every provider call
;;; instead goes to the keyproxy sidecar, which holds the real keys, injects
;;; the right one for the one upstream host it forwards to, and refuses
;;; everything else. See run.sh and keyproxy/.

(defparameter *keyproxy-url* (uiop:getenv "KEYPROXY_URL")
  "Base URL of the keyproxy sidecar, e.g. \"http://keyproxy:8080\".")

(defun proxy-url (path)
  "PATH (e.g. \"anthropic/v1/messages\") resolved against *KEYPROXY-URL*."
  (unless *keyproxy-url*
    (error "KEYPROXY_URL is not set -- agents talk to providers through keyproxy, never directly."))
  (format nil "~a/~a" *keyproxy-url* path))

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
