(defpackage :openai-utils
  (:use :cl :utils)
  (:export #:execute #:get-usage #:deepseek-balance #:openai-usage #:mistral-account #:format-usage-tokens #:print-usage-tokens))

(in-package :openai-utils)

;;; --- the tool call, OpenAI-compatible shape -----------------------------
;;; ChatGPT, DeepSeek and Mistral all speak the same tool-calling dialect:
;;; each tool_call carries its name and (JSON-encoded) arguments nested
;;; under "function", and the result is fed back as a "tool" message keyed
;;; by the call's id. Only the progress log line ever differed between the
;;; three agents, so the arrow is shared too.

(defun execute (tool-call)
  "Turn one tool_call from the model into a tool-result message."
  (let* ((name (ref tool-call "function" "name"))
         (args (shasht:read-json (ref tool-call "function" "arguments")))
         (result (run-lisp-eval-tool name args)))
    (format t "~&  ⤷ ~a => ~a~%" (gethash "form" args) result)
    (obj "role" "tool"
         "tool_call_id" (gethash "id" tool-call)
         "content" result)))

;;; --- account usage & balance ------------------------------------------
;;; What an account has spent is not one API across these three providers:
;;; DeepSeek exposes a balance, OpenAI exposes per-day token usage, and
;;; Mistral exposes neither (only who the key belongs to). GET-USAGE keeps
;;; that split in one place, querying each provider's own endpoint and
;;; printing whatever it can answer.

(defconstant +usage-separator+
  "___________________________________________________________________________")

(defun today-string ()
  "Today as an ISO \"YYYY-MM-DD\" date, what OpenAI's usage endpoint wants."
  (multiple-value-bind (s m h day month year) (get-decoded-time)
    (declare (ignore s m h))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun usage-get (url api-key &key query)
  "GET URL with a bearer API-KEY (a query string appended when given), and
return the parsed JSON. Kept here rather than in http-utils because it is
the one call these usage endpoints have in common."
  (shasht:read-json
   (dex:get (if query (format nil "~a?~a" url query) url)
            :headers `(("Authorization" . ,(format nil "Bearer ~a" api-key))))))

;;; DeepSeek: one balance endpoint, no dates.

(defun deepseek-balance (api-key)
  "DeepSeek's /user/balance payload: IS_AVAILABLE plus BALANCE_INFOS."
  (usage-get "https://api.deepseek.com/user/balance" api-key))

(defun print-deepseek-usage (balance)
  (format t "~&Balance:~%")
  (loop for info across (gethash "balance_infos" balance)
        do (format t "  ~a ~a total (~a granted, ~a topped up)~%"
                   (gethash "total_balance" info)
                   (gethash "currency" info)
                   (gethash "granted_balance" info)
                   (gethash "topped_up_balance" info)))
  (format t "  available: ~a~%" (gethash "is_available" balance)))

;;; OpenAI: per-day token usage. /v1/organization/costs would report money
;;; instead, but it needs an admin-scoped key (api.usage.read) a normal key
;;; lacks, so the plain /v1/usage is what we can actually read.

(defun openai-usage (api-key &key (date (today-string)))
  "OpenAI's /v1/usage for DATE, a list of per-model token buckets."
  (gethash "data" (usage-get "https://api.openai.com/v1/usage" api-key
                              :query (format nil "date=~a" date))))

(defun print-openai-usage (data &key (date (today-string)))
  (if (and data (plusp (length data)))
      (let ((input 0) (output 0))
        (format t "~&Usage for ~a:~%" date)
        (loop for bucket across data
              do (format t "  ~a: ~a in, ~a out~%"
                         (gethash "snapshot_id" bucket)
                         (gethash "n_context_tokens_total" bucket)
                         (gethash "n_generated_tokens_total" bucket))
                 (incf input  (or (gethash "n_context_tokens_total" bucket) 0))
                 (incf output (or (gethash "n_generated_tokens_total" bucket) 0)))
        (format t "  total: ~a in, ~a out~%" input output))
      (format t "~&Usage: none recorded for ~a.~%" date)))

;;; Mistral: no balance or usage endpoint exists for an API key, so the
;;; best available is who the key belongs to.

(defun mistral-account (api-key)
  "Mistral's /v1/users/me payload: account, workspace and organization."
  (usage-get "https://api.mistral.ai/v1/users/me" api-key))

(defun print-mistral-usage (account)
  ;; shasht parses a JSON null as the truthy keyword :NULL, so a missing
  ;; last name has to be filtered out rather than trusted to ~@[.
  (flet ((real (value) (and value (not (eq value :null)) value)))
    (let ((workspace (gethash "workspace" account))
          (org (gethash "organization" account)))
      (format t "~&Account: ~a~@[ ~a~] <~a>~%"
              (gethash "first_name" account)
              (real (gethash "last_name" account))
              (gethash "email" account))
      (when workspace (format t "  workspace: ~a~%" (gethash "name" workspace)))
      (when org (format t "  organization: ~a~%" (gethash "name" org)))
      (format t "  (Mistral exposes no balance or usage over the API.)~%"))))

(defun get-usage (provider api-key &key date)
  "Report what PROVIDER -- :DEEPSEEK, :OPENAI or :MISTRAL -- says about the
account API-KEY belongs to: balance, token usage or identity, whichever
that provider exposes. DATE, an ISO \"YYYY-MM-DD\" string, picks the day for
OpenAI's per-day usage (today by default). Prints a short report and
returns the raw parsed reply, so a caller can also use it in code."
  (let ((reply (ecase provider
                 (:deepseek (deepseek-balance api-key))
                 (:openai   (openai-usage api-key :date (or date (today-string))))
                 (:mistral  (mistral-account api-key)))))
    (format t "~&~a~%" +usage-separator+)
    (ecase provider
      (:deepseek (print-deepseek-usage reply))
      (:openai   (print-openai-usage reply :date (or date (today-string))))
      (:mistral  (print-mistral-usage reply)))
    (format t "~a~%" +usage-separator+)
    reply))


;;; --- per-call token counts -------------------------------------------
;;; The account endpoints above answer what an account has spent; the model
;;; responses answer what one call cost, and that is the same shape on all
;;; three providers: prompt/completion/total tokens, with the cache and
;;; reasoning sub-counts present only when the model reports them. Each
;;; agent keeps the last response's usage in *LAST-USAGE* and prints it here.

(defun usage-token-counts (usage)
  "USAGE's token fields as a plist of :INPUT, :OUTPUT, :TOTAL, :CACHED and
:REASONING, any of which is NIL when the provider did not report it. The
OpenAI-compatible spelling is prompt_tokens/completion_tokens/total_tokens,
with the cache and reasoning counts nested under the *_details objects."
  (when usage
    (let ((prompt-details (gethash "prompt_tokens_details" usage))
          (completion-details (gethash "completion_tokens_details" usage)))
      (list :input (gethash "prompt_tokens" usage)
            :output (gethash "completion_tokens" usage)
            :total (gethash "total_tokens" usage)
            :cached (and prompt-details (gethash "cached_tokens" prompt-details))
            :reasoning (and completion-details
                            (gethash "reasoning_tokens" completion-details))))))

(defun format-usage-tokens (usage)
  "USAGE as one readable line, or NIL when there is nothing to show. A
count the provider left out is not printed rather than shown as zero, so
the line never implies a figure the reply did not carry."
  (let ((counts (usage-token-counts usage)))
    (when counts
      (destructuring-bind (&key input output total cached reasoning) counts
        (with-output-to-string (s)
          (write-string "Tokens:" s)
          (when input  (format s " ~a in" input))
          (when output (format s "~:[, ~; ~]~a out" (not input) output))
          (when (and total (/= total (or output 0))) (format s " (~a total)" total))
          (when (and cached (plusp cached)) (format s ", ~a cached" cached))
          (when (and reasoning (plusp reasoning)) (format s ", ~a reasoning" reasoning)))))))

(defun print-usage-tokens (usage)
  "Print FORMAT-USAGE-TOKENS of USAGE, silenced when there is nothing to say."
  (let ((line (format-usage-tokens usage)))
    (when line (format t "~&~a~%" line))))

