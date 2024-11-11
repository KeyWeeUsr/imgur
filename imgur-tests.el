;;; imgur-tests.el -- tests for imgur

;;; Code:

(require 'ert)
(require 'imgur)
(setq ert-quiet t)

(ert-deftest imgur-authorize-missing-or-empty-args ()
  "Check for missing or empty args in authorize func."
  (dolist (args-result
           `(((nil nil nil) ,(format "Bad base (%s)" nil))
             (("" nil nil) ,(format "Bad base (%s)" ""))
             (("base" nil nil) ,(format "Bad client-id (%s)" nil))
             (("base" "" nil) ,(format "Bad client-id (%s)" ""))
             (("base" "id" nil) ,(format "Bad client-secret (%s)" nil))
             (("base" "id" "") ,(format "Bad client-secret (%s)" ""))))
    (condition-case err
        (progn (apply #'imgur-authorize (car args-result)) (should nil))
      (user-error (should (string= (cadr args-result)
                                   (error-message-string err)))))))

(ert-deftest imgur-authorize-cb-on-fail ()
  "Check whether :fail function is called and :success skipped."
  (dolist (args-result
           `(((nil nil nil
                   :fail (lambda (fail-err) (setq result fail-err))
                   :success (lambda () (setq result "bad")))
              ,(format "Bad base (%s)" nil))
             (("" nil nil
               :fail (lambda (fail-err) (setq result fail-err))
               :success (lambda () (setq result "bad")))
              ,(format "Bad base (%s)" ""))
             (("base" nil nil
               :fail (lambda (fail-err) (setq result fail-err))
               :success (lambda () (setq result "bad")))
              ,(format "Bad client-id (%s)" nil))
             (("base" "" nil
               :fail (lambda (fail-err) (setq result fail-err))
               :success (lambda () (setq result "bad")))
              ,(format "Bad client-id (%s)" ""))
             (("base" "id" nil
               :fail (lambda (fail-err) (setq result fail-err))
               :success (lambda () (setq result "bad")))
              ,(format "Bad client-secret (%s)" nil))
             (("base" "id" ""
               :fail (lambda (fail-err) (setq result fail-err))
               :success (lambda () (setq result "bad")))
              ,(format "Bad client-secret (%s)" ""))))
    (let (result server-launched)
      (unwind-protect
          (condition-case err
              (progn
                (advice-add 'make-network-process
                            :override
                            (lambda (&rest r) (setq server-launched t)))
                (apply #'imgur-authorize (car args-result)) (should nil))
            (user-error
             (should (string= (cadr args-result) (error-message-string err)))
             (should (string= (cadr args-result) result))
             (should-not server-launched)
             (should-not imgur-creds)
             (should-not imgur-procs)))
        (setq result nil)
        (advice-remove 'make-network-process
                       (lambda (&rest r) (setq server-launched t)))))))

(ert-deftest imgur-authorize-noninteractive-run-server ()
  "Run server without asking when `noninteractive' and `imgur-creds' is nil."
  (let (imgur-creds imgur-procs (noninteractive t) result called)
    (unwind-protect
        (progn
          (advice-add 'make-network-process
                      :override (lambda (&rest r) (setq called t)))
          (imgur-authorize "base" "id" "secret"
                           :success (lambda () (setq result "abc")))
          (should called)
          (should-not imgur-creds)
          (should-not result))
      (advice-remove 'make-network-process
                     (lambda (&rest r) (setq called t))))))

(ert-deftest imgur-authorize-interactive-run-server ()
  "Ask to open authorization page (and run server) when `imgur-creds' is nil."
  (let (imgur-creds imgur-procs result asked server-launched browser-opened
                    noninteractive)
    (unwind-protect
        (progn
          (advice-add 'read-string :override (lambda (&rest r) (setq asked t)))
          (advice-add 'make-network-process
                      :override (lambda (&rest r) (setq server-launched t)))
          (advice-add 'browse-url
                      :override (lambda (&rest r) (setq browser-opened t)))
          (imgur-authorize "base" "id" (lambda () (setq result "abc")) nil)
          (should (and asked server-launched browser-opened
                       (null imgur-creds) (null result))))
      (advice-remove 'make-network-process
                     (lambda (&rest r) (setq server-launched t)))
      (advice-remove 'browse-url
                     (lambda (&rest r) (setq browser-opened t)))
      (advice-remove 'read-string (lambda (&rest r))))))

(ert-deftest imgur-upload-missing-or-empty-args ()
  "Check for missing or empty args in upload func."
  (dolist (args-result
           `(((nil nil nil nil nil nil nil)
              ,(format "Bad base (%s)" nil))
             (("" nil nil nil nil nil nil)
              ,(format "Bad base (%s)" ""))
             (("base" nil nil nil nil nil nil)
              ,(format "Bad client-id (%s)" nil))
             (("base" "" nil nil nil nil nil)
              ,(format "Bad client-id (%s)" ""))
             (("base" "id" nil nil nil nil nil)
              ,(format "Bad client-secret (%s)" nil))
             (("base" "id" "" nil nil nil nil)
              ,(format "Bad client-secret (%s)" ""))))
    (condition-case err
        (progn (apply #'imgur-upload (car args-result)) (should nil))
      (user-error (should (string= (cadr args-result)
                                   (error-message-string err)))))))

(ert-deftest imgur-upload-deleted-request ()
  "Try deleting right after requesting."
  (call-interactively 'imgur-reset)
  (let* ((host "127.0.0.1")
         (port 8000)
         failed
         (imgur-upload-fail-func (lambda (status resp)
                                   (setq failed `(,status ,resp)))))
    (unwind-protect
        (progn
          (advice-add 'message :override (lambda (&rest _)))
          (setf (alist-get 'base (alist-get 'default imgur-creds))
                (format "http://%s:%s" host port)
                (alist-get 'client-id (alist-get 'default imgur-creds))
                "client-id"
                (alist-get 'client-secret (alist-get 'default imgur-creds))
                "client-secret")
          (should-not failed)
          (apply 'imgur-upload-image-interactive '("tiny.gif" "" ""))
          (dolist (item (process-list))
            (when (string= host (process-name item))
              (delete-process item)))
          (should failed)
          (should (= 2 (length (car failed))))
          (let* ((err (car failed))
                 (resp (cadr failed))
                 (pulled-err (plist-get err :error)))
            (unless pulled-err
              (should-not "Failed retrieving :error"))
            (should (eq 'connection-failed (cadr pulled-err)))
            (let ((err-data (cddr pulled-err)))
              (should (string= "deleted" (string-trim-right (car err-data))))
              (should (string= (plist-get (cdr err-data) :host) host))
              (should (= (plist-get (cdr err-data) :service) port)))))
      (advice-remove 'message (lambda (&rest _))))))

(provide 'imgur-tests)

;;; imgur-tests.el ends here
