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

(provide 'imgur-tests)

;;; imgur-tests.el ends here
