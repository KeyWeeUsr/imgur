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

(provide 'imgur-tests)

;;; imgur-tests.el ends here
