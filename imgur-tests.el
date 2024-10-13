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

(provide 'imgur-tests)

;;; imgur-tests.el ends here
