;;; imgur-integration-tests.el -- tests for imgur

;;; Code:

(require 'ert)
(require 'imgur)
(setq ert-quiet t)

(defvar imgur--deletehash nil)
(defun imgur--parse-upload (status response)
  "Parse deletehash from upload response."
  (ignore status)
  )

(ert-deftest imgur-real-upload-and-delete ()
  "Try against real Imgur API."
  (call-interactively 'imgur-reset)
  (let (done deletehash)
    (unwind-protect
        (progn
          (advice-add
           'imgur-upload-default-success-func
           :after
           (lambda (_ response)
             (setq deletehash
                   (alist-get 'deletehash
                              (alist-get 'data
                                         (imgur-response-body response))))
             (setq done t)))
          (apply #'imgur-authorize-interactive
                 `("https://api.imgur.com"
                   ,(getenv "IMGUR_CLIENT_ID")
                   ,(getenv "IMGUR_CLIENT_SECRET")))
          (message "Test: Authorizing")
          (while (null imgur-creds)
            (message "...still authorizing")
            (sleep-for 2))
          (message "Test: Uploading")
          (apply #'imgur-upload-image-interactive
                 '("tiny.gif" "My title" "My description"))
          (while (null done)
            (message "...still uploading")
            (sleep-for 2)))
      (advice-remove
       'imgur-upload-default-success-func
       (lambda (_ response)
         (setq deletehash
               (alist-get 'deletehash
                          (alist-get 'data
                                     (imgur-response-body response))))
         (setq done t))))
    (setq done nil)
    (unwind-protect
        (progn
          (advice-add 'imgur-delete-default-success-func
                      :after
                      (lambda (&rest _) (setq done t)))
          (message "Test: Deleting")
          (apply #'imgur-delete-interactive `(,deletehash))
          (while (null done)
            (message "...still deleting")
            (sleep-for 2)))
      (advice-remove 'imgur-delete-default-success-func
                     (lambda (&rest _) (setq done t))))))

(provide 'imgur-integration-tests)

;;; imgur-integration-tests.el ends here
