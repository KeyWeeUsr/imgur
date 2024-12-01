;;; imgur-integration-tests.el -- tests for imgur

;;; Code:

(require 'ert)
(require 'imgur)
(setq ert-quiet t)

(defvar imgur--deletehash nil)


(ert-deftest imgur-real-upload-and-delete ()
  "Try against real Imgur API."
  (call-interactively 'imgur-reset)
  (let ((client-id (getenv "IMGUR_CLIENT_ID"))
        (client-secret (getenv "IMGUR_CLIENT_SECRET"))
        done deletehash)
    (when (or (not client-id) (not client-secret))
      (error "Missing credentials IMGUR_CLIENT_ID, IMGUR_CLIENT_SECRET"))
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
                   ,client-id
                   ,client-secret))
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
