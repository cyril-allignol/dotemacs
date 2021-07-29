(defvar old-file-name-handler file-name-handler-alist)
(setq file-name-handler-alist nil
      package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum)
