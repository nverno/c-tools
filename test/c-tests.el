(require 'c-tools)
(require 'ert)

(defun c--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "c--test")
    (message "cant run without ert.")))

(provide 'c-tests)
