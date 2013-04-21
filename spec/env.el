(require 'ert)

(setq tests-directory (file-name-directory (or load-file-name
                                               (buffer-file-name))))

(add-to-list 'load-path (expand-file-name (concat tests-directory "../vendor/el-spec")))
(add-to-list 'load-path (expand-file-name (concat tests-directory "../vendor/test-double")))
(add-to-list 'load-path (expand-file-name (concat tests-directory "../")))
(add-to-list 'load-path (expand-file-name (concat tests-directory "./")))

(require 'el-spec)
(require 'test-double)
(require 'sandbox)

(load "./spec-helpers")

;; now load all spec files
(dolist (file (directory-files tests-directory t "^[^\.].*spec.el$"))
  (load file))


