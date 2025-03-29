;;; ai.el --- AI integration -*- lexical-binding: t -*-
;;; Commentary:
;;; AI plugins etc.
;;; Code:
(use-package gptel
  :ensure t
  :demand t
  :config
  (defun setup-deep-seek (apikey)
    "Sets up deepseek with the given APIKEY."
    (interactive)
    (setq gptel-model   'deepseek-reasoner
          gptel-backend (gptel-make-deepseek "DeepSeek"
                                             :stream t
                                             :key apikey))))
;;; ai.el ends here
