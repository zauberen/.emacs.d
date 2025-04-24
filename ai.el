;;; ai.el --- AI integration -*- lexical-binding: t -*-
;;; Commentary:
;;; AI plugins etc.
;;; Code:
(use-package gptel
  :ensure t
  :demand t
  :bind (("C-c a i" . gptel)
         ("C-c a a" . gptel-add)
         ("C-c a f" . gptel-add-file)
         :map gptel-mode-map
         ("S-<return>" . gptel-send))
  :config
  (defun set-default-chat (model-name setup-func)
    "Set the default model, given a MODEL-NAME and the output of the SETUP-FUNC."
    (setq gptel-model model-name
          gptel-backend setup-func))
  ;; In local.el, call the function with an api key to use DeepSeek.
  ;; Here's an example:
  ;; (use-package gptel
  ;;   :ensure nil
  ;;   :config
  ;;   (setup-deep-seek "key"))
  (defun setup-deep-seek (apikey)
    "Sets up deepseek with the given APIKEY."
    (gptel-make-deepseek "DeepSeek"
      :stream t
      :key apikey))
  (defun setup-ollama (model-list)
    "Sets up ollama with the given
MODEL-LIST like \='(deepseek-r1:8b deepseek-coder-v2:16b)."
    (gptel-make-ollama "Ollama"
      :host "localhost:11434"
      :stream t
      :models model-list)))

(use-package copilot-chat
  :ensure t
  :bind ("C-c a c" . copilot-chat-display)
  :config
  ; Fix doom-modeline
  (defun copilot-chat--org-goto-input()
    "Go to the input part of the chat buffer.
The input is created if not found."
    (goto-char (point-max))
    (let ((span (pm-innermost-span (point))))
      (if (and span
               (not (eq (car span) nil)))  ; nil span-type means host mode
          (goto-char (+ 1 (car (pm-innermost-range (point)))))
        (insert "\n\n")
        (let ((start (point))
              (inhibit-read-only t))
          (insert copilot-chat--org-delimiter "\n\n"))))))
;;; ai.el ends here
