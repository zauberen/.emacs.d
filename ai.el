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
         ("C-c a m" . gptel-menu)
         :map gptel-mode-map
         ("S-<return>" . gptel-send))
  :hook (gptel-post-stream . gptel-auto-scroll)
  :custom
  ; Write prompt buffers and responses in org syntax.
  (gptel-default-mode 'org-mode)
  ; Allow use and reference of media files.
  (gptel-track-media t)
  ; Do not show thinking in the buffer.
  (gptel-include-reasoning nil)
  ; Set the default prompt prefixes
  (gptel-prompt-prefix-alist   '((markdown-mode . "### ")
                                 (org-mode . "* ")
                                 (text-mode . "# ")))
  (gptel-response-prefix-alist '((markdown-mode . "")
                                 (org-mode . "** Response:\n")
                                 (text-mode . "")))
  (gptel-directives '((default     . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
                      (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
                      (writing     . "You are a large language model and a writing assistant. Respond concisely.")
                      (chat        . "You are a large language model and a conversation partner. Respond concisely.")
                      (prog-so     . "I want you to act as a stackoverflow post. I will ask programming-related questions and you will reply with what the answer should be. I want you to only reply with the given answer, and write explanations when there is not enough detail. do not write explanations. When I need to tell you something in English, I will do so by putting text inside curly brackets {like this}.")))
  ; Some tuning settings
  (gptel-temperature 0.6)
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (defun set-default-chat (model-name setup-func)
    "Set the default model, given a MODEL-NAME and the output of the SETUP-FUNC."
    (setq gptel-model model-name
          gptel-backend setup-func))
  ;; In local.el, call the function with an api key to use DeepSeek.
  ;; Here's an example:
  ;; Note: if you do not want to set deepseek as the default chat, simply
  ;;       call the setup-deep-seek function without the set-default-chat wrapping it.
  ;; (use-package gptel
  ;;   :ensure nil
  ;;   :config
  ;;   (set-default-chat 'deepseek-reasoner
  ;;                     (setup-deep-seek "key")))
  (defun setup-deep-seek (apikey)
    "Sets up deepseek with the given APIKEY."
    (gptel-make-deepseek "DeepSeek"
      :stream t
      :key apikey))
  (defun setup-gemini (apikey)
    "Sets up gemini with the given APIKEY."
    (gptel-make-gemini "Gemini"
      :stream t
      :key apikey))
  ;; In local.el, call the function to use Ollama. Model list is automatically created when called.
  ;; Note: if you do not want to set an ollama llm as the default chat, simply
  ;;       call the setup-ollama function without the set-default-chat wrapping it.
  ;; (use-package gptel
  ;;   :ensure nil
  ;;   :config
  ;;   (set-default-chat 'gpt-oss:20b
  ;;                     (setup-ollama)))
  (defun setup-ollama ()
    "Sets up ollama, model list is built from the ollama cli."
    (gptel-make-ollama "Ollama"
      :host "localhost:11434"
      :stream t
      :models (let* ((ollama-ls (->> (split-string (shell-command-to-string "ollama ls") "  ")
                                     (-map 's-trim)
                                     (-filter (lambda (n) (not (string= "" n))))))
                     (size (/ (length ollama-ls) 4))
                     (result ()))
                (dotimes (idx size)
                  (push (nth (* idx 4) ollama-ls) result))
                (->> result
                     (nreverse)
                     (-take-last (- size 1))
                     (-map 'intern))))))

;; Want to test first, dont have time now
;; (use-package eca
;;   :ensure t
;;   :bind (("C-c a e" . eca)
;;          ("C-c a s" . eca-stop)
;;          ("C-c a r" . eca-restart)
;;          ("C-c a w" . eca-workspaces)))

;;; ai.el ends here
