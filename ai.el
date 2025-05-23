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
  ;; In local.el, call the function with a model list to use Ollama.
  ;; Here's an example with my list of preferred llms:
  ;; Note: some other nice llms to have: nomic-embed-text
  ;; Note: if you do not want to set an ollama llm as the default chat, simply
  ;;       call the setup-ollama function without the set-default-chat wrapping it.
  ;; (use-package gptel
  ;;   :ensure nil
  ;;   :config
  ;;   (set-default-chat 'qwen3:32b
  ;;                     (setup-ollama '(deepseek-r1:32b   ; Decent thinking AI
  ;;                                     gemma3:27b        ; Decent text based AI
  ;;                                     qwen3:32b         ; Decent coding AI
  ;;                                     mistral-small3.1:24b
  ;;                                     starcoder2:15b
  ;;                                     devstral:24b      ; Thinking about getting rid of qwen3 and starcoder for this one.
  ;;                                     qwen3:30b-a3b)))) ; Good at mind numbing things with /no_think
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
