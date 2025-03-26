;;; hydra.el --- Hydra configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; hydra
;;; Code:
;; Hydra makes pretty menus for complex commands
(use-package hydra
  :ensure t
  :demand t
  :bind (("C-c C-v" . my-hydra-rare-modes/body)
         :map Buffer-menu-mode-map
         ("." . hydra-buffer-menu/body))
  :config
  (defhydra my-hydra-rare-modes (:color pink)
    "
^Editing^            ^Debug^           ^Formatting^          ^Text Manipulation
^^^^^^^^--------------------------------------------------------------------------
_a_: abbrev          _d_: debug        _r_: rainbow parens   _s_: search & replace
_f_: fill            _v_: setup venv   _w_: whitespace       _S_: regex & replace
^ ^                  ^ ^               ^ ^                   ^^
"
    ("a" abbrev-mode)
    ("d" toggle-debug-on-error)
    ("f" auto-fill-mode)
    ("r" rainbow-delimiters-mode)
    ("s" query-replace)
    ("S" query-replace-regexp)
    ("t" toggle-truncate-lines)
    ("w" whitespace-mode)
    ("v" my-python-use-venv)
    ("q" nil "quit" :color blue))
  ;; vc-annotate hydra, breaks vc annotate, needs work.
  ;; I want to set something up with major-mode-hydra's pretty-hydra package.
  (defhydra my-hydra-vc-annotate (:color pink)
    "
^Movement^            ^Review^           ^Time^
^^^^^^^^--------------------------------------------------------
_k_: Up               _l_: Show log      _n_: Next revision
_j_: Down             _q_: Exit          _p_: Previous revision
^ ^                   ^ ^                ^^
"
    ("k" previous-line)
    ("j" next-line)
    ("l" vc-annotate-show-log-revision-at-line)
    ("n" vc-annotate-next-revision)
    ("p" vc-annotate-prev-revision)
    ("q" quit-window)
    ("x" nil "quit" :color blue))
  ;; Only in Emacs mode
  (defhydra hydra-buffer-menu (:color pink
                                      :hint nil)
    "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------                        (__)
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch                         (oo)
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch                      /------\\/
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur                 / |    ||
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only^^    *  /\\---/\\
_~_: modified      ^ ^                ^ ^                ^^                                 ~~   ~~
"
    ("m" Buffer-menu-mark)
    ("u" Buffer-menu-unmark)
    ("U" Buffer-menu-backup-unmark)
    ("d" Buffer-menu-delete)
    ("D" Buffer-menu-delete-backwards)
    ("s" Buffer-menu-save)
    ("~" Buffer-menu-not-modified)
    ("x" Buffer-menu-execute)
    ("b" Buffer-menu-bury)
    ("g" revert-buffer)
    ("T" Buffer-menu-toggle-files-only)
    ("O" Buffer-menu-multi-occur :color blue)
    ("I" Buffer-menu-isearch-buffers :color blue)
    ("R" Buffer-menu-isearch-buffers-regexp :color blue)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue)))

;; Switch windows using ace style bindings
(use-package ace-window
  :ensure t
  :bind (("C-x C-o" . ace-window)
         ("C-x M-o" . ace-swap-window)
         ("C-S-w" . ace-swap-window)
         :map evil-insert-state-map
         ("C-w" . ace-window))
  :init
  (evil-define-key 'normal 'global
    (kbd "SPC w") #'ace-swap-window)
  (setq aw-dispatch-always t
        aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g)
        aw-dispatch-alist '((?q aw-delete-window "Delete")
                            (?m aw-move-window "Move")
                            (?y aw-copy-window "Copy")
                            (?n aw-flip-window "Previous")
                            (?w aw-swap-window "Swap")
                            (?o delete-other-windows "Delete other windows")
                            (?? aw-show-dispatch-help))))
;;; hydra.el ends here
