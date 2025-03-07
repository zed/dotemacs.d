; -*- coding: utf-8 lexical-binding: t; -*-
;; * configure packages
(when (< emacs-major-version 27)
  (package-initialize))
(init:report-elapsed-time "package-initialize")

;; ** load secrets
;; to fix gpg while using ipad, force reading password from stdin
(unless (display-graphic-p)
  ;; running without GUI
  ;; accept gpg passphrase without GUI/gpg-agent
  (setq epa-pinentry-mode 'loopback))
(require '.secrets "~/.secrets.el.gpg" 'noerror)

;; ** Ensure system binaries exist alongside your package declarations.
;;   Enable :ensure-system-package keyword for use-package
;; https://github.com/jwiegley/use-package?tab=readme-ov-file#keyword-extensions
(use-package use-package-ensure-system-package
  :ensure t)

;; ** delight: remove modes from ModeLine
;; C-h v minor-mode-alist
(use-package delight
  :commands delight)

;; ** which-key: show commands for the current prefix after a delay
(use-package which-key
  :delight
  :config
  (which-key-mode))

;; ** show typed keys
(use-package command-log-mode
  :commands command-log-mode
  :config
  ;; M-x clm/open-command-log-buffer
  (global-command-log-mode))

;; ** theme-changer: use dark theme after sunset
(use-package theme-changer
  :custom
  (calendar-latitude 55.8) ; for solar package
  (calendar-longitude 37.6)
  :init
  (defun init:disable-before-load (theme &optional no-confirm no-enable)
    "Reset old theme settings while loading a new theme"
    (mapc 'disable-theme custom-enabled-themes))
  (advice-add 'load-theme :before #'init:disable-before-load)
  :config
  (change-theme 'modus-operandi-deuteranopia 'modus-vivendi-deuteranopia))

;; ** navigating,searching,selecting lists ivy, swiper, counsel
;; *** ivy
; https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html
(use-package ivy
  :delight
  :init
  (ivy-mode 1)  ; turn on ivy for default functions
  :bind ("C-c C-r" . ivy-resume)
  :config
  ; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-ignore-order))) ; "C-o m" toggles the current regexp builder
  (setq ivy-height 20)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d")
  ; remove "^" from the default regex
  (setq ivy-initial-inputs-alist nil)
  )
;; **** ~C-o~ (=hydra-ivy/body=) invokes Hydra menus with key shortcuts.
;;      When in Hydra, ~C-o~ or ~i~ resumes editing.
(use-package ivy-hydra
  :after (ivy hydra))
(use-package swiper
  )
;; *** amx -- M-x smex fork
(use-package amx
  )  ; used by counsel-M-x
(use-package request
  )  ; used by counsel-search
(use-package counsel
  :ensure-system-package (rg . ripgrep)
  :delight
  :bind (("C-s" . counsel-grep-or-swiper)
	 ("M-x" . counsel-M-x) ; show keybindings
	 ("<f5>" . compile)
	 ("<S-f5>" . counsel-compile)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h a" . counsel-apropos))
  :custom
  (enable-recursive-minibuffers t)
  (counsel-search-engine 'google)
  :init
  (when (executable-find "rg")
					; https://oremacs.com/2017/08/04/ripgrep/
    (setq counsel-grep-base-command
	  "rg -i -M 120 --no-heading --line-number --color never --text -e %s %s"))
  :config
  ;; Enabling counsel-mode remaps built-in Emacs functions that have counsel replacements
  (counsel-mode 1)
  (minibuffer-depth-indicate-mode) ;; for enable-recursive-minibuffers
  )
(with-eval-after-load-feature (counsel vterm)
  (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  (if (equal major-mode 'vterm-mode)
      (let ((inhibit-read-only t)
            (yank-undo-function (lambda (_start _end) (vterm-undo))))
        (cl-letf (((symbol-function 'insert-for-yank)
               (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
    (apply orig-fun args)))

  (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)
  )

;; *** projectile C-c p p
(use-package projectile
  :delight
  :custom
  (projectile-completion-system 'ivy))

(use-package counsel-projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (counsel-projectile-mode +1))

;; counsel-dash
;; *** counsel-dash
(use-package counsel-dash
  :ensure-system-package sqlite3
  :bind ("C-c d" . counsel-dash-at-point)
  :custom
  (dash-docs-browser-func 'eww)

  ;; Note: can't use :config here -- too late for the hook to run then the
  ;; keys are invoked (causing the config) in the corresponding buffer
  :init
  (progn  ;; M-x counsel-dash-install-docset  (after using counsel-dash)
          ;; M-x counsel-dash-install-user-docset -> 404 from https://dashes-to-dashes.herokuapp.com/docsets/contrib
    ;;;; (setq url-debug t) ;; see *URL-DEBUG* buffer
    (defun python3-doc ()
      (interactive)
      (setq-local counsel-dash-docsets
                  '("Pandas" "Python 3" "NumPy" "SciPy" "Matplotlib" "scikit-learn" "seaborn")))

    (add-hook 'python-mode-hook 'python3-doc)
    (add-hook 'org-mode-hook 'python3-doc)

    (defun c++-doc ()
      (interactive)
      (setq-local counsel-dash-docsets
		  '("C++")))
    (add-hook 'c++-mode-hook 'c++-doc)
    (defun c-doc ()
      (interactive) (setq-local counsel-dash-docsets '("C")))
    (add-hook
     'c-mode-hook 'c-doc)
    (defun bash-doc ()
      (interactive)
      (setq-local
       counsel-dash-docsets '("Bash")))
    (add-hook 'sh-mode-hook 'bash-doc))
  :config

  ;; from https://github.com/dash-docs-el/dash-docs/issues/23#issuecomment-1694059091
  (defun dash-docs-unofficial-docsets ()
    "Return a list of lists with docsets contributed by users.
The first element is the docset's name second the docset's archive url."
    (let ((user-docs (assoc-default 'docsets
                                    (dash-docs-read-json-from-url
                                     "https://kapeli.com/feeds/zzz/user_contributed/build/index.json"))))
      (mapcar (lambda (docset)
                (list
                 (assoc-default 'name docset)
                 (car docset)
                 (assoc-default 'archive docset)))
              user-docs)))

  (defun dash-docs-install-user-docset ()
    "Download an unofficial docset with specified DOCSET-NAME and move its stuff to docsets-path."
    (interactive)
    (let* ((docsets (dash-docs-unofficial-docsets))
           (docset-name (dash-docs-read-docset
                         "Install docset"
                         (mapcar 'car docsets)))
           (docset (assoc-default docset-name docsets)))
      (when (dash-docs--ensure-created-docsets-path (dash-docs-docsets-path))
        (let ((url
               (format "https://kapeli.com/feeds/zzz/user_contributed/build/%s/%s"
                       (car docset)
                       (cadr docset))))
          (dash-docs--install-docset url (car docset)))))))

;; ** ripgrep
;;; type `e' in rg-mode to edit search results, `C-x C-s' to save them
(use-package wgrep
  ;; save buffers automatically before finishing wgrep mode
  :custom
  (wgrep-auto-save-buffer t))
(use-package rg
  :ensure-system-package (rg . ripgrep)
  :bind ("C-x C-r" . rg))
;;; https://robbmann.io/emacsd/
(use-package grep
  :config
  (when (executable-find "rg")
    (setq grep-program "rg")
    (grep-apply-setting
     'grep-find-command
     '("rg -n -H --color always --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))))

;; ** ace-link
(use-package ace-link
  :ensure nil
  :config
  (require 'info)
  (ace-link-setup-default))

;; ** magit
(use-package magit
  :ensure-system-package git
  :bind ("C-c M-g" . magit-file-dispatch)
  :bind ("C-c g" . magit-status)
  :custom
  (magit-log-section-commit-count 40)
  (git-commit-major-mode 'markdown-mode)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))
(with-eval-after-load-feature (magit org)
  (define-key magit-process-mode-map (kbd "o") 'ace-link-org))

(use-package forge

  :defer t
  :after magit)

;; ***
(use-package git-gutter

  :delight
  :custom
  (git-gutter:hide-gutter t "Hide gutter when there are no changes")
  (git-gutter:window-width 2)
  (git-gutter:modified-sign "☁")
  (git-gutter:added-sign "☀")
  (git-gutter:deleted-sign "☂")
  :config
  (global-git-gutter-mode t))

;; ***
(use-package git-timemachine
  :commands git-timemachine-toggle)

;; ***
(use-package diffview
  :commands (diffview-region diffview-current))

;; *** handle git conflicts
;;; from https://ladicle.com/post/config/#smerge
(use-package smerge-mode
  :ensure nil
  :delight
  :preface
  (with-eval-after-load 'hydra
    (defhydra smerge-hydra
      (:color pink :hint nil :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
       "Save and bury buffer" :color blue)
      ("q" nil "cancel" :color blue)))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-hydra/body))))))

;; ** debugger
(use-package realgud
  ; run it from a Python file
  ; set breakpoint in command buffer (b <line>), to attach source file
  ; https://stackoverflow.com/questions/21443801/how-to-attach-an-existing-buffer-to-a-realgud-debugger
  :commands (realgud:pdb realgud:trepan3k))

;; ** real-time syntax check
(use-package flycheck
  :defer t
  )

(use-package sh-mode
  :ensure-system-package shellcheck
  :ensure nil
  :hook (sh-mode . flycheck-mode))


;; ** yasnippet
;; from https://github.com/Schnouki/dotfiles/blob/master/emacs/init-30-yasnippet.el
(use-package yasnippet
  :defer t

  :config
  (progn
    ;; Snippets dir:
    ;; - make sure the local one (~/.emacs.d/snippets) comes first
    (setq yas-snippet-dirs
          (cons "~/.emacs.d/snippets"
                (cl-remove-if (lambda (item) (string-equal "~/.emacs.d/snippets" item))
                              yas-snippet-dirs)))
    (yas-global-mode 1)))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet
  )

(use-package pet
  :delight
  :hook (python-base-mode . init:pet-mode-hook)
  :config
  (defun init:pet-mode-hook ()
    "Disable pet-mode for TRAMP sessions."
    (unless (file-remote-p default-directory)
      (pet-mode))))

;; ** elpy (python)
(use-package elpy
  :commands elpy-enable
  :delight
  :init
  ;; https://github.com/jorgenschaefer/elpy/blob/master/docs/introduction.rst
  (advice-add 'python-mode :before 'elpy-enable))

;; *** format python files on save using black if project is configured for black
(use-package blacken
  :ensure-system-package (black . "pipx install black-macchiato --include-deps")
  :ensure nil
  :delight
  :hook (python-mode . blacken-mode)
  :custom
  (blacken-only-if-project-is-blackened t)
  )

;; *** format python files on save using ruff if project is configured for ruff
(use-package ruff-format
  :commands ruff-format-region
  :hook (python-mode . ruffed-enable)
  :config
  (defun ruffed-project-is-ruffed (&optional display)
    "Whether the project has a pyproject.toml with [tool.ruff.format] in it."
    (when-let (parent (locate-dominating-file default-directory "pyproject.toml"))
      (with-temp-buffer
        (insert-file-contents (concat parent "pyproject.toml"))
        (re-search-forward "^\\[tool.ruff.format\\]$" nil t 1))))
  (defun ruffed-enable (&optional _ignored)
    "Enable ruff-format if the project is ruffed."
    (interactive)
    (if (ruffed-project-is-ruffed)
        (add-hook 'before-save-hook 'ruff-format-buffer nil t)
      (remove-hook 'before-save-hook 'ruff-format-buffer t))))

;; *** use ruff as a linter for python-mode
(use-package flymake-ruff
  :hook (python-mode . init:flymake-ruff-load)
  :config
  (defun init:flymake-ruff-load (&optional _ignored)
    "Configure flymake to use ruff check and disable python-flymake backend."
    (interactive)
    (when (derived-mode-p 'python-mode 'python-ts-mode)
      (remove-hook 'flymake-diagnostic-functions #'python-flymake t)
      (add-hook 'flymake-diagnostic-functions #'flymake-ruff--run-checker nil t))))

;; **
(use-package idle-highlight-mode
  :ensure nil
  :hook ((python-mode . init:enable-idle-highlight-mode)
         (emacs-lisp-mode . init:enable-idle-highlight-mode))
  :config
  (defun init:enable-idle-highlight-mode ()
    (idle-highlight-mode t)))

(use-package gist
  :defer 1
  )

;; ** web-mode
(use-package web-mode
  :ensure nil
  :mode "\\.html?\\'")
;; ** yaml-mode
(use-package yaml-mode
  :ensure nil
  :mode "\\.yml\\'"
  :bind ("C-m" . newline-and-indent))

(use-package css-mode
  :ensure nil
  :mode "\\.tcss?\\'")

;; ** typescript
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

;; ** php
(use-package php-mode
  :defer t
  )

;; ** golang
(use-package flycheck-gometalinter

  :defer t
  :config
  (progn
    (flycheck-gometalinter-setup)))

;;; from https://github.com/mswift42/.emacs.d/blob/master/init.el
(use-package company-go

  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

(use-package go-mode
  :defer t
  )

(use-package go-eldoc
  :defer t

  :hook (go-mode-hook . go-eldoc-setup))


;; ** Dockerfile
(use-package dockerfile-mode
  :defer t
  )

;; ** hydra

(use-package hydra
  :ensure nil
  :init
(progn
  ;; https://github.com/abo-abo/hydra/wiki/Basics
  (defhydra hydra-zoom (global-map "C-c")
    "zoom"
    ("+" text-scale-increase "in")
    ("-" text-scale-decrease "out"))

;; *** C-c n/p - next/prev logical line
  (defhydra hydra-logical-line (global-map "C-c")
      "logical line"
    ("n" next-logical-line "next")
    ; note: "C-c p" is reserved for projectile
    ("p" previous-logical-line "prev" :bind nil))

;; *** https://github.com/abo-abo/hydra/wiki/Compilation
  (defhydra hydra-next-error (global-map "C-x")
    "
Compilation errors:
_j_: next error        _h_: first error    _q_uit
_k_: previous error    _l_: last error
"
    ("`" next-error     nil)
    ("j" next-error     nil :bind nil)
    ("k" previous-error nil :bind nil)
    ("h" first-error    nil :bind nil)
    ("l" (condition-case err
             (while t
               (next-error))
           (user-error nil))
     nil :bind nil)
    ("q" nil            nil :color blue))

;; *** https://github.com/abo-abo/hydra/wiki/multiple-cursors
  (global-set-key (kbd "C-c m")  (defhydra hydra-multiple-cursors (:hint nil)
    "
     ^Up^            ^Down^          ^Mark^                ^Edit^            ^Other^
--------------------------------------------------------------------------------------
[_p_]   Next    [_n_]   Next    [_a_] Mark all        [_l_] Edit lines  [_i_] Insert numbers
[_P_]   Skip    [_N_]   Skip    [_m_] Mark all dwim   [_C-a_] Edit BOL  [_R_] Reverse regions
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp  [_C-e_] Edit EOL  [_s_] Sort regions
^ ^             ^ ^             [_d_] Mark in defun   [_C-'_] Hide unmatched [_q_] Quit
"
    ("a" mc/mark-all-like-this :exit t)
    ("d" mc/mark-all-symbols-like-this-in-defun :exit t)
    ("C-'" mc-hide-unmatched-lines-mode)
    ("i" mc/insert-numbers :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("l" mc/edit-lines :exit t)
    ("m" mc/mark-all-dwim :exit t)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("R" mc/reverse-regions)
    ("s" mc/sort-regions)
    ("q" nil)
    ("C-a" mc/edit-beginnings-of-lines :exit t)
    ("C-e" mc/edit-ends-of-lines :exit t)))

;; *** https://github.com/abo-abo/hydra/wiki/Dired
  (with-eval-after-load-feature 'dired
    (define-key dired-mode-map "."
      (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all                                             C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     ^ ^                C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        ^ ^                _s_ort             ^ ^
_S_ymlink          ^ ^                                 _._ toggle hydra   \\ flyspell
^ ^                ^ ^              ^ ^                ^ ^                _?_ summary
^ ^                _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("(" dired-hide-details-mode)
    ("+" dired-create-directory)
    ("?" dired-summary)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy) ;; Copy all marked files
    ("D" dired-do-delete)
    ("G" dired-do-chgrp)
    ("g" revert-buffer) ;; read all directories again (refresh)
    ("i" dired-subtree-insert) ;; insert subtree under its line
    ("l" dired-do-redisplay) ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file) ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("Z" dired-do-compress)
    ("q" nil :color blue)
    ("." nil :color blue))))

;; *** https://github.com/abo-abo/hydra/wiki/Rectangle-Operations
  (with-eval-after-load-feature 'rect
    (global-set-key
     (kbd "C-c r")
     (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
					  :color pink
					  :hint nil
					  :post (deactivate-mark))
       "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _q_ quit     ^ ^                     '---''(./..)-'(_\_)
"
       ("k" rectangle-previous-line)
       ("j" rectangle-next-line)
       ("h" rectangle-backward-char)
       ("l" rectangle-forward-char)
       ("d" kill-rectangle)		     ;; C-x r k
       ("y" yank-rectangle)		     ;; C-x r y
       ("w" copy-rectangle-as-kill)	     ;; C-x r M-w
       ("o" open-rectangle)		     ;; C-x r o
       ("t" string-rectangle)		     ;; C-x r t
       ("c" clear-rectangle)		     ;; C-x r c
       ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
       ("N" rectangle-number-lines)            ;; C-x r N
       ("r" (if (region-active-p)
		(deactivate-mark)
	      (rectangle-mark-mode 1)))
       ("u" undo nil)
       ("q" nil))))

;; *** Windows management
  (progn
;; return to a previous window configuration easily with C-c <left>
(require 'winner)
(winner-mode)
                   ; Navigate windows with s-<arrows>
(windmove-default-keybindings 'super)
(customize-set-variable 'windmove-wrap-around t)

;;
(customize-set-variable 'confirm-kill-processes nil)

(with-eval-after-load-feature (ivy counsel)
  ; 404 for hydra-move-splitter
  (defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(global-set-key (kbd "C-c w") (defhydra hydra-window (:hint nil)
  "
Movement^^        ^Split^         ^Switch^      ^Resize^
----------------------------------------------------------------
_j_ ←          _v_ertical       _b_uffer         _J_ X←
_k_ ↓          _h_ horizontal   _f_ind files     _K_ X↓
_i_ ↑          _u_ undo         _a_ce 1          _I_ X↑
_l_ →         _r_ reset        _s_ave           _L_ X→
_q_ cancel     _D_lt Other      _S_wap           _m_aximize
^ ^            _o_nly this      _d_elete
"
  ("j" windmove-left )
  ("k" windmove-down )
  ("i" windmove-up )
  ("l" windmove-right )
  ("J" hydra-move-splitter-left)
  ("K" hydra-move-splitter-down)
  ("I" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("b" ivy-switch-buffer :color blue)
  ("B" ivy-switch-buffer)
  ("f" counsel-find-file :color blue)
  ("F" counsel-find-file)
  ("a" (lambda ()
	 (interactive)
	 (ace-window 1)
	 (add-hook 'ace-window-end-once-hook
		   'hydra-window/body)))
  ("h" (lambda ()
	 (interactive)
	 (split-window-right)
	 (windmove-right)))
  ("v" (lambda ()
	 (interactive)
	 (split-window-below)
	 (windmove-down)))
  ("S" (lambda ()
	 (interactive)
	 (ace-window 4)
	 (add-hook 'ace-window-end-once-hook
		   'hydra-window/body))
        :color blue)
  ("s" save-buffer :color blue)
  ("d" delete-window :color blue)
  ("D" (lambda ()
	 (interactive)
	 (ace-window 16)
	 (add-hook 'ace-window-end-once-hook
		   'hydra-window/body)))
  ("o" delete-other-windows :color blue)
  ("O" delete-other-windows)
  ("m" ace-delete-other-windows :color blue)
  ("M" ace-delete-other-windows)
  ("u" (progn
	 (winner-undo)
	 (setq this-command 'winner-undo)))
  ("r" winner-redo)
  ("q" nil)))))))

;; *** PDF Tools https://github.com/abo-abo/hydra/wiki/PDF-Tools
(use-package pdf-tools
  :config
  (pdf-tools-install))
(with-eval-after-load-feature 'pdf-tools
  (setq-default pdf-view-display-size #'fit-page)
   (defhydra hydra-pdftools (:color blue :hint nil)
    "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_ _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
    ("\\" nil "back")
    ("<ESC>" nil "quit")
    ("al" pdf-annot-list-annotations)
    ("ad" pdf-annot-delete)
    ("aa" pdf-annot-attachment-dired)
    ("am" pdf-annot-add-markup-annotation)
    ("at" pdf-annot-add-text-annotation)
    ("y"  pdf-view-kill-ring-save)
    ("+" pdf-view-enlarge :color red)
    ("-" pdf-view-shrink :color red)
    ("0" pdf-view-scale-reset)
    ("H" pdf-view-fit-height-to-window)
    ("W" pdf-view-fit-width-to-window)
    ("P" pdf-view-fit-page-to-window)
    ("n" pdf-view-next-page-command :color red)
    ("p" pdf-view-previous-page-command :color red)
    ("d" pdf-view-dark-minor-mode)
    ("b" pdf-view-set-slice-from-bounding-box)
    ("r" pdf-view-reset-slice)
    ("g" pdf-view-first-page)
    ("G" pdf-view-last-page)
    ("e" pdf-view-goto-page)
    ("o" pdf-outline)
    ("s" pdf-occur)
    ("i" pdf-misc-display-metadata)
    ("u" pdf-view-revert-buffer)
    ("F" pdf-links-action-perfom)
    ("f" pdf-links-isearch-link)
    ("B" pdf-history-backward :color red)
    ("N" pdf-history-forward :color red)
    ("l" image-forward-hscroll :color red)
    ("h" image-backward-hscroll :color red))
   (progn
    (define-key pdf-view-mode-map (kbd "\\") #'hydra-pdftools/body)
    (define-key pdf-view-mode-map (kbd "<s-spc>") #'pdf-view-scroll-down-or-next-page)
    (define-key pdf-view-mode-map (kbd "g")  #'pdf-view-first-page)
    (define-key pdf-view-mode-map (kbd "G")  #'pdf-view-last-page)
    (define-key pdf-view-mode-map (kbd "l")  #'image-forward-hscroll)
    (define-key pdf-view-mode-map (kbd "h")  #'image-backward-hscroll)
    (define-key pdf-view-mode-map (kbd "j")  #'pdf-view-next-page)
    (define-key pdf-view-mode-map (kbd "k")  #'pdf-view-previous-page)
    (define-key pdf-view-mode-map (kbd "e")  #'pdf-view-goto-page)
    (define-key pdf-view-mode-map (kbd "u")  #'pdf-view-revert-buffer)
    (define-key pdf-view-mode-map (kbd "al") #'pdf-annot-list-annotations)
    (define-key pdf-view-mode-map (kbd "ad") #'pdf-annot-delete)
    (define-key pdf-view-mode-map (kbd "aa") #'pdf-annot-attachment-dired)
    (define-key pdf-view-mode-map (kbd "am") #'pdf-annot-add-markup-annotation)
    (define-key pdf-view-mode-map (kbd "at") #'pdf-annot-add-text-annotation)
    (define-key pdf-view-mode-map (kbd "y")  #'pdf-view-kill-ring-save)
    (define-key pdf-view-mode-map (kbd "i")  #'pdf-misc-display-metadata)
    (define-key pdf-view-mode-map (kbd "s")  #'pdf-occur)
    (define-key pdf-view-mode-map (kbd "b")  #'pdf-view-set-slice-from-bounding-box)
    (define-key pdf-view-mode-map (kbd "r")  #'pdf-view-reset-slice)))


;; ** org-fc :: spaced repetition system SRS in emacs
(use-package org-fc
  :load-path "~/src/org-fc"
  :custom (org-fc-directories '("~/org/"))
  :bind
  ("C-c f" . org-fc-hydra/body)
  :config
  (require 'org-fc-hydra))

;; ** misc
(progn
					; uniquify buffers with the same name
					; instead of buf<2>, etc it shows
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

                    ; enable recent files menu

(recentf-mode t)
(setq recentf-max-saved-items 100)

                    ; use 4 spaces instead of tabs for indentation
                    ; http://stackoverflow.com/a/471916/
(setq tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))


;; Part of the Emacs Starter Kit
;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el) to jump to it.
;; You should add registers here for the files you edit most often.
(dolist (r `((?i (file . ,(expand-file-name "~/.emacs")))))
  (set-register (car r) (cadr r)))

;; Commands which ask for a destination directory, such as those which
;; copy and rename files or create links for them, try to guess the
;; default target directory for the operation. Normally, they suggest
;; the Dired buffer's default directory, but if the variable
;; dired-dwim-target is non-nil, and if there is another Dired buffer
;; displayed in the next window, that other buffer's directory is
;; suggested instead.
(setq dired-dwim-target t)

;; [[~/src/emacs-starter-kit/starter-kit-bindings.el]]
;; . Indentation help
(global-set-key (kbd "C-x ^") #'join-line)

                    ; [[https://github.com/dimitri/emacs-kicker/blob/master/init.el]]
                    ; If you do use M-x term, you will notice there's line mode that acts like
                    ; emacs buffers, and there's the default char mode that will send your
                    ; input char-by-char, so that curses application see each of your key
                    ; strokes.
                    ;
                    ; The default way to toggle between them is C-c C-j and C-c C-k, let's
                    ; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") #'term-line-mode)
(define-key term-mode-map (kbd "C-'") #'term-char-mode)
                    ; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
                    ; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") #'term-paste)

(global-unset-key (kbd "C-x C-c"))

                    ; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x))

;; *** Insert directory subtree at point or remove it if it was not present
(use-package dired-subtree
  :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle)))

;; ** Save point position between sessions
(require 'saveplace)
(setq-default save-place t) ; set global default value for buffer local variable
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; ** save minibuffer history
(use-package savehist
  :ensure nil
  :config (savehist-mode))


;; ** gnus
(with-eval-after-load "gnus"
  (setq mm-discouraged-alternatives '("text/html" "text/richtext"))
  (setq gnus-message-archive-group
	'((if (message-news-p)
	      "sent-news"
	    "sent-mail")))
  (setq gnus-use-adaptive-scoring t)
  (setq gnus-score-expiry-days 14)
  (setq gnus-default-adaptive-score-alist
	'((gnus-unread-mark)
	  (gnus-ticked-mark (from 4))
	  (gnus-dormant-mark (from 5))
	  (gnus-saved-mark (from 20) (subject 5))
	  (gnus-del-mark (from -2) (subject -5))
	  (gnus-read-mark (from 2) (subject 1))
	  (gnus-killed-mark (from 0) (subject -3))))

  (setq gnus-decay-scores t)
  (setq gnus-global-score-files
	'("~/gnus/scores/all.SCORE"))

  ;; all.SCORE contains:
  (setq gnus-summary-expunge-below -999)
  (setq gnus-summary-line-format "%O%U%R%z%d %B%(%[%4L: %-22,22f%]%) %s\n")
  (setq gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")
  (setq gnus-summary-same-subject "")
  (setq gnus-sum-thread-tree-root "")
  (setq gnus-sum-thread-tree-single-indent "")
  (setq gnus-sum-thread-tree-leaf-with-other "+-> ")
  (setq gnus-sum-thread-tree-vertical "|")
  (setq gnus-sum-thread-tree-single-leaf "`-> ")
  (setq message-generate-headers-first t)
  (setq message-kill-buffer-on-exit t)
  (add-hook 'message-mode-hook #'turn-on-auto-fill)
  (add-hook 'message-sent-hook #'gnus-score-followup-article)
  (add-hook 'message-sent-hook #'gnus-score-followup-thread)
  (setq gnus-directory "~/gnus")
  (setq message-directory "~/gnus/mail")
  (setq nnml-directory "~/gnus/nnml-mail")
  (setq gnus-article-save-directory "~/gnus/saved")
  (setq gnus-kill-files-directory "~/gnus/scores")
  (setq gnus-cache-directory "~/gnus/cache")

  (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

  (require 'gnus-registry)
  (gnus-registry-initialize)

  (setq gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:\\|^User-Agent:\\|^X-Mailer:\\|^X-Newsreader:")

  (setq gnus-sorted-header-list '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^Followup-To:" "^To:" "^Cc:" "^Date:" "^User-Agent:" "^X-Mailer:" "^X-Newsreader:"))

  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)

  (define-key gnus-summary-mode-map [(meta up)] (lambda() (interactive) (scroll-other-window -1)))
  (define-key gnus-summary-mode-map [(meta down)] (lambda() (interactive) (scroll-other-window 1)))
  (define-key gnus-summary-mode-map [(control down)] #'gnus-summary-next-thread)
  (define-key gnus-summary-mode-map [(control up)] #'gnus-summary-prev-thread)
  (setq spam-directory "~/gnus/spam/")

  (setq gnus-spam-process-newsgroups
	'(("^gmane\\."
	   ((spam spam-use-gmane)))))

  (require 'spam))

;; ** irc
(defun irc-start ()
  (interactive)
  (.secrets-irc-start))

(with-eval-after-load "erc"
					; IRC client (*nix only)


					; . Make C-c RET (or C-c C-RET) send messages instead of RET.
  (define-key erc-mode-map (kbd "RET") nil)
  (define-key erc-mode-map (kbd "C-c RET") #'erc-send-current-line)
  (define-key erc-mode-map (kbd "C-c C-RET") #'erc-send-current-line)
					; Kill buffers for channels after /part
  (setq erc-kill-buffer-on-part t)
					; Kill buffers for private queries after quitting the server
  (setq erc-kill-queries-on-quit t)
					; Kill buffers for server messages after quitting the server
  (setq erc-kill-server-buffer-on-quit t)

  (setq erc-auto-query 'buffer)		; add a whois when someone pms

					; keep erc from eating ram by truncating chat logs
  (setq erc-max-buffer-size 20000)
  (add-hook 'erc-insert-post-hook #'erc-truncate-buffer)


					; Notify when someone mentions my nick.
					; http://bbs.archlinux.org/viewtopic.php?id=40190
  (defun erc-global-notify (matched-type nick msg)
    (interactive)
    (when (eq matched-type 'current-nick)
      (shell-command
       (concat "notify-send -t 4000 -c \"im.received\" \""
	       (car (split-string nick "!"))
	       " mentioned your nick\" \""
	       msg
	       "\""))))
  (add-hook 'erc-text-matched-hook #'erc-global-notify))

;; ** misc
                    ; * https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks t)


                    ; suppress Warning (mule): Invalid coding system `ascii' is specified
(define-coding-system-alias 'ascii 'us-ascii)

(setq compilation-ask-about-save nil)
(setq compilation-read-command nil)

                    ; use 'y'/'n' instead of 'yes'/'no'
(fset 'yes-or-no-p 'y-or-n-p)

                    ; whenever an external process changes a file underneath emacs, and there
                    ; was no unsaved changes in the corresponding buffer, just revert its
                    ; content to reflect what's on-disk.
(global-auto-revert-mode 1)

(column-number-mode)   ; enable columns numbers globally, it has a performance hit

;;
(defun init-trailing-whitespace-hook ()
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

;; don't edit non programming files such as images (png, jpg, etc)
;; https://stackoverflow.com/questions/6138029/how-to-add-a-hook-to-only-run-in-a-particular-mode
(add-hook 'prog-mode-hook #'init-trailing-whitespace-hook)
;;; for some reason groovy mode is not derived from the prog mode
(add-hook 'groovy-mode-hook #'init-trailing-whitespace-hook)
(add-hook 'markdown-mode-hook #'init-trailing-whitespace-hook)
(add-hook 'yaml-mode-hook #'init-trailing-whitespace-hook)

                    ; copy/kill line on M-w, C-w
(defun init:slickcopy (beg end &optional region)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(advice-add 'kill-ring-save :before #'init:slickcopy)
(defun init:slickcut (beg end &optional region)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(advice-add 'kill-region :before #'init:slickcut)

                    ; http://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 vc-make-backup-files t   ; make backups of files, even when they're in version control
 version-control t)       ; use versioned backups

                    ; Open *.m in Octave-mode instead of ObjC
(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))

                    ; style I want to use in c++ mode
                    ; from http://www.emacswiki.org/emacs/CPlusPlusMode
(c-add-style "my-style"
         '("python"
           (indent-tabs-mode . nil)        ; use spaces rather than tabs
           (c-basic-offset . 2)))
(defun init:c-mode-common-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c-mode-common-hook #'init:c-mode-common-hook)

;; from https://emacs.stackexchange.com/questions/508/how-to-configure-specific-java-indentation
;; by @Sigma
(c-add-style "my-java-style"
	       '((c-basic-offset . 4)
		 (c-offsets-alist . ((inline-open . 0)
				     (topmost-intro-cont    . +)
				     (statement-block-intro . +)
				     (knr-argdecl-intro     . 5)
				     (substatement-open     . +)
				     (substatement-label    . +)
				     (label                 . +)
				     (statement-case-open   . +)
				     (statement-cont        . ++)
				     (arglist-intro  . c-lineup-arglist-intro-after-paren)
				     (arglist-close  . c-lineup-arglist)
				     (access-label   . 0)
				     (inher-cont     . ++)
                      (func-decl-cont . ++)))))
(defun init:java-mode-hook ()
  (c-set-style "my-java-style"))
(add-hook 'java-mode-hook #'init:java-mode-hook)

;; multi-language spell checking
;; https://github.com/Hi-Angel/dotfiles/blob/55e1daf5266ee997e72700cf7d8b6463e2b02531/.emacs#L859-L870
(use-package ispell
  :ensure-system-package (hunspell hunspell-ru)
  :init
  ;; It's unclear if the default aspell supports multiple langs at once, but Emacs
  ;; with aspel backend doesn't. Let's use hunspell instead.
  (setq ispell-program-name "hunspell")
  (setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))
  :config
  (ispell-set-spellchecker-params) ;; ispell initialization, a mandatory call
  (ispell-hunspell-add-multi-dic "en_US,ru_RU")
  (ispell-change-dictionary "en_US,ru_RU" t) ;; with t set dict globally
  )

;; ** flyspell
(use-package flyspell
  :ensure nil
  :defer t
  :delight
  :hook
  ((org-mode yaml-mode markdown-mode git-commit-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  )


;; ** configure org
(use-package org
  :ensure nil
  :bind (
         :map org-mode-map
         ("C-c C-x C-s" . org-archive-subtree-default-with-confirmation)
         )
  :custom
  (org-image-max-width 1.0 "limit the displayed image width")
  (org-list-allow-alphabetical t "allow a) b) c) plain list items")
  (org-id-link-to-org-use-id t "`C-c l' uses ID to link to an org entry")
  (org-enforce-todo-dependencies t "can't mark DONE if there are TODO children")
  (org-use-property-inheritance t "properties such as attachment dirs apply also for sublevels")
  (org-src-preserve-indentation nil "leading whitespace blocks are stripped")
  (org-edit-src-content-indentation 0 " and the code block is not indented (0!)")
  (org-src-tab-acts-natively t "make TAB insert spaces in python src blocks")
  (org-export-use-babel nil "disable evaluation of babel code blocks on export")
  (org-log-into-drawer t "hide State DONE. Useful for repeating tasks")
  (org-export-backends '(md odt latex icalendar html ascii) "List of export back-ends that should be always available.")
  (org-modules '(org-habit ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww org-tempo))
  :hook
  (kill-emacs . init:org-clock-out-and-save)
  :preface
  (defun init:org-clock-out-and-save ()
    "Save buffers and stop clocking."
    (ignore-errors (org-clock-out) t)
    (save-some-buffers t))
  :init
  ;; export to the kill ring automatically for interactive exports
  (setq org-export-copy-to-kill-ring 'if-interactive)
  ;; orgmobile
  (setq org-mobile-use-encryption t)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!)" "DEFERRED(e)" "CANCELED(c)")))

  (setq org-tag-alist '((:startgroup . nil) ("home" . ?h) ("work" . ?w) (:endgroup . nil)
			("idea" . nil) ("code" . nil) ("pythonista" . nil) ("org" . nil)
			("so" . nil) ("birthday" . nil) ("buy" . nil) ("quick" . nil)
			("tutorial" . nil) ("github" . nil) ("day" . nil) ("pyopenssl" . nil)
			("cluster" . nil) ("read" . nil) ("book" . nil)
			("feature" . nil) ("psutil" . nil) ("twisted" . nil) ("cpython" . nil)
			("subprocess" . nil) ("bug" . nil) ("easy" . nil) ("stackoverflow" . ?s)
			("wurlitzer" . nil) ("lrange" . nil) ("telegram" . nil)
			("d0" . ?d) ("telethon" . nil) ("emacs" . ?e)))
  (setq org-agenda-log-mode-items '(closed clock state)) ; show DONE

  ;; Effort and global properties
  (setq org-global-properties '(("Effort_ALL". "0 0:10 0:20 0:30 1:00 2:00 4:00 8:00")))

  ;; Set global Column View format
  (setq org-columns-default-format '"%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM(Clock)")


					; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull (getenv "ORG_MOBILE_INBOX_FOR_PULL"))
					; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory (getenv "ORG_MOBILE_DIRECTORY"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "inbox.org" "Tasks")
	   "* TODO %?\n  %i\n  %a" :clock-in t :clock-resume t)
	  ("j" "Journal" entry (file+olp+datetree "journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")))

  ;; start agenda view relative to current day, show given number of days
  (setq org-agenda-span 1
	org-agenda-start-on-weekday nil)

  ;; if non-nill, only show habits in today’s agenda view
  (setq org-habit-show-habits-only-for-today nil)

  ;; shift the org habit graph in the agenda further out right so as
  ;; to leave enough room for the headings to be visible.
  (setq org-habit-graph-column 90)
  (setq org-confirm-babel-evaluate nil)
  :bind (("C-c l" . org-store-link)
	 ("C-c o" . org-open-at-point-global)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
         ("C-c C-x C-j" . org-clock-goto))
  :config
  (define-key global-map (kbd "M-o") 'ace-link-org)

  ;; how large are inline latex formula (between $$ on =C-x C-c C-l=)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  ;; resume the clock under the assumption that you have worked on this task while outside Emacs
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-reverse-note-order t)
  (setq org-agenda-include-diary t)

  (setq org-plantuml-jar-path
        ;; sudo apt install default-jre graphviz
        (expand-file-name "~/src/plantuml/plantuml.jar"))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; Mark heading done when all subtasked are done
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; Mark heading done when all checkboxes are checked.
  ;; https://orgmode.org/worg/org-hacks.html#mark-done-when-all-checkboxes-checked
  ;; see https://list.orgmode.org/87r5718ytv.fsf@sputnik.localhost
  (eval-after-load 'org-list
    '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

  (defun ndk/checkbox-list-complete ()
    (save-excursion
      (org-back-to-heading t)
      (let ((beg (point)) end)
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    ;; all done - do the state change
                    (org-todo 'done)
                  (org-todo 'todo))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (org-todo 'done)
                (org-todo 'todo)))))))

  ;; FIXME: workaround
  ;; https://github.com/syl20bnr/spacemacs/issues/11798
  (when (version<= "9.2" (org-version))
    (require 'org-tempo))

  ;; archive logbook entries
  (progn (defun qpet/org-archive-delete-logbook ()
           (save-excursion
             (org-end-of-meta-data)
             (let ((elm (org-element-at-point)))
               (when (and
                      (equal (org-element-type elm) 'drawer)
                      (equal (org-element-property :drawer-name elm) "LOGBOOK"))
                 (delete-region (org-element-property :begin elm)
                                (org-element-property :end elm))))))

         (defun qpet/org-archive-without-delete ()
           (cl-letf (((symbol-function 'org-cut-subtree) (lambda () nil)))
             (org-archive-subtree)))

         (defun qpet/org-archive-logbook ()
           (interactive)
           (qpet/org-archive-without-delete)
           (qpet/org-archive-delete-logbook)))

  ;; copy link url from org to outside of org mode
  ;; https://emacs.stackexchange.com/questions/3981/how-to-copy-links-out-of-org-mode
  (progn
    (defun iqbal-yank-org-link (text)
      (if (derived-mode-p 'org-mode)
          (insert text)
        (string-match org-bracket-link-regexp text)
        (insert (substring text (match-beginning 1) (match-end 1)))))

    (defun iqbal-org-retrieve-url-from-point ()
      (interactive)
      (let* ((link-info (assoc :link (org-context)))
             (text (when link-info
                     ;; org-context seems to return nil if the current element
                     ;; starts at buffer-start or ends at buffer-end
                     (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                     (or (caddr link-info) (point-max))))))
        (if (not text)
            (error "Not in org link")
          (add-text-properties 0 (length text) '(yank-handler (iqbal-yank-org-link)) text)
          (kill-new text))))))

(use-package org-agenda
  :ensure nil
  :config
  (define-key org-agenda-mode-map (kbd "o") 'ace-link-org))

(use-package hl-line
  :ensure nil
  :after org
  :hook
    (org-mode . hl-line-mode))

(use-package htmlize
  :commands (htmlize-buffer htmlize-file htmlize-many-files htmlize-many-files-dired)
  )

(use-package ox-jira
  :after org
  :commands ox-jira-export-as-jira
  )

(use-package ox-gfm
  :after org
  :commands org-gfm-export-to-markdown
  )

;;
(use-package ox-slack
  :after org
  :commands org-slack-export-to-clipboard-as-slack
  )

(use-package ob-async
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-ansible" "jupyter-julia")))
(use-package ob-restclient)

(use-package jupyter
  :after org
  :defer 2
  :config
  (setq jupyter-use-zmq nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (shell . t)
                                        ; https://emacs.stackexchange.com/questions/37692/how-to-fix-symbols-function-definition-is-void-org-babel-get-header
     (plantuml . t)
                                        ; http://eschulte.github.io/babel-dev/DONE-integrate-plantuml-support.html
     (R . t)
     (sqlite . t)
     (restclient . t)
     (jupyter . t)))
                                        ; must be last https://github.com/dzop/emacs-jupyter#org-mode-source-blocks
  ;; default args for jupyter-python
  (setq org-babel-default-header-args:jupyter-python
        '((:results . "replace")
          (:display . "plain")
	  (:async . "yes")
	  (:session . "py3.12")
          ; to list: (jupyter-available-kernelspecs 'refresh)
	  (:kernel . "python3")))
  ;; default args for sh
  (setq org-babel-default-header-args:sh
        '((:results . "output")))
  (setq org-babel-default-header-args:shell
        '((:results . "output")))
  ;; fix:
  ;; jupyter-api-url-request: Jupyter REST API error: 404, "Not found"
  ;; from
  ;; https://github.com/emacs-jupyter/jupyter/issues/542#issuecomment-2210783148
  (defun gm/jupyter-api-request-xsrf-cookie-error-advice (func &rest args)
  (condition-case nil
      (apply func args)
    (jupyter-api-http-error nil)))
  (advice-add 'jupyter-api-request-xsrf-cookie :around #'gm/jupyter-api-request-xsrf-cookie-error-advice)
  )

(use-package f)
(with-eval-after-load-feature 'f
  ;; https://www.mail-archive.com/emacs-orgmode@gnu.org/msg129554.html
  (define-advice org-babel-execute-src-block (:filter-args (&optional args)
                                                           set-detault-dir-to-org-attach-path)
    "Set working directory to the current entry's attach directory."
    (if (eq major-mode 'org-mode)
        (let* ((directory (file-name-as-directory (org-attach-dir
                                                   'create-if-none)))
               (arg (car args))
               (info (cadr args))
               (params (org-babel-merge-params (nth 2 info) (caddr args)))
               (dir-param (alist-get :dir params)))
          (unless (and dir-param (or (equal (f-full default-directory) (f-full
                                                                        dir-param))
                                     (f-absolute-p dir-param)))
            (setf (alist-get :dir params)
                  (if dir-param
                      (f-join directory (alist-get :dir params))
                    directory)))
          (list arg info params))
      (list arg info params))))

;; *** pretty asterisk symbols for org mode
(use-package org-bullets
  :after org

  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ** recursive directory tree comparison: M-x ztree-diff
(use-package ztree
  :defer t
  ) ; needs GNU diff utility

;; ** fzf
(use-package fzf
  )

;; ** groovy
(use-package groovy-mode
  :defer t
  )

(use-package jenkinsfile-mode
  :defer t
  :custom
  (groovy-indent-offset 2))

;; ** python black
(use-package python-black
  :ensure nil
  :demand t
  :after blacken
  )

;; ** M-x elisp-format-region
(use-package elisp-format
  :defer 100
  )

;; ** google-this
(use-package google-this
  :defer t
  :delight
  :custom
  (google-this-location-suffix "com" "(com, co.uk, fr, etc)")
  ;;  note: google-this-mode enables "C-c /" map
  (google-this-mode t)
  :config
  ;; override google-this-url to force English
  (defun google-this-url ()
    "URL for google searches."
    (concat google-this-base-url google-this-location-suffix "/search?hl=en&ion=1&q=%s")))

;; ** display web search results in Ivy-enabled minibuffer
(use-package counsel-web
  :commands (counsel-web-suggest counsel-web-search counsel-web-thing-at-point)
  :bind
  ("C-c s" . counsel-web-suggest-thing-at-point)
  :custom
  (counsel-web-engine 'google)
  :config
  ;; start web search suggesting a thing at point
  (defun counsel-web-suggest-thing-at-point (thing)
    "Use THING as the initial input for counsel-web-suggest"
    (interactive (list (thing-at-point counsel-web-thing)))
    (counsel-web-suggest thing)))

;; ** C-a moves the point to the first non-whitespace character on the line
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)))

;; ** visible bookmark to jump around a file
(use-package bm
  :bind (("<C-f2>" . bm-toggle)
         ("<f2>" . bm-next)
         ("<S-f2>" . bm-previous)))

;; ** treemacs https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; ** config for company
(use-package company
  :ensure nil
  :delight
  :init
  (global-company-mode)
  (with-eval-after-load-feature (company restclient)
    (add-to-list 'company-backends 'company-restclient)))

;; ** slack config
(use-package alert)  ;; fix Error (use-package): Failed to install slack: https://melpa.org/packages/alert-20191126.2032.el: Not found
(use-package slack
  :after alert
  :commands slack-start
  :custom
  (slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (slack-prefer-current-team t)
  (slack-display-team-name nil)
  :config
  (.secrets-slack-register-team))

;; ** emacs-libvterm
;; - C-c C-t :: to toggle vterm-copy-mode, press Enter to leave
;; - C-c C-n and C-c C-p :: next/previous prompt
(use-package vterm
  :commands (vterm vterm-other-window)
  :config
  (unbind-key "<f11>" vterm-mode-map)
  :custom
  (vterm-max-scrollback 100000) ;; SB_MAX 100000
  (vterm-buffer-name-string "vterm %s"))

;; ** enable Emacs keybinding in the non-default system layout too
;; to see the remappings: M-x reverse-im-which-key-show
(use-package reverse-im
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;; ** Deleting a whitespace character will delete all whitespace until the next non-whitespace character
(use-package hungry-delete
  :delight
  :config
  (global-hungry-delete-mode))

;; ** grep for definitions
(use-package dumb-jump
  :commands dumb-jump-go
  :init
  (defhydra dumb-jump-hydra (global-map "M-g" :color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

;; ** delete upto char
(use-package avy-zap
  :bind
  ("M-z" . avy-zap-to-char-dwim)
  ("M-z" . avy-zap-up-to-char-dwim))

;; ** with with jira issues in org mode
(use-package org-jira
  :commands org-jira-get-issues)

;; ** automatically toggle a latex fragment preview in org-mode
;;    when cursor enters/leaves it
(use-package org-fragtog
  :after org
  :hook ((org-mode . org-fragtog-mode)))

;; ** syntax highlighting for TLA+ code
(use-package tla-mode
  :ensure nil
  :mode "\.tla$")

;; ** syntax highlighting for Zig code
(use-package zig-mode
  :mode "\.zig$")


;; ** ox-ipynb — Convert an org-file to an ipynb (jupyter notebook)
(use-package ox-ipynb
  :ensure nil
  ;; Works interactively:
  ;;   M-x ox-ipynb-export-org-file-to-ipynb-file RET
  ;; Works non-interactively:
  ;;   (ox-ipynb-export-org-file-to-ipynb-file "test.org")
  ;; Works in Dired+ by marking some *.org files
  ;;   and pressing "@" ox-ipynb-export-org-file-to-ipynb-file RET
  :commands ox-ipynb-export-org-file-to-ipynb-file)

;; ** C-x n n recursive-narrow-or-widen-dwim
(use-package recursive-narrow)

;; ** R support
(use-package ess)

;; ** direnv: .envrc envvars visible for commands started in emacs
(use-package envrc
  ; expect envrc.el to cause less performance issues than direnv.el
  :config
  ; buffer-local direnv integration for Emacs
  (envrc-global-mode))

;; * create presentations in emacs
;;   reveal.js
(use-package ox-reveal)

;; * colorize *compilation* buffer output (interpret escape codes)
(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))

(use-package xterm-color
  :commands xterm-color-colorize-buffer) ; interpret ansi escape codes

(use-package rust-mode
  :hook (rust-mode . prettify-symbols-mode)
  :custom
  (rust-format-on-save t))
;; ** htmlz-mode -- simple live html preview in browser
(use-package htmlz-mode
  :vc (:url "https://github.com/zed/htmlz-mode"
       :rev "build/set-package-requires")
  ;; M-x htmlz-mode in html buffer, to enable the minor-mode
  :commands htmlz-mode)

(use-package minimap
  :commands minimap-mode
  :custom  (minimap-window-location 'right)
  :init
  (setq minimap-major-modes '(prog-mode text-mode)))

; an alternative for *help* buffer presentation
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-key helpful-command helpful-at-point helpful-function))

; pulsar
; https://protesilaos.com/emacs/pulsar#h:6ca69953-1a89-4968-a46c-2fa5e57aca9b
(use-package pulsar
  :hook (window-state-change . pulsar-pulse-line))

; https://github.com/karthink/gptel?tab=readme-ov-file#usage
(use-package gptel
  :commands (gptel gptel-send gptel-rewrite)
  :config
  (setq
 gptel-model 'llama3.2:latest
 gptel-backend (gptel-make-ollama "llama3.2"
                 :host "localhost:11434"
                 :stream t
                 :models '(llama3.2:latest))))

;
(use-package protobuf-mode
  :ensure nil   ; installed in early-init.el
  :hook (protobuf-mode . init:protobuf-mode-hook)
  :config
  (defconst init:protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  (defun init:protobuf-mode-hook ()
    (c-add-style "init-protobuf-style" init:protobuf-style t)))

;; * ^^^last non-core use-package
(init:report-elapsed-time "use-package")

;; ** core emacs settings
(use-package emacs
  :ensure nil
  :custom
  (tramp-allow-unsafe-temporary-files t "suppress: Autosave file on local temporary directory")
  (enable-local-variables :safe) ; suppress prompt for safe variables
  (indent-tabs-mode nil)
  (menu-bar-mode nil) ; hide Menu Bar
  (tool-bar-mode nil) ; hide Tool Bar
  (set-mark-command-repeat-pop t)
                                        ; use Org mode for the *scratch* buffer
  (initial-major-mode 'org-mode)
                                        ; make the *scratch* buffer empty
  (initial-scratch-message "")
  :hook
  (after-save . init:chmod+x-files-with-shebang)
  (prog-mode . show-paren-mode)
  ;; automatically create matching parens in programming modes but not in org-mode
  (prog-mode . electric-pair-mode) ; Enable in programming modes
  (org-mode . init:disable-electric-pair-mode) ; Disable in org-mode
  :config
                                        ; delete region when we yank on top of it
  (delete-selection-mode t)
                                        ; try to detect long line and mitigate performance issues
  (global-so-long-mode)

  (defun init:disable-electric-pair-mode ()
    "Disable electric-pair-mode in the current buffer."
    (electric-pair-local-mode -1))
  (defun init:chmod+x-files-with-shebang ()
    (unless (string-match "__init__.py" (or (buffer-file-name) ""))
      (executable-make-buffer-file-executable-if-script-p)))
  (defun init:align-regexp-with-spaces (orig-fun &rest args)
    (let ((indent-tabs-mode nil))
      (apply orig-fun args)))
  (advice-add 'align-regexp :around #'init:align-regexp-with-spaces))

;; make C-g close open minibuffer
;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)


(setq nand2tetris-core-base-dir (getenv "NAND2TETRIS_CORE_BASE_DIR"))
(setq safe-local-variable-values
      (quote
       ((encoding . utf-8)
        (whitespace-style face trailing lines-tail)
        (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
              (add-hook
               (quote write-contents-functions)
               (lambda nil
                 (delete-trailing-whitespace)
                 nil))
              (require
               (quote whitespace))
              "Sometimes the mode needs to be toggled off and on."
              (whitespace-mode 0)
              (whitespace-mode 1))
        (whitespace-line-column . 80)
        (whitespace-style face tabs trailing lines-tail)
        (require-final-newline . t))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; ** https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f4>") #'ace-window)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)
(delight 'my-keys-minor-mode nil t)

;; ** https://unix.stackexchange.com/questions/108259/how-do-i-stop-emacs-from-asking-me-if-i-want-to-load-a-large-file
(setq large-file-warning-threshold 1000000000)

;; ** measure how long it took to load .emacs
(progn (init:report-elapsed-time)
       (add-hook 'after-init-hook
		 `(lambda ()
		    (let ((elapsed (float-time (time-subtract (current-time)
							      emacs-start-time))))
		      (message "Loading %s...done (elapsed %.3fs) [after-init]"
			       ,load-file-name elapsed)))
		 t))
;; * the end
(put 'scroll-left 'disabled nil)
