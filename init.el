; -*- coding: utf-8 lexical-binding: t orgstruct-heading-prefix-regexp: ";; *"; -*-
;; * prelude
(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; ** https://www.reddit.com/r/emacs/comments/ct0h6m/this_nobbled_me_yesterday_on_debian_10_it_needs/
(when (and (>= libgnutls-version 30603)
           (version<= emacs-major-version "26.2"))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(defun init:report-elapsed-time (&optional what-has-loaded)
  "Report time elapsed since emacs start."
  (or what-has-loaded (setq what-has-loaded load-file-name))
  (let ((elapsed (float-time (time-subtract (current-time)
					      emacs-start-time))))
    (message "Loading %s...done (elapsed %.3fs)" what-has-loaded elapsed)))

;; * load customizations
(setq custom-file "~/.custom.el")
(load custom-file)

;; * bootstrap el-get
(add-to-list 'load-path  (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get-user/recipes"))

;; * install packages
(setq load-prefer-newer t) ; suppress warning about .autoloads.el files
(el-get-bundle! with-eval-after-load-feature) ; to suppress "free variable" warning
(el-get-bundle! use-package)
(with-eval-after-load 'use-package
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
  (setq use-package-verbose t)
					; M-x use-package-report to see the results
  (setq use-package-compute-statistics t))

;; ** Hydra
(el-get-bundle hydra-move-splitter
  :url "https://raw.githubusercontent.com/erreina/dotfiles/a8a19b3a466720ba345e68b71449191644987276/emacs/elisp/hydra-move-splitter.el")

(el-get-bundle hydra)
(with-eval-after-load-feature 'hydra ; fix "free variable warning"
  (setq hydra-look-for-remap t))

(el-get-bundle ace-window)
(with-eval-after-load-feature 'ace-window
  (setq aw-background t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (add-to-list 'aw-dispatch-alist '(?\\ hydra-window/body) t))

;; *** PDF Tools https://github.com/abo-abo/hydra/wiki/PDF-Tools
(el-get-bundle pdf-tools
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

;; ** highlight jira formatted files
(el-get-bundle jira-markup-mode
  :description "Emacs major mode for JIRA-markup-formatted text files"
  :type github
  :pkgname "mnuessler/jira-markup-mode")

;; ** Support for hacking python code
(el-get-bundle tdd
  :description "Run recompile (or a customisable function) after saving a buffer"
  :type github
  :pkgname "jorgenschaefer/emacs-tdd")

                    ; format Python code on save (C-x C-s)
(el-get-bundle blacken
  :description "Reformat python buffers using the 'black' formatter"
  :type github
  :pkgname "proofit404/blacken")

(el-get-bundle reformatter)
(el-get-bundle python-black
  :description "Emacs package to reformat Python using black-macchiato"
  :type github
  :pkgname "wbolster/emacs-python-black")

;; ** misc
(el-get-bundle multiple-cursors ;; see also hydra-multiple-cursors
  (global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click))

(el-get-bundle markdown-mode
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(el-get-bundle sqlite-dump
  :url "https://download.tuxfamily.org/user42/sqlite-dump.el")

;; ** insecure
(setq el-get-allow-insecure t)
(el-get-bundle geiser)
(el-get-bundle sr-speedbar)
(setq el-get-allow-insecure nil)

(with-eval-after-load-feature 'geiser
  (setq geiser-active-implementations '(racket)))
(el-get-bundle paredit)

(el-get-bundle rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; ** jump to visible input (use keyboard as a mouse)
(el-get-bundle avy
  (global-set-key (kbd "M-s") 'avy-goto-word-1)
  (global-set-key (kbd "s-j") 'avy-goto-char-timer)
  (global-set-key (kbd "s-J") 'avy-pop-mark)
  (avy-setup-default))
(with-eval-after-load-feature 'avy
    (setq avy-background t))

; *** jump to link in info, eww buffers: type O + appeared avy letters
(el-get-bundle ace-link)


(el-get-bundle typing)

;; ** company
(el-get-bundle company
  (global-company-mode))
(with-eval-after-load-feature 'company
  (add-to-list 'company-backends 'company-restclient))

;; ** Increase selection by semantic units
(el-get-bundle expand-region
  (global-set-key (kbd "C-=") 'er/expand-region))

;; ** restclient
(el-get-bundle restclient)
; NOTE: avoid "recursive load" error from el-get
(el-get-bundle company-restclient)

; for ivy-regex-fuzzy sorting of large lists
(el-get-bundle flx)

;; *** ido/ivy/helm imenu tag selection across buffers with the same mode/project etc
(el-get-bundle imenu-anywhere)

;; ** web-mode
(el-get-bundle web-mode)
;; ** yaml-mode
(el-get-bundle yaml-mode)
;; ** try
(el-get-bundle try
  :url "https://raw.githubusercontent.com/larstvei/Try/master/try.el")

;; ** idle-highlight
(el-get-bundle idle-highlight-mode)
;; ** ^^^last el-get-bundle installed package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; no new el-get packages from here on out
(setq my:el-get-packages
       ;; list of packages we use straight from official recipes
       (mapcar #'el-get-as-symbol (mapcar #'el-get-source-name el-get-sources)))


;; https://github.com/dimitri/el-get/issues/2232
(el-get-ensure-byte-compilable-autoload-file el-get-autoload-file)
(el-get-cleanup my:el-get-packages) ; uninstall packages that are not mentioned
(el-get 'sync my:el-get-packages)
(init:report-elapsed-time "el-get-packages")

;; * configure packages
(package-initialize)
(init:report-elapsed-time "package-initialize")

(defun init:with-secrets (orig-fun &rest args)
  "Load secrets before calling `orig-fun'."
  (require '.secrets "~/.secrets.el.gpg")
  (apply orig-fun args))

;; ** delight: remove modes from ModeLine
;; C-h v minor-mode-alist
(use-package delight
  :commands delight)

;; ** which-key: show commands for the current prefix after a delay
(use-package which-key

  :config
  (which-key-mode))

;; ** theme-changer: use dark theme after sunset
(use-package theme-changer

  :custom
  (calendar-latitude 55.8) ; for solar package
  (calendar-longitude 37.6)
  :init
  ; reset old theme settings while loading a new theme
  (defadvice load-theme
      (before disable-before-load (theme &optional no-confirm no-enable) activate)
    (mapc 'disable-theme custom-enabled-themes))
  :config
  (change-theme 'tango 'tango-dark))

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
  )
(use-package swiper
  )
;; *** amx -- M-x smex fork
(use-package amx
  )  ; used by counsel-M-x
(use-package request
  )  ; used by counsel-search
(use-package counsel
  :delight
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-c s" . counsel-search)
	 ("M-x" . counsel-M-x) ; show keybindings
	 ("<f5>" . counsel-compile)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h a" . counsel-apropos))
  :init
					; https://oremacs.com/2017/08/04/ripgrep/
  (setq counsel-grep-base-command
	"rg -i -M 120 --no-heading --line-number --color never -e %s %s")
  :config
  ;; Enabling counsel-mode remaps built-in Emacs functions that have counsel replacements
  (counsel-mode 1)
  )

;; counsel-dash
;; *** counsel-dash
(use-package counsel-dash

  :bind ("C-c d" . counsel-dash)

  ;; Note: can't use :config here -- too late for the hook to run then the
  ;; keys are invoked (causing the config) in the corresponding buffer
  :init
  (progn
    (defun python3-doc ()
      (interactive)
      (setq-local counsel-dash-docsets
		  '("Python 3" "NumPy")))
    (add-hook 'python-mode-hook 'python3-doc)

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
    (add-hook 'sh-mode-hook 'bash-doc)))

;; ** ripgrep
;;; type `e' in rg-mode to edit search results, `C-x C-s' to save them
(use-package wgrep
  ;; save buffers automatically before finishing wgrep mode
  :custom
  (wgrep-auto-save-buffer t))
(use-package rg
  :bind ("C-x C-r" . rg))

;; ** ace-link
(use-package ace-link
  :ensure nil
  :config
  (require 'info)
  (ace-link-setup-default))

;; ** magit
(use-package magit
  :bind ("C-c M-g" . magit-file-dispatch)
  :bind ("C-c g" . magit-status)
  :custom
  (magit-log-section-commit-count 20)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package forge

  :defer t
  :after magit)

;; ***
(use-package git-gutter

  :delight
  :custom
  (git-gutter:window-width 2)
  (git-gutter:modified-sign "☁")
  (git-gutter:added-sign "☀")
  (git-gutter:deleted-sign "☂")
  :config
  (global-git-gutter-mode t))

;; ***
(use-package git-timemachine
  :defer t
  )

;; ** python debugger
; realgud has to be installed manually to avoid https://github.com/realgud/realgud/issues/77

;; ** real-time syntax check
(use-package flycheck
  :defer t
  )

(use-package sh-mode
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


;; ** elpy (python)
(use-package elpy
  :commands elpy-enable flycheck-mode

  :init (with-eval-after-load 'python (elpy-enable))
  :config
  ;; flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook #'flycheck-mode))

(use-package blacken
  :ensure nil
  :custom
  ; use fill-column as the Maximum Line Length
  (blacken-line-length 'fill))

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

;; ** typescript
(use-package typescript

  :defer t)
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

;; *** https://github.com/abo-abo/hydra/wiki/Compilation
  (defhydra hydra-next-error
    (global-map "C-x")
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
                   ; Navigate windows with S-<arrows>
(windmove-default-keybindings 'super)
(customize-set-variable 'windmove-wrap-around t)

;;
(customize-set-variable 'confirm-kill-processes nil)

;; https://github.com/erreina/.emacs.d/blob/master/init.d/init-keybindings.el
(require 'hydra-move-splitter)
(with-eval-after-load-feature (ivy counsel)
(global-set-key (kbd "C-c w") (defhydra hydra-window (:hint nil)
  "
Movement^^        ^Split^         ^Switch^      ^Resize^
----------------------------------------------------------------
_j_ ←          _v_ertical       _b_uffer         _J_ X←
_k_ ↓          _h_ horizontal   _f_ind files     _K_ X↓
_i_ ↑          _u_ undo         _a_ce 1          _I_ X↑
_k_ →         _r_ reset        _s_ave           _L_ X→
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
		   'hydra-window/body)))
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
(dolist (r `((?i (file . ,(expand-file-name "~/.emacs")))
             (?r (file . ,(expand-file-name (getenv "ORG_AGENDA_FILE"))))))
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

; don't ask which shell to use
(defvar init:term-shell "/bin/zsh")
(defadvice ansi-term (before init:force-zsh)
  (interactive (list init:term-shell)))
(ad-activate 'ansi-term)

; kill the buffer when the shell exits
; from http://echosa.github.io/blog/2012/06/06/improving-ansi-term/
(defadvice term-sentinel (around init:advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(define-key term-raw-map  (kbd "C-'") #'term-line-mode)
(define-key term-mode-map (kbd "C-'") #'term-char-mode)
                    ; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
                    ; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") #'term-paste)


(global-set-key (kbd "C-x C-c") #'save-buffers-kill-emacs)

                    ; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x))

;; *** insert subdirectory directly below its line
(use-package dired-subtree

  :bind (:map dired-mode-map ("i" . dired-subtree-insert)))

(require '.secrets "~/.secrets.el.gpg")

;; ** Save point position between sessions
(require 'saveplace)
(setq-default save-place t) ; set global default value for buffer local variable
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; ** gnus
(with-eval-after-load "gnus"
  (advice-add 'gnus :around #'init:with-secrets)

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
(advice-add 'irc-start :around #'init:with-secrets)

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


;; don't edit non programming files such as images (png, jpg, etc)
;; https://stackoverflow.com/questions/6138029/how-to-add-a-hook-to-only-run-in-a-particular-mode
(add-hook 'prog-mode-hook
	  #'(lambda ()
	      (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

                    ; copy/kill line on M-w, C-w
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

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

                    ; список используемых нами словарей
                    ; from https://habrahabr.ru/post/215055/
(setq ispell-local-dictionary-alist
      '(("russian"
     "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
     "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
     "[-]"  nil ("-d" "ru_RU") nil utf-8)
    ("english"
     "[A-Za-z]" "[^A-Za-z]"
     "[']"  nil ("-d" "en_US") nil iso-8859-1)))
;; ** flyspell
(use-package flyspell
  :ensure nil
  :defer t
  :delight
  :hook (prog-mode . flyspell-prog-mode))

;; ** configure org
(use-package org
  :ensure nil
  :commands turn-on-orgstruct++
  :custom
  (org-src-preserve-indenhtation nil "leading whitespace blocks are stripped")
  (org-edit-src-content-indentation 0 " and the code block is not indented (0!)")
  (org-src-tab-acts-natively t "make TAB insert spaces in python src blocks")
  (org-export-use-babel nil "disable evaluation of babel code blocks on export")
  (org-log-into-drawer t "hide State DONE. Useful for repeating tasks")
  (org-export-backends '(md odt latex icalendar html ascii) "List of export back-ends that should be always available.")
  (org-modules '(org-habit))
  (org-refile-targets  '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 2)))

  :init
  ;; export to the kill ring automatically for interactive exports
  (setq org-export-copy-to-kill-ring 'if-interactive)
  ;; orgmobile
  (setq org-mobile-use-encryption t)
  (require '.secrets "~/.secrets.el.gpg")

  (setq org-todo-keywords
	'((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d!)" "DEFERRED(e)" "CANCELED(c)")))

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



  ;; From https://github.com/higham/dot-emacs/blob/master/.emacs
  ;; fold with Tab/S-Tab on headers in org-mode
  (add-hook 'emacs-lisp-mode-hook #'turn-on-orgstruct++)

					; Set to the location of your Org files on your local system
  (setq org-directory (getenv "ORG_DIRECTORY"))
					; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull (getenv "ORG_MOBILE_INBOX_FOR_PULL"))
					; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory (getenv "ORG_MOBILE_DIRECTORY"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "inbox.org" "Tasks")
	   "* TODO %?\n  %i\n  %a" :clock-in t :clock-resume t)
	  ("j" "Journal" entry (file+datetree "journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")))

  ;; start agenda view relative to current day, show given number of days
  (setq org-agenda-span 2
	org-agenda-start-on-weekday nil
	org-agenda-start-day "-1d")

  ;; if non-nill, only show habits in today’s agenda view
  (setq org-habit-show-habits-only-for-today nil)

  ;; shift the org habit graph in the agenda further out right so as
  ;; to leave enough room for the headings to be visible.
  (setq org-habit-graph-column 90)
  (setq org-confirm-babel-evaluate nil)
  (setq org-agenda-custom-commands
        '(("n" agenda "+notes")
          ("w" agenda "-notes")))
  :bind (("C-c l" . org-store-link)
	 ("C-c o" . org-open-at-point-global)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (define-key org-mode-map (kbd "M-o") 'ace-link-org)

  ;; resume the clock under the assumption that you have worked on this task while outside Emacs
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-reverse-note-order t)
  (setq org-agenda-include-diary t)

  (setq org-plantuml-jar-path
      (expand-file-name "~/src/plantuml/plantuml.jar"))

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
  :defer 10
  :config
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia")))

(use-package jupyter
  :after org
  :defer 10
  :config
  ;; org src blocks languages
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)
							   (emacs-lisp . t)
							   (shell . t) ; https://emacs.stackexchange.com/questions/37692/how-to-fix-symbols-function-definition-is-void-org-babel-get-header
							   (plantuml . t) ; http://eschulte.github.io/babel-dev/DONE-integrate-plantuml-support.html
							   (jupyter . t))) ; must be last https://github.com/dzop/emacs-jupyter#org-mode-source-blocks
  ;; default args for jupyter-python
  (setq org-babel-default-header-args:jupyter-python
	'((:results .
		    "replace")
	  (:async .
		  "yes")
	  (:session .
		    "py")
	  (:kernel .
		   "python3"))))

(use-package org-bullets
  :after org

  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ** recursive directory tree comparison: M-x ztree-diff
(use-package ztree
  :defer t
  ) ; needs GNU diff utility

;; ** edit browser text area in Emacs (sync both ways)
(use-package atomic-chrome
  ;; dependency Atomic Chrome extension (in Chrome)
  :defer 10
  :init
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-extension-type-list '(atomic-chrome))
  :config
  (atomic-chrome-start-server))

;; ** fzf
(use-package fzf
  )

;; ** groovy
(use-package groovy-mode
  :defer t
  )

;; ** direnv
(use-package direnv

  :config
  (direnv-mode))

;; ** python black
(use-package python-black
  :ensure nil
  :demand t
  :after python
  :custom
  (python-black-extra-args '("-l" "78")))

;; ** C-x C-f /docker:user@container:/path/to/file
(use-package docker-tramp
  :defer t
  )

;; ** M-x elisp-format-region
(use-package elisp-format
  :defer 100
  )

;; ** google-this
(use-package google-this
  :defer t
  ;;  note: google-this-mode enables "C-c /" map
  :custom
  (google-this-location-suffix "com" "(com, co.uk, fr, etc)")
  (google-this-mode t)
  :config
  ;; override google-this-url to force English
  (defun google-this-url ()
    "URL for google searches."
    (concat google-this-base-url google-this-location-suffix "/search?hl=en&ion=1&q=%s")))

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
  :delight)
;; * ^^^last non-core use-package
(init:report-elapsed-time "use-package")

;; ** core emacs settings
(use-package emacs
  :custom
  (indent-tabs-mode nil)
  :config
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it)))


;; ** nand2tetris
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
(show-paren-mode)

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

(setq ad-redefinition-action 'accept) ;; suppress "got redefined" warning

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
