; -*- coding: utf-8 orgstruct-heading-prefix-regexp: ";; *"; -*-

(message "Loading .emacs...")

;; * helper function
(defun init:disable-linum-mode-in-local-buffer ()
  (linum-mode -1)) ;; an alternative is to define #'linum-on


;; * bootstrap el-get
(setq load-prefer-newer t) ; suppress warning about .autoloads.el files
(add-to-list 'load-path  (concat user-emacs-directory "el-get/el-get"))

;; NOTE if you change it; update using M-x el-get-elpa-build-local-recipes
(unless (require 'el-get nil 'noerror)
  (require 'package)
  (setq package-archives
    (quote
     (("gnu" . "https://elpa.gnu.org/packages/")
      ("melpa" . "https://melpa.org/packages/")
      ("elpy" . "https://jorgenschaefer.github.io/packages/"))))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'el-get)
  (require 'el-get))
(el-get 'sync 'el-get) ; workaround "can't find \"package\" package"


;; * install & configure packages
(setq el-get-allow-insecure 'nil)
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get-user/recipes"))


(el-get-bundle! with-eval-after-load-feature) ; to suppress "free variable" warning

(el-get-bundle gist)
(with-eval-after-load-feature 'gist
  (setq gist-view-gist t)) ; suppress "free variable" warning

(el-get-bundle company
  (add-hook 'after-init-hook #'global-company-mode))

(el-get-bundle hydra
  (defhydra hydra-zoom (global-map "C-c")
    "zoom"
    ("+" text-scale-increase "in")
    ("-" text-scale-decrease "out"))

  ;; Movement
  (global-set-key (kbd "C-n")  (defhydra hydra-move
     (:body-pre (forward-line))
     "move"
     ("n" next-line)
     ("p" previous-line)
     ("f" forward-char)
     ("b" backward-char)
     ("a" beginning-of-line)
     ("e" move-end-of-line)
     ("v" scroll-up-command)
     ;; Converting M-v to V here by analogy.
     ("V" scroll-down-command)
     ("l" recenter-top-bottom)))

  (defhydra hydra-multiple-cursors (:hint nil)
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
    ("C-e" mc/edit-ends-of-lines :exit t))

  ;; https://github.com/abo-abo/hydra/wiki/Dired
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
    ("i" dired-maybe-insert-subdir)
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
       ("q" nil)))))
(with-eval-after-load-feature 'hydra ; fix "free variable warning"
  (setq hydra-look-for-remap t))

(el-get-bundle ace-window)
(with-eval-after-load-feature 'ace-window
  (setq aw-background t)
  (defhydra hydra-window-stuff (:hint nil)
    "
          Split: _v_ert  _s_:horz
         Delete: _c_lose  _o_nly
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file  _F_projectile
         Winner: _u_ndo  _r_edo
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
           Move: _a_:up  _z_:down  _i_menu"


    ("z" scroll-up-line)
    ("a" scroll-down-line)
    ("i" idomenu)

    ("u" winner-undo)
    ("r" winner-redo)

    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" ido-switch-buffer)
    ("f" ido-find-file)
    ("F" projectile-find-file)

    ("s" split-window-below)
    ("v" split-window-right)

    ("c" delete-window)
    ("o" delete-other-windows)

    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)

    ("q" nil))
  (add-to-list 'aw-dispatch-alist '(?\\ hydra-window-stuff/body) t))

;; PDF Tools https://github.com/abo-abo/hydra/wiki/PDF-Tools
(el-get-bundle pdf-tools
  (pdf-tools-install))
(with-eval-after-load-feature 'pdf-tools
  (setq-default pdf-view-display-size #'fit-page)
  (add-hook 'pdf-view-mode-hook #'init:disable-linum-mode-in-local-buffer)
   (defhydra hydra-pdftools (:color blue :hint nil)
    "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
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

(el-get-bundle tdd
  :description "Run recompile (or a customisable function) after saving a buffer"
  :type github
  :pkgname "jorgenschaefer/emacs-tdd")

(el-get-bundle elpy
  (elpy-enable)
  (el-get-bundle pyvenv
    (add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python)))

                    ; real-time syntax check
(el-get-bundle flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook #'flycheck-mode))

                    ; format and correct any PEP8 erros on save (C-x C-s)
(el-get-bundle py-autopep8
  (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

(el-get-bundle magit
  (global-set-key (kbd "C-c g") #'magit-status))

(el-get-bundle smex
  (global-set-key (kbd "M-x") #'smex)
  (global-set-key (kbd "M-X") #'smex-major-mode-commands))

                    ; Navigate windows with S-<arrows>
(windmove-default-keybindings 'super)
(setq windmove-wrap-around t)

                    ; uniquify buffers with the same name
                    ; instead of buf<2>, etc it shows
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

                    ; enable recent files menu

(recentf-mode t)
(setq recentf-max-saved-items 100)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
;; http://emacs.stackexchange.com/questions/3063/recently-opened-files-in-ido-mode
(setq ido-use-virtual-buffers t)


                    ; use 4 spaces instead of tabs for indentation
                    ; http://stackoverflow.com/a/471916/
(setq tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
(setq indent-tabs-mode nil)


;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Part of the Emacs Starter Kit
;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el) to jump to it.
;; You should add registers here for the files you edit most often.
(dolist (r `((?i (file . ,(expand-file-name "~/.emacs")))
             (?r (file . ,(expand-file-name (getenv "ORG_AGENDA_FILE"))))
	     (?c (file . ,(expand-file-name "~/.custom.el")))
	     ))
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

;; . Use regex searches by default.
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "\C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-M-r") #'isearch-backward)

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
(defadvice ansi-term (before force-bash)
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


(add-hook 'term-mode-hook #'init:disable-linum-mode-in-local-buffer)
(define-key term-raw-map  (kbd "C-'") #'term-line-mode)
(define-key term-mode-map (kbd "C-'") #'term-char-mode)
                    ; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
                    ; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") #'term-paste)


                    ; use ido for minibuffer completion

(ido-mode t)
(setq ido-save-directory-list-file (concat user-emacs-directory ".ido.last"))
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
                    ; default key to switch buffer is C-x b, but that's not easy enough
                    ;
                    ; when you do that, to kill emacs either close its frame from the window
                    ; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
                    ; week (or day) action.
(global-set-key (kbd "C-x C-b") #'ido-switch-buffer)
(global-set-key (kbd "C-x C-c") #'save-buffers-kill-emacs)
(global-set-key (kbd "C-x B") #'ibuffer)


                    ; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

                    ; ripgrep -- https://github.com/dajva/rg.el
(el-get-bundle multiple-cursors
  (global-set-key (kbd "C-c m") #'hydra-multiple-cursors/body)
  (global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click))

(el-get-bundle which-key
  ;; show commands for the current prefix after a delay
  (which-key-mode))

(el-get-bundle helm
  ;; Help should search more than just commands
  (global-set-key (kbd "C-h a") #'helm-apropos)
  (customize-set-variable 'helm-allow-mouse t))

(el-get-bundle helm-google
  ;; If you want to keep the search open use C-z instead of RET.
  (global-set-key (kbd "C-h C--") #'helm-google)
  (setq browse-url-browser-function #'eww-browse-url)
  (setq url-user-agent "User-Agent: Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0 like Mac OS X; en-us) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A293 Safari/6531.22.7\r\n")
  (add-hook 'eww-mode-hook #'init:disable-linum-mode-in-local-buffer))

(el-get-bundle markdown-mode
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

;; ripgrep
(el-get-bundle rg
  (global-set-key (kbd "C-x C-r") #'rg))

(el-get-bundle avy
  (global-set-key (kbd "M-s") 'avy-goto-word-1)
  (avy-setup-default))

(require 'el-get-elpa) ; install melpa packages via el-get
; Build the El-Get copy of the package.el packages if we have not
; built it before.  Will have to look into updating later ...
; M-x el-get-elpa-build-local-recipes
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

(setq my:el-get-packages
      (append
       ;; list of packages we use straight from official recipes
       '(yasnippet		    ; interactive templates
	 skewer-mode)		    ;  org-store-link fails without it
       (mapcar #'el-get-as-symbol (mapcar #'el-get-source-name el-get-sources))))


;; https://github.com/dimitri/el-get/issues/2232
(el-get-ensure-byte-compilable-autoload-file el-get-autoload-file)
(el-get-cleanup my:el-get-packages)
(el-get 'sync my:el-get-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; no new packages from here on out

(defadvice load-theme
    (before disable-before-load (theme &optional no-confirm no-enable) activate)
  (mapc 'disable-theme custom-enabled-themes))

(load-theme 'tango-dark)

;; * configure builtin packages
(defun init:with-secrets (orig-fun &rest args)
  "Load secrets before calling `orig-fun'."
  (require '.secrets "~/.secrets.el.gpg")
  (apply orig-fun args))

;; return to a previous window configuration easily with C-c <left>
(require 'winner)
(winner-mode)


(require 'saveplace)
(setq-default save-place t) ; set global default value for buffer local variable

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

                    ; * https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks t)


                    ; suppress Warning (mule): Invalid coding system `ascii' is specified
(define-coding-system-alias 'ascii 'us-ascii)

                    ; run compile command on F5, change command with C-u F5
(global-set-key [f5] #'compile)


(setq compilation-ask-about-save nil)
(setq compilation-read-command nil)

                    ; use 'y'/'n' instead of 'yes'/'no'
(fset 'yes-or-no-p 'y-or-n-p)

                    ; whenever an external process changes a file underneath emacs, and there
                    ; was no unsaved changes in the corresponding buffer, just revert its
                    ; content to reflect what's on-disk.
(global-auto-revert-mode 1)

(global-linum-mode)    ; enable line numbers globally if #'linum-on would do it
(setq linum-eager nil) ; improve performance
(column-number-mode)   ; enable columns numbers globally

(add-hook 'before-save-hook #'delete-trailing-whitespace)

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
 version-control t)       ; use versioned backups

                    ; show column number
(column-number-mode t) ;;NOTE: it has a performance hit

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
           (c-basic-offset . 2)
           ))
(defun init:c-mode-common-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c-mode-common-hook #'init:c-mode-common-hook)

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

(global-set-key (kbd "C-c C-f") #'ido-recentf-open)
(setq flyspell-default-dictionary "ru")


(require 'org)

;; orgmobile
(setq org-mobile-use-encryption t)
(advice-add 'org-mobile-push :around #'init:with-secrets)
(advice-add 'org-mobile-pull :around #'init:with-secrets)


(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
	(sequence "|" "CANCELED(c)")))

(setq org-tag-alist '((:startgroup . nil) ("home" . ?h) ("work" . ?w) (:endgroup . nil)
		      ("idea" . nil) ("code" . nil) ("pythonista" . nil) ("org" . nil)
		      ("so" . nil) ("birthday" . nil) ("buy" . nil) ("quick" . nil)
		      ("tutorial" . nil) ("github" . nil) ("day" . nil) ("pyopenssl" . nil)
		      ("iprim" . nil) ("cluster" . nil) ("read" . nil) ("book" . nil)
		      ("feature" . nil) ("psutil" . nil) ("twisted" . nil) ("cpython" . nil)
		      ("subprocess" . nil) ("bug" . nil) ("easy" . nil) ("stackoverflow" . ?s)
		      ("wurlitzer" . nil) ("lrange" . nil) ("pythinsta" . nil) ("telegram" . nil)
		      ("d0" . ?d) ("telethon" . nil) ("emacs" . ?e)))

;; Effort and global properties
(setq org-global-properties '(("Effort_ALL". "0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))

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
(customize-set-variable 'org-agenda-files (list
					   (getenv "ORG_AGENDA_FILE")
					   (getenv "ORG_MOBILE_INBOX_FOR_PULL")))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (getenv "ORG_AGENDA_FILE") "Projects")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree (concat org-directory "journal.org"))
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-confirm-babel-evaluate nil)
(global-set-key "\C-cl" #'org-store-link)
(global-set-key "\C-ca" #'org-agenda)
(global-set-key "\C-cc" #'org-capture)
(global-set-key "\C-cb" #'org-iswitchb)

;; resume the clock under the assumption that you have worked on this task while outside Emacs
(setq org-clock-persist t)
(org-clock-persistence-insinuate)
(setq org-reverse-note-order t)
(setq org-agenda-include-diary t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (sh . t)))

;; enable export to markdown in on C-c C-e
(require 'ox-md nil t)

;; drastically improve performance of org-capture for large org files
(add-hook 'org-mode-hook #'init:disable-linum-mode-in-local-buffer)


;;
(setq nand2tetris-core-base-dir (getenv "NAND2TETRIS-CORE-BASE-DIR"))
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

;; https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
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

(setq custom-file "~/.custom.el")
(load custom-file 'noerror)

(message "Done .emacs")
;; end
