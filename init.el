(message "Loading .emacs...")

;; * bootstrap el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; NOTE if you change it; update using M-x el-get-elpa-build-local-recipes
(unless (require 'el-get nil 'noerror)
  (require 'package)
  (setq package-archives
	(quote
	 (("gnu" . "https://elpa.gnu.org/packages/")
	  ("marmalade" . "https://marmalade-repo.org/packages/")
	  ("melpa" . "https://melpa.org/packages/")
	  ("elpy" . "https://jorgenschaefer.github.io/packages/"))))
  (package-refresh-contents)
  (package-install 'el-get)
  (package-initialize)
  (require 'el-get))

(setq el-get-allow-insecure 'nil)

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;;(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")

(el-get 'sync 'el-get)


(el-get-bundle hydra
  (setq hydra-look-for-remap t)

  (defhydra hydra-zoom (global-map "C-c")
    "zoom"
    ("+" text-scale-increase "in")
    ("-" text-scale-decrease "out"))

  ;; Movement
  (global-set-key
   (kbd "C-n")
   (defhydra hydra-move
     (:body-pre (next-line))
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

  ;; PDF Tools
  (eval-after-load "pdf-tools"
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
      ("\\" hydra-master/body "back")
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
      ("h" image-backward-hscroll :color red))))

(el-get-bundle pdf-tools
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
  (progn
    (define-key pdf-view-mode-map (kbd "\\") 'hydra-pdftools/body)
    (define-key pdf-view-mode-map (kbd "<s-spc>") 'pdf-view-scroll-down-or-next-page)
    (define-key pdf-view-mode-map (kbd "g")  'pdf-view-first-page)
    (define-key pdf-view-mode-map (kbd "G")  'pdf-view-last-page)
    (define-key pdf-view-mode-map (kbd "l")  'image-forward-hscroll)
    (define-key pdf-view-mode-map (kbd "h")  'image-backward-hscroll)
    (define-key pdf-view-mode-map (kbd "j")  'pdf-view-next-page)
    (define-key pdf-view-mode-map (kbd "k")  'pdf-view-previous-page)
    (define-key pdf-view-mode-map (kbd "e")  'pdf-view-goto-page)
    (define-key pdf-view-mode-map (kbd "u")  'pdf-view-revert-buffer)
    (define-key pdf-view-mode-map (kbd "al") 'pdf-annot-list-annotations)
    (define-key pdf-view-mode-map (kbd "ad") 'pdf-annot-delete)
    (define-key pdf-view-mode-map (kbd "aa") 'pdf-annot-attachment-dired)
    (define-key pdf-view-mode-map (kbd "am") 'pdf-annot-add-markup-annotation)
    (define-key pdf-view-mode-map (kbd "at") 'pdf-annot-add-text-annotation)
    (define-key pdf-view-mode-map (kbd "y")  'pdf-view-kill-ring-save)
    (define-key pdf-view-mode-map (kbd "i")  'pdf-misc-display-metadata)
    (define-key pdf-view-mode-map (kbd "s")  'pdf-occur)
    (define-key pdf-view-mode-map (kbd "b")  'pdf-view-set-slice-from-bounding-box)
    (define-key pdf-view-mode-map (kbd "r")  'pdf-view-reset-slice)))

(el-get-bundle! tdd
  :description "Run recompile (or a customisable function) after saving a buffer"
  :type github
  :pkgname "jorgenschaefer/emacs-tdd")

(el-get-bundle elpy
  (elpy-enable)
  (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
  (add-hook 'elpy-mode-hook 'hl-line-mode))

					; real-time syntax check
(el-get-bundle! flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

					; format and correct any PEP8 erros on save (C-x C-s)
(el-get-bundle! py-autopep8
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(el-get-bundle magit
  (global-set-key (kbd "C-c g") 'magit-status))

(el-get-bundle smex
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(el-get-bundle flymake
  (global-set-key [f3] 'flymake-display-err-menu-for-current-line)
  (global-set-key [f4] 'flymake-goto-next-error))

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
             (?r (file . ,(expand-file-name "~/private/org/notes.org")))
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
(global-set-key (kbd "C-x ^") 'join-line)

;; . Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; . Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

					; [[https://github.com/dimitri/emacs-kicker/blob/master/init.el]]
					; If you do use M-x term, you will notice there's line mode that acts like
					; emacs buffers, and there's the default char mode that will send your
					; input char-by-char, so that curses application see each of your key
					; strokes.
					;
					; The default way to toggle between them is C-c C-j and C-c C-k, let's
					; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)
					; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
					; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") 'term-paste)


					; use ido for minibuffer completion

(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
					; default key to switch buffer is C-x b, but that's not easy enough
					;
					; when you do that, to kill emacs either close its frame from the window
					; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
					; week (or day) action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)


					; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

					; ripgrep -- https://github.com/dajva/rg.el
(el-get-bundle rg
  (global-set-key (kbd "C-x C-r") 'rg))

(el-get-bundle multiple-cursors
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

(el-get-bundle which-key
  (which-key-mode))

(el-get-bundle markdown-mode
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))


(require 'el-get-elpa) ; install melpa packages via el-get
; Build the El-Get copy of the package.el packages if we have not
; built it before.  Will have to look into updating later ...
; M-x el-get-elpa-build-local-recipes
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

(setq my-packages
      (append
       ;; list of packages we use straight from official recipes
       '(el-get geiser helm dash deferred el-get elpy
		yasnippet pyvenv highlight-indentation find-file-in-project
		async epl flycheck let-alist package pkg-info fuzzy gh
		marshal ht request logito pcache gist tabulated-list git-modes
		google ein auto-complete popup cl-lib websocket helm-google
		ido-vertical-mode company company-restclient know-your-http-well
		restclient ac-geiser
		names json-mode json-snatcher json-reformat
		multiple-cursors rg py-autopep8 magit with-editor smex
		; for org-store-link
		skewer-mode)
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

;; https://github.com/dimitri/el-get/issues/2232
(el-get-ensure-byte-compilable-autoload-file el-get-autoload-file)
(el-get-cleanup my-packages)
(el-get 'sync my-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'saveplace)
(setq-default save-place t)

					; Configure gnus (mail lists, nntp news groups)
					; http://www.xsteve.at/prg/gnus/
(setq gnus-select-method '(nnimap "gmail"
				  (nnimap-address "imap.gmail.com")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)
				  (nnimap-authinfo-file "~/.authinfo.gpg")))
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq user-full-name "Akira Li")
(setq user-mail-address "4kir4.1i@gmail.com")

;; Replace [ and ] with _ in ADAPT file names for due to Gnus 5.13 bug
(setq nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)) )

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-authinfo-file "~/.authinfo.gpg"
      smtpmail-auth-credentials "~/.authinfo.gpg"
      starttls-use-gnutls t)

(eval-after-load "mail-source"
  '(add-to-list 'mail-sources '(file :path "/var/spool/mail/den")))

(setq read-mail-command 'gnus)
(setq message-send-mail-real-function 'smtpmail-send-it)
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")
(setq gnus-secondary-select-methods
      '((nnml "")
	(nntp "news.gmane.org"
	      (nntp-open-connection-function nntp-open-tls-stream)
	      (nntp-port-number 563))
	))
(setq nnmail-expiry-wait 35)
;;;;(setq mm-text-html-renderer 'w3m)
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
					;(gnus-killed-mark (from -1) (subject -3))))
					;(gnus-kill-file-mark (from -9999)))
					;(gnus-expirable-mark (from -1) (subject -1))
					;(gnus-ancient-mark (subject -1))
					;(gnus-low-score-mark (subject -1))
					;(gnus-catchup-mark (subject -1))))

(setq gnus-score-decay-constant 1)	;default = 3
(setq gnus-score-decay-scale 0.03)	;default = 0.05

(setq gnus-decay-scores t)		;(gnus-decay-score 1000)
(setq gnus-global-score-files
      '("~/gnus/scores/all.SCORE"))

;; all.SCORE contains:
;;(("xref"
;;  ("gmane.spam.detected" -1000 nil s)))
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
(add-hook 'message-mode-hook 'turn-on-auto-fill)
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(setq gnus-directory "~/gnus")
(setq message-directory "~/gnus/mail")
(setq nnml-directory "~/gnus/nnml-mail")
(setq gnus-article-save-directory "~/gnus/saved")
(setq gnus-kill-files-directory "~/gnus/scores")
(setq gnus-cache-directory "~/gnus/cache")

;;;;(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(require 'gnus-registry)
(gnus-registry-initialize)

(setq gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:\\|^User-Agent:\\|^X-Mailer:\\|^X-Newsreader:")

(setq gnus-sorted-header-list '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^Followup-To:" "^To:" "^Cc:" "^Date:" "^User-Agent:" "^X-Mailer:" "^X-Newsreader:"))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(define-key gnus-summary-mode-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
(define-key gnus-summary-mode-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))
(define-key gnus-summary-mode-map [(control down)] 'gnus-summary-next-thread)
(define-key gnus-summary-mode-map [(control up)] 'gnus-summary-prev-thread)
(setq spam-directory "~/gnus/spam/")

(setq gnus-spam-process-newsgroups
      '(("^gmane\\."
	 ((spam spam-use-gmane)))))

(require 'spam)

					; IRC client (*nix only)
(require 'erc)
					; . Make C-c RET (or C-c C-RET) send messages instead of RET.
(define-key erc-mode-map (kbd "RET") nil)
(define-key erc-mode-map (kbd "C-c RET") 'erc-send-current-line)
(define-key erc-mode-map (kbd "C-c C-RET") 'erc-send-current-line)
					; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
					; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
					; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)
					; . hide server messages
;;(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(setq erc-auto-query 'buffer) ; add a whois when someone pms

					; keep erc from eating ram by truncating chat logs
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)


					; .
(defun irc-seen_ ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "me" :port 6697
	   :nick "seen_"
	   :full-name "D M"
	   :password "seen_@erc/freenode:sweOwamAygIlcejIcVic"))

					; Notify when someone mentions my nick.
					; http://bbs.archlinux.org/viewtopic.php?id=40190
(defun erc-global-notify (matched-type nick msg)
  (interactive)
  (when (and (eq matched-type 'current-nick)
	     (not (string= (frame-parameter (selected-frame) 'name) "#python")))
    (shell-command
     (concat "notify-send -t 4000 -c \"im.received\" \""
	     (car (split-string nick "!"))
	     " mentioned your nick\" \""
	     msg
	     "\""))))
(add-hook 'erc-text-matched-hook 'erc-global-notify)

					; * https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks t)


(add-hook 'after-init-hook 'global-company-mode)

(setq gist-view-gist t)

					; suppress Warning (mule): Invalid coding system `ascii' is specified
(define-coding-system-alias 'ascii 'us-ascii)

					; run compile command on F5, change command with C-u F5
(global-set-key [f5] 'compile)


(setq compilation-ask-about-save nil)
(setq compilation-read-command nil)

					; use 'y'/'n' instead of 'yes'/'no'
(fset 'yes-or-no-p 'y-or-n-p)

					; whenever an external process changes a file underneath emacs, and there
					; was no unsaved changes in the corresponding buffer, just revert its
					; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; enable line numbers globally
(global-linum-mode t)

					;
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
(defun my-c-mode-common-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

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

;; w3m setup
;; from http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html
;;change default browser for 'browse-url' to w3m
;;;;(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

(global-set-key (kbd "C-c C-f") 'ido-recentf-open)
(setq flyspell-default-dictionary "ru")


(require 'org)
					; Set to the location of your Org files on your local system
(setq org-directory "~/private/org")
					; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/private/org/from-mobile.org")
					; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-confirm-babel-evaluate nil)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

					; enable export to markdown in on C-c C-e
(eval-after-load "org"
  '(require 'ox-md nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq canlock-password "58c566bd78ee9cbec1af9280ae463f8a0df16fbc"
      column-number-mode t
      elpy-mode-hook (quote (flycheck-mode hl-line-mode))
      inhibit-startup-screen t
      nand2tetris-core-base-dir "~/prj/coursera/nand2tetris"
      org-agenda-files (quote ("~/private/org/notes.org"))
      org-agenda-include-diary t
      org-babel-load-languages (quote ((python . t) (shell . t)))
      safe-local-variable-values
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
        (require-final-newline . t)))
      show-paren-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(message "Done .emacs")
;; end
