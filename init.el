(message "Loading .emacs...")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; if you change it; update using M-x el-get-elpa-build-local-recipes
(setq package-archives
 (quote
  (("marmalade" . "https://marmalade-repo.org/packages/")
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
   ("elpy" . "https://jorgenschaefer.github.io/packages/")
   ("org" . "http://orgmode.org/elpa/"))))


(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
;;

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;;;;(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")

;; https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks t)


;; org-mode setup
;; from https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "~/.virtualenvs/certifi/bin/python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile))))

;; from https://github.com/nicferrier/elmarmalade/issues/55#issuecomment-166271364
(if (fboundp 'gnutls-available-p)
    (fmakunbound 'gnutls-available-p))
(setq tls-program '("gnutls-cli --tofu -p %p %h")
      imap-ssl-program '("gnutls-cli --tofu -p %p %s")
      smtpmail-stream-type 'starttls
      starttls-extra-arguments '("--tofu")
      )
(setq tls-checktrust t)

(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load

(package-initialize)                ;; Initialize & Install Package

(require 'el-get-elpa) ;; install melpa packages via el-get
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
;; M-x el-get-elpa-build-local-recipes
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

;;
;; my packages
(setq my-packages
      (append
       ;; list of packages we use straight from official recipes
       '(ac-geiser auto-complete cl-generic cl-lib color-theme
       color-theme-twilight company company-mode
       company-restclient dash deferred ein el-get elpy
       emacs-async epl find-file-in-project flycheck fuzzy geiser
       gh gist git-modes google helm helm-google highlight-80+
       highlight-indentation ht ido-vertical-mode json-mode
       json-reformat json-snatcher know-your-http-well let-alist
       logito magit markdown-mode marshal multiple-cursors names
       org-plus-contrib package pcache pkg-info popup py-autopep8
       pyvenv realgud request restclient rg s seq sigbegone
       skewer-mode smex tabulated-list tdd websocket with-editor
       yasnippet)
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

;; run recompile on save
(require 'tdd)

;; https://github.com/dimitri/el-get/issues/2232
(el-get-ensure-byte-compilable-autoload-file el-get-autoload-file)
(el-get 'sync my-packages)

;;
(add-hook 'after-init-hook 'global-company-mode)

;;
(require 'ein)
(setq ein:console-args '("--profile" "default"))
(setq ein:console-security-dir "~/tmp")

(elpy-enable)
(add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
(add-hook 'elpy-mode-hook 'hl-line-mode)

;;
;; real-time syntax check
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; format and correct any PEP8 erros on save (C-x C-s)
(when (require 'py-autopep8 nil t)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;;
(setq gist-view-gist t)

;;
(global-set-key (kbd "C-c g") 'magit-status)

;;
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; user specific configuration
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; HELPER code (XXX lexical scoping for custom-basedir)
(setq custom-basedir (expand-file-name "~/.emacs.d/"))

;; 2015-07-04 bug of pasting in emacs.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16737#17
;; http://ergoemacs.org/misc/emacs_bug_cant_paste_2015.html
(setq x-selection-timeout 300)

(defun my-add-path (p)
  (add-to-list 'load-path (concat custom-basedir p)))

(global-set-key (kbd "C-c C-f") 'ido-recentf-open)

;; suppress Warning (mule): Invalid coding system `ascii' is specified
(define-coding-system-alias 'ascii 'us-ascii)

;; flymake
(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)

;; flyspell
(setq flyspell-default-dictionary "ru") ;;XXX test (ispell-change-dictionary "ru")

;; run compile command on F5, change command with C-u F5
(global-set-key [f5] 'compile)

;;
(setq compilation-ask-about-save nil)
(setq compilation-read-command nil)

;; [http://www.jwz.org/doc/x-cut-and-paste.html]
(setq x-select-enable-clipboard t)
;; fixes hanging on paste from System Clipboard in Emacs 24 on Ubuntu 14.04
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; use 'y'/'n' instead of 'yes'/'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Navigate windows with S-<arrows>
(windmove-default-keybindings 'super)
(setq windmove-wrap-around t)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; gist.el open browser with created gist
;;;;(setq gist-view-gist t)

;; enable line numbers globally
(global-linum-mode t)

;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; copy/kill line on M-w, C-w
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

;; http://www.emacswiki.org/emacs/BackupDirectory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; show column number
(column-number-mode t) ;;NOTE: it has a performance hit

;; uniquify buffers with the same name
;; instead of buf<2>, etc it shows
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; enable recent files menu
(require 'recentf)
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


;; use 4 spaces instead of tabs for indentation
;; http://stackoverflow.com/a/471916/
(setq tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
(setq indent-tabs-mode nil)
;;;;(highlight-tabs)
;;;;(highlight-trailing-whitespace)


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

;; ;; Make C-c C-c behave like C-u C-c C-c in Python mode
;; ;; always pass ARG when run the functions interactively regardless
;; ;; of key bindings
;; (require 'python)
;; (define-key python-mode-map [remap python-shell-send-buffer]
;;   (lambda () (interactive) (python-shell-send-buffer t)))
;; (require 'elpy)
;; (define-key elpy-mode-map [remap elpy-shell-send-region-or-buffer]
;;   (lambda () (interactive) (elpy-shell-send-region-or-buffer t)))

;; [[https://github.com/dimitri/emacs-kicker/blob/master/init.el]]
;; If you do use M-x term, you will notice there's line mode that acts like
;; emacs buffers, and there's the default char mode that will send your
;; input char-by-char, so that curses application see each of your key
;; strokes.
;;
;; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
;; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") 'term-paste)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; Open *.m in Octave-mode instead of ObjC
(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))

;; style I want to use in c++ mode
;; from http://www.emacswiki.org/emacs/CPlusPlusMode
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

;; список используемых нами словарей
;; from https://habrahabr.ru/post/215055/
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


;;;;;; rgrep
;;;;(global-set-key (kbd "C-x C-r") 'rgrep)
;; ripgrep -- https://github.com/dajva/rg.el
(require 'rg)
(global-set-key (kbd "C-x C-r") 'rg)

;;
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)


;; Set to the location of your Org files on your local system
(setq org-directory "~/private/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/private/org/from-mobile.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-confirm-babel-evaluate nil)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; enable export to markdown in on C-c C-e
(eval-after-load "org"
  '(require 'ox-md nil t))

;; IRC client (*nix only)
(require 'erc)
;; . Make C-c RET (or C-c C-RET) send messages instead of RET.
(define-key erc-mode-map (kbd "RET") nil)
(define-key erc-mode-map (kbd "C-c RET") 'erc-send-current-line)
(define-key erc-mode-map (kbd "C-c C-RET") 'erc-send-current-line)
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)
;; . hide server messages
;;;;(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(setq erc-auto-query 'buffer) ;; add a whois when someone pms

;; keep erc from eating ram by truncating chat logs
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)


;; .
(defun irc-seen_ ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "me" :port 6697
           :nick "seen_"
           :full-name "D M"
           :password "seen_@erc/freenode:sweOwamAygIlcejIcVic"))

;; Notify when someone mentions my nick.
;; http://bbs.archlinux.org/viewtopic.php?id=40190
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

;; Configure gnus (mail lists, nntp news groups)
;; http://www.xsteve.at/prg/gnus/
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

(setq gnus-score-decay-constant 1) ;default = 3
(setq gnus-score-decay-scale 0.03) ;default = 0.05

(setq gnus-decay-scores t) ;(gnus-decay-score 1000)
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
(require 'sigbegone)

(require 'saveplace)
(setq-default save-place t)

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
(message "Done .emacs")
;; end
