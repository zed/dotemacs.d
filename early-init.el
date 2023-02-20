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

(setq ad-redefinition-action 'accept) ;; suppress "got redefined" warning

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
  :url "https://raw.githubusercontent.com/larstvei/Try/8831ded1784df43a2bd56c25ad3d0650cdb9df1d/try.el")

;; ** idle-highlight
(el-get-bundle idle-highlight-mode)

;; ** Show the history of points you visited before
(el-get-bundle popwin)
(el-get-bundle point-history
  :url "https://raw.githubusercontent.com/blue0513/point-history/65eb652549abc704e07311a7820956e467444ec5/point-history.el"
  (point-history-mode t))
; M-x ivy-point-history
(el-get-bundle ivy-point-history
  :url "https://raw.githubusercontent.com/SuzumiyaAoba/ivy-point-history/88c0a585105271322ac0bc65418c7eb908139bcd/ivy-point-history.el")

;; ** tla-mode
(el-get-bundle tla-mode
  :url "https://raw.githubusercontent.com/ratish-punnoose/tla-mode/28c915aa49e043358a29bde045a68357027d96de/tla-mode.el")

;; ** ox-ipynb — Convert an org-file to an ipynb
(el-get-bundle ox-ipynb
  :url "https://raw.githubusercontent.com/jkitchin/ox-ipynb/0af95842c4158c71edd6173dbc3405597b528871/ox-ipynb.el")

;; ** copilot
(el-get-bundle zerolfx/copilot.el)

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
