; -*- coding: utf-8; lexical-binding: t; -*-
;; * prelude
(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; ** suppress GUI elements early for TUI and faster startup
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

;; ** suppress obsolete alias warnings from unmaintained packages
(setq warning-minimum-level :error)

;; ** adjust gc to speedup startup (4s->2s)
;; We up the gc threshold to temporarily prevent it from running, then
;; reset it later after startup is complete. Not resetting it will
;; cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8))
            (setq gc-cons-percentage 0.1)))

;; ** https://www.reddit.com/r/emacs/comments/ct0h6m/this_nobbled_me_yesterday_on_debian_10_it_needs/
(when (and (>= libgnutls-version 30603)
           (version<= emacs-version "26.2"))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(defun init:report-elapsed-time (&optional what-has-loaded)
  "Report time elapsed since emacs start."
  (or what-has-loaded (setq what-has-loaded load-file-name))
  (let ((elapsed (float-time (time-subtract (current-time)
					    emacs-start-time))))
    (message "Loading %s...done (elapsed %.3fs)" what-has-loaded elapsed)))

;; * load customizations
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/") t) ; Fallback
(add-to-list 'package-archives '("gnu"          . "https://elpa.gnu.org/packages/") t)
;; Prefer MELPA Stable
(setq package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"          . 5)
        ("melpa"        . 0)))

(setq package-install-upgrade-built-in t)  ; to update magit, transient, etc
(setq custom-file "~/.custom.el")
(load custom-file)

;; * bootstrap el-get
(setq el-get-install-skip-emacswiki-recipes t) ; don't build local emacswiki recipes
(add-to-list 'load-path  (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get-user/recipes"))
(require 'el-get-bundle)  ; no longer autoloaded by el-get itself (el-get 5.2+)

;; * install packages
(setq load-prefer-newer t) ; suppress warning about .autoloads.el files

;; Shim for the dropped `with-eval-after-load-feature' package
;; (unmaintained since 2014, pulls in obsolete cl on Emacs 31+).
;; The original also preloaded features at byte-compile time to silence
;; free-variable warnings; init.el is loaded as source and never
;; byte-compiled, so plain nested `with-eval-after-load' is equivalent.
(defmacro with-eval-after-load-feature (feature &rest body)
  "Run BODY after FEATURE is loaded.
FEATURE is a (quoted) symbol or a list of symbols; for a list, BODY
runs after all of them have been loaded, in order."
  (declare (indent 1) (debug t))
  (let* ((fs (if (and (listp feature) (eq (car-safe feature) 'quote))
                 (cdr feature)
               feature))
         (form `(progn ,@body)))
    (dolist (f (reverse fs) form)
      (setq form `(with-eval-after-load ',f ,form)))))
(require 'use-package)
(with-eval-after-load 'use-package
  (when init-file-debug  ; emacs --debug-init
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t  ; M-x use-package-report to see the results
          debug-on-error t))
  (require 'use-package-ensure)
  (setopt use-package-always-ensure t))

(setq ad-redefinition-action 'accept) ;; suppress "got redefined" warning

;; ** Silence "Accessing slot ... via obsolete initarg name" chatter from
;; packages that access slots via their :initarg (e.g. old gh/pcache).
;; t keeps the backward-compat path working, just without the message.
(setq eieio-backward-compatibility t)

;; ** vendored packages (see vendor/README.md in the repo)
;; know-your-http-well: upstream is dead and its files lack
;; lexical-binding cookies (warning spam on every load since Emacs 30).
;; company-restclient's recipe override in el-get-user/recipes/ drops the
;; dependency on the el-get package; this vendored copy serves it instead.
(add-to-list 'load-path
             (concat user-emacs-directory "vendor/know-your-http-well"))

;; ** Hydra
(el-get-bundle hydra
  :checkout "59a2a45a35027948476d1d7751b0f0215b1e61aa")

;; ** jump to visible input (use keyboard as a mouse)
;; NOTE: must be declared BEFORE ace-window/ace-link, which :depend on
;; avy — otherwise the dependency-driven init sees the built-in recipe
;; (without :checkout) and el-get warns about the cached recipe.
(el-get-bundle avy
  :checkout "933d1f36cca0f71e4acb5fac707e9ae26c536264")

(el-get-bundle ace-window
  :checkout "77115afc1b0b9f633084cf7479c767988106c196")

;; ** highlight jira formatted files
(el-get-bundle jira-markup-mode
  :checkout "4fc534c47df26a2f402bf835ebe2ed89474a4062"
  :description "Emacs major mode for JIRA-markup-formatted text files"
  :type github
  :pkgname "mnuessler/jira-markup-mode")

;; ** Support for hacking python code
(el-get-bundle tdd
  :checkout "1f18a061c92bffb3fbf3f690dc52bf8a76291c92"
  :description "Run recompile (or a customisable function) after saving a buffer"
  :type github
  :pkgname "jorgenschaefer/emacs-tdd")

                                        ; format Python code on save (C-x C-s)
(el-get-bundle blacken
  :checkout "196cc080f2aaec33caa1b9298e3a9f3dc52c26c5"
  :description "Reformat python buffers using the 'black' formatter"
  :type github
  :pkgname "proofit404/blacken")

;; NOTE: keep this pin at the same commit as the elpa reformatter
;; (MELPA snapshot, currently 20241204.1051 = f2cb594) — elpa
;; zig-mode/ruff-format and el-get python-black share the load-path, so
;; the copies must not drift.
(el-get-bundle reformatter
  :checkout "f2cb59466b1c3f85a8c960f7d4b7b7ead015bedc")
(el-get-bundle python-black
  :checkout "4da1519345b3d5c513d82ef0d39536dd9c626d42"
  :description "Emacs package to reformat Python using black-macchiato"
  :type github
  :pkgname "wbolster/emacs-python-black")

;; ** misc
(el-get-bundle multiple-cursors
  :checkout "89f1a8df9b1fc721b1672b4c7b6d3ab451e7e3ef")

(el-get-bundle markdown-mode
  :checkout "7c51a2167c5a1330e0ab52fe5b2d03c1ead122ca")

(el-get-bundle paredit
  :checkout "af075775af91f2dbc63b915d762b4aec092946c4")
(el-get-bundle rainbow-delimiters
  :checkout "f40ece58df8b2f0fb6c8576b527755a552a5e763")

                                        ; *** jump to link in info, eww buffers: type O + appeared avy letters
(el-get-bundle ace-link
  :checkout "d9bd4a25a02bdfde4ea56247daf3a9ff15632ea4")
(el-get-bundle typing
  :checkout "a2ef25dde2d8eb91bd9c0c6164cb5208208647fa")

;; ** Increase selection by semantic units
(el-get-bundle expand-region
  :checkout "351279272330cae6cecea941b0033a8dd8bcc4e8")

;; ** restclient
(el-get-bundle restclient
  :checkout "e2a2b13482d72634f8e49864cd9e5c907a5fe137")
                                        ; NOTE: avoid "recursive load" error from el-get
(el-get-bundle company-restclient
  :checkout "e5a3ec54edb44776738c13e13e34c85b3085277b")

                                        ; for ivy-regex-fuzzy sorting of large lists
;; *** ido/ivy/helm imenu tag selection across buffers with the same mode/project etc
(el-get-bundle imenu-anywhere
  :checkout "06ec33d79e33edf01b9118aead1eabeae8ee08b1")

;; ** web-mode
(el-get-bundle web-mode
  :checkout "994cb350bceeebb031406112cf6da119e066ef8e")
;; ** yaml-mode
(el-get-bundle yaml-mode
  :checkout "d91f878729312a6beed77e6637c60497c5786efa")

;; ** try
(el-get-bundle try
  :url "https://raw.githubusercontent.com/larstvei/Try/8831ded1784df43a2bd56c25ad3d0650cdb9df1d/try.el")

;; ** idle-highlight
(el-get-bundle idle-highlight-mode
  :checkout "c466f2a9e291f9da1167dc879577b2e1a7880482")

;; ** Show the history of points you visited before
(el-get-bundle popwin
  :checkout "ec77f3f1631cb2666971c57027a64833636ef0f6")
(el-get-bundle point-history
  :url "https://raw.githubusercontent.com/blue0513/point-history/65eb652549abc704e07311a7820956e467444ec5/point-history.el")
                                        ; M-x ivy-point-history
(el-get-bundle ivy-point-history
  :url "https://raw.githubusercontent.com/SuzumiyaAoba/ivy-point-history/88c0a585105271322ac0bc65418c7eb908139bcd/ivy-point-history.el")

;; ** tla-mode
(el-get-bundle tla-mode
  :url "https://raw.githubusercontent.com/ratish-punnoose/tla-mode/28c915aa49e043358a29bde045a68357027d96de/tla-mode.el")

;; ** ox-ipynb — Convert an org-file to an ipynb
(el-get-bundle ox-ipynb
  :url "https://raw.githubusercontent.com/jkitchin/ox-ipynb/0af95842c4158c71edd6173dbc3405597b528871/ox-ipynb.el")

;; ** protobuf-mode — major mode for editing protocol buffers.
(el-get-bundle protobuf-mode
  :url "https://raw.githubusercontent.com/protocolbuffers/protobuf/b9483e03c7cfa854c250ad6415b4d9e6f7a9709e/editors/protobuf-mode.el")
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
