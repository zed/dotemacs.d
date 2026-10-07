---
name: emacs-package-maintenance
description: Use when upgrading, reinstalling, pinning or dropping el-get/elpa packages in this Emacs config (~/.emacs.d), when looking up which version/tag MELPA stable or unstable offers for a package, when doing a clean package.el install/reinstall, when el-get prints "Must update or reinstall ... cached recipe" warnings, when el-get autoloads break (e.g. "void defhydra" at startup), when keeping el-get↔elpa duplicate pins in sync, or when touching vendored packages under vendor/.
---

# Emacs package maintenance (el-get + elpa)

Context: personal Emacs config; repo at `~/private/.emacs.d`, live dir
`~/.emacs.d` (reaches the repo via symlinks — see AGENTS.md). Two
package managers: el-get (pins, bundles) and package.el/use-package
(everything else). Verify any change with the batch sanity check in
AGENTS.md ("Testing / Validation").

## Duplicated packages (el-get + elpa) must not drift

8 packages are installed through BOTH managers (ace-window, avy,
company-mode, dash, hydra, markdown-mode, reformatter, restclient):
elpa copies are forced by elpa dependents (e.g. zig-mode needs elpa
reformatter — package.el can't see el-get installs and refuses
activation), el-get copies are deliberate pins needed by el-get
dependents. The el-get copy wins on `load-path`, so the pin IS the
running version for everyone.

Convention: pin the el-get `:checkout` to the exact commit behind the
installed elpa version (for MELPA snapshots find the commit in the
archive-contents `:commit` field, e.g. reformatter 20241204.1051 ==
f2cb594). When the elpa side upgrades, re-pin + `M-x
el-get-reinstall`. `company-mode` is the one exception: unpinned,
tracks master; after upgrading elpa company, `git pull` the el-get
copy too (it is the copy that actually runs).

## Looking up MELPA distributives (archive-contents)

For "what does MELPA actually offer for package X", fetch
archive-contents directly (full download — entries are
alphabetical, so truncating the file loses later packages):

```bash
curl -s https://stable.melpa.org/packages/archive-contents | rg -o '\(NAME \. \[\([0-9 ]+\).*'
curl -s https://melpa.org/packages/archive-contents         | rg -o '\(NAME \. \[\([0-9 ]+\).*'
# gnu:   https://elpa.gnu.org/packages/archive-contents
# nongnu: https://elpa.nongnu.org/nongnu/archive-contents
```

stable.melpa.org builds only from version tags — the version IS the
tag (`(3 3 0)` = v3.3.0; `:commit` is its commit, `:revdesc` shows
"v3.3.0-0-g…"). melpa.org serves dated snapshots (`(20261004
1450)`), always newer-looking than any tag. The config prefers
stable (priorities in early-init.el), so a tagged version there is
what a plain `package-install` picks. To map an installed elpa
version to an upstream commit, match `:commit`/`:revdesc` extras
(this is also how the el-get pins below get their `:checkout`).

## Clean package.el install/upgrade (no local hacks)

Install through package.el only — never untar into `elpa/`, copy a
dir in, or hand-write autoloads. Batch script (replicates
early-init.el's archives/priorities verbatim):

```elisp
(require 'package)
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu"          . "https://elpa.gnu.org/packages/") t)
(setq package-archive-priorities
      '(("melpa-stable" . 10) ("gnu" . 5) ("melpa" . 0)))
(package-initialize)
(package-refresh-contents)
(package-install 'NAME)
```

Gotchas: re-running the script after a successful install prints
"'NAME' is already installed" — that is success, not failure.
Verify with `(assq 'NAME package-alist)` — keys are SYMBOLS; `assoc
"NAME"` silently returns nil. Then run the batch sanity check in
AGENTS.md. `package-selected-packages` lives in `~/.custom.el`; a
use-package'd package should already be listed there.

## el-get "cached recipe" warnings

If "Must update or reinstall ... to modify its cached recipe" warnings
appear, re-run `el-get-merge-properties-into-status` for every entry
in `el-get-sources` (operation `'reinstall`) in a batch Emacs, or do a
real reinstall. Never edit `.status.el` by hand.

## el-get `.loaddefs.el` can silently go empty (Emacs 29+)

el-get regenerates autoloads per package via `loaddefs-generate`,
which (a) only scrapes files NEWER than the existing output file and
(b) does not recurse into subdirectories. If `.loaddefs.el` is ever
recreated as an empty rubric, per-package regeneration adds nothing
and every el-get autoload disappears (first symptom: `defhydra` void
in the hydra `use-package` `:init`).

Fix: delete `.loaddefs.el{,c,~}` and regenerate in ONE call over all
installed package dirs, then strip the `no-byte-compile` cookie el-get
dislikes and byte-compile:

```elisp
(let* ((pkgs (el-get-list-package-names-with-status "installed"))
       (dirs (delete-dups (seq-filter #'file-directory-p
                          (delq nil (apply #'append
                                 (mapcar #'el-get-load-path pkgs)))))))
  (loaddefs-generate dirs el-get-autoload-file))
```

## Vendored dead packages

`vendor/` holds patched copies of packages that are unmaintained
upstream and warned on Emacs 31 (see `vendor/README.md`): currently
just `know-your-http-well` (el-get package dropped via the recipe
override `el-get-user/recipes/company-restclient.rcp` in this repo, so
`el-get-cleanup` removes it). **Do not reinstall it via el-get.**

The missing-lexical-cookie warning bypasses both
`warning-minimum-level` and `warning-suppress-types` (verified on
Emacs 31), so suppression is not an alternative to vendoring. Only
the two data files company-restclient uses are vendored;
`http-relations`/`http-status-codes` were dropped (unused, and the
latter carries a GPLv2+ header contradicting upstream's Unlicense).

## eieio obsolete-initarg chatter

`eieio-backward-compatibility` is set to `t` in `early-init.el`:
packages accessing slots via their `:initarg` (old `gh`/`pcache` did)
print "Accessing slot ... via obsolete initarg name" straight to
`*Messages*` (plain `message`, not filterable by
`warning-minimum-level`; default value `warn` prints, `t` keeps the
compat path silently). pcache ≥ 2026-07 fixed its side; the variable
stays as insurance for other old packages.

## realgud recursive autoloads

realgud ships `realgud-recursive-autoloads.el` inside its tarball;
released tarballs may lack the lexical-binding cookie (fixed on
upstream master). After a realgud package update, check line 1 for
the cookie; if missing, regenerate with `loaddefs-generate` over ALL
leaf subdirs of the package's `realgud/` dir (no recursion — pass
every leaf dir, see the `.loaddefs.el` section above).
