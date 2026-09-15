# AGENTS.md — Emacs Configuration

This is a personal Emacs configuration (not a package). It is
bootstrapped via `el-get` and uses `use-package` for most package
declarations.

## Architecture

- **`early-init.el`** — Package archives, `el-get` bootstrap,
  GC tuning, and packages installed via `el-get-bundle`.
- **`init.el`** — Everything else: `use-package` declarations,
  keybindings, hooks, and custom functions. 2112 lines total.
- **No `use-package` declarations after line 1940** — the file ends
  with core Emacs settings and a final `use-package emacs` block.
- **`~/.custom.el`** — Expected to exist (set in `early-init.el` via
  `custom-file`). It is loaded unconditionally; do not delete it.
- **`~/.secrets.el.gpg`** — GPG-encrypted secrets loaded early
  (optional, errors silently if missing).

## Package Management

Two systems are used:

1. **`el-get`** — Bootstrapped from GitHub in `early-init.el`. Used
   for packages not on MELPA or pinned to specific URLs/revisions
   (e.g. `ox-ipynb`, `protobuf-mode`, `htmlz-mode`).
2. **`use-package` + `package.el`** — Used for everything else. MELPA
   Stable is preferred over MELPA.

**Important:** `el-get-cleanup` is called with
`my:el-get-packages`. If you add an `el-get-bundle` declaration, you
must also ensure it is captured in that list or it will be uninstalled
on next startup.

**Do not edit files under `~/.emacs.d/` directly** — `elpa/` and
`el-get/` are installed artifacts managed by their package managers.
Change them through `package.el` / `el-get` (update, reinstall, drop),
not by hand: manual edits are lost on the next update and are
invisible to this repo. If a package is unmaintained, prefer dropping
it for a small shim in the repo (see the
`with-eval-after-load-feature` shim in `early-init.el`) or forking it.

## Key Conventions

- `init:` prefix is used for all user-defined functions (e.g. `init:report-elapsed-time`).
- `with-eval-after-load-feature` is a shim macro in `early-init.el`
  wrapping `with-eval-after-load` (the old `el-get` bundle was
  dropped: unmaintained since 2014, required obsolete `cl`).
- `lexical-binding: t` is declared in both init files.
- **Gitmoji** — Commit messages use gitmoji prefixes (e.g. `✨`, `♻️`, `🐛`).
  See existing git history for examples.
- **LLM-generated commits** — Commits created by an AI assistant are
  tagged with `[LLM-generated]` in the message body.

## Language / Mode Notes

- **Python** — `elpy` is enabled via advice on
  `python-mode`. Format-on-save uses `ruff-format` if
  `[tool.ruff.format]` is found in `pyproject.toml`, otherwise
  `blacken` if `[tool.black]` is present. `flymake-ruff` replaces the
  default Python flymake backend when `[tool.ruff.lint]` is present.
- **TypeScript/TSX** — Uses tree-sitter modes (`typescript-ts-mode`,
  `tsx-ts-mode`). `eglot` is used for LSP with
  `typescript-language-server`. `prettier-js` runs on save.
- **Org** — Extensive config including `jupyter`, `ob-async`,
  `code-cells` (for `.ipynb` ↔ `.org` conversion via pandoc), and
  `org-fc` (spaced repetition, loaded from `~/src/org-fc`).
- **C/C++** — Custom style `my-style` (2-space indent, no tabs).
- **Java** — Custom style `my-java-style` (4-space indent).

## External Dependencies

Some packages declare `:ensure-system-package` dependencies. Notable ones:
- `ripgrep` (for `rg`, `counsel-grep`)
- `git` (for `magit`)
- `sqlite3` (for `counsel-dash`)
- `shellcheck` (for `sh-mode` flycheck)
- `black` / `black-macchiato` (for `blacken`)
- `pandoc` (for `code-cells` ipynb conversion)
- `hunspell` + dictionaries (for `flyspell` multi-language)

## Testing / Validation

There is no test suite. To validate changes:

```bash
# Start Emacs with debug init
emacs --debug-init

# Start Emacs without loading init (for comparison)
emacs -Q

# Batch sanity check (loads init without interactive UI)
# Requires ~/.custom.el to exist and keyring access for ~/.secrets.el.gpg
keyring get "$USER" .secrets.el.gpg | \
  emacs --batch --eval "
    (progn
      (setq debug-on-error nil
            package-check-signature nil
            epa-pinentry-mode 'loopback)
      (load \"~/.emacs.d/early-init.el\")
      ;; batch startup skips package activation; the daemon does this
      ;; between early-init and init, so replicate it here
      (package-activate-all)
      (condition-case err
          ;; ~/.emacs is the real init (symlink into this repo);
          ;; ~/.emacs.d/init.el does NOT exist here
          (load \"~/.emacs\")
        (error (message \"=== INIT LOAD ERROR: %S ===\" err)))
      (message \"=== INIT LOADED SUCCESSFULLY ===\"))"
```

The batch check catches syntax errors and missing autoloads. Watch for
`=== INIT LOAD ERROR ===` in the output — the `condition-case` keeps the
run going past init errors, it must not hide them. Decryption failures
of `.secrets.el.gpg` are silently ignored (the file is optional).

## Gotchas

- **Startup shows agenda** — `after-init-hook` runs
  `init:show-agenda`, which deletes other windows and opens
  `org-agenda`. This can be surprising when testing in a graphical
  frame.
- **GPG loopback** — In non-GUI sessions, `epa-pinentry-mode` is set
  to `loopback` to allow GPG passphrase entry via stdin (relevant for
  `~/.secrets.el.gpg`).
- **Electric pair** — Enabled in `prog-mode`, explicitly disabled in
  `org-mode`.
- **TRAMP performance** — `vc-handled-backends` is set to `nil` for
  remote files via `find-file` hook.
- **Large files** — Warning threshold is set to 1 GB (`large-file-warning-threshold`).
- **Obsolete alias warnings suppressed** — `(setq warning-minimum-level :error)`
  in `early-init.el` hides warnings from unmaintained packages (e.g.
  `gist`, `elisp-format`) that still use obsolete `cl` aliases.
- **Duplicated packages (el-get + elpa) must not drift** — 7 packages
  are installed through BOTH managers (ace-window, avy, dash, hydra,
  markdown-mode, reformatter, restclient): elpa copies are forced by
  elpa dependents (e.g. zig-mode needs elpa reformatter — package.el
  can't see el-get installs and refuses activation), el-get copies are
  deliberate pins needed by el-get dependents. The el-get copy wins on
  `load-path`, so the pin IS the running version for everyone.
  Convention: pin the el-get `:checkout` to the exact commit behind
  the installed elpa version (for MELPA snapshots the pin date == the
  snapshot version, e.g. avy 2024-11-01 == 20241101.1357; for stable
  the tag commit, e.g. reformatter 0.7 == bfe3f1c). When the elpa side
  upgrades, re-pin + `M-x el-get-reinstall`.
- **el-get ≥ 5.2 migration done (2026-09)** — el-get was updated past
  the recipe-cache format change; all `el-get-bundle` packages had
  their cached recipes force-merged (`el-get-merge-properties-into-status`).
  If "Must update or reinstall ... to modify its cached recipe"
  warnings ever reappear, the fix is
  `M-x el-get-merge-properties-into-status` (or a real reinstall), not
  editing files.
- **Stale .elc/.eln after an Emacs snap refresh (seen 2026-09)** —
  the snap tracks Emacs master; when a core macro changes between
  builds (e.g. `define-globalized-minor-mode` grew
  `<mode>--set-explicitly`), packages whose `.elc`/`.eln` were
  compiled under the old build crash with `Symbol's value as variable
  is void: <mode>--set-explicitly` in long-running daemons.  Fix:
  delete the package's `.elc` + its `~/.emacs.d/eln-cache/<ver>/*.eln`,
  batch recompile (`byte-compile-file` + `native-compile`), restart
  Emacs.  `M-x package-reinstall` works too.
- **Emacs 31 (snap master ≥ 2026-09-04) core incompatibilities** —
  two classes, both shimmed in `init.el` (see "Emacs 31 (snap master)
  compatibility shims" near the top and the `company` block):
  1. `cl-remove-if-not` (and 21 sibling `cl-*-if(-not)` functions in
     cl-seq) was reimplemented as `(cl-remove pred list :test-not
     #'funcall)`; a nil predicate now errors with `funcall: Symbol's
     function definition is void: nil` (Emacs ≤ 30 tolerated nil).
     This broke ivy's alist path (`ivy--reset-state`): counsel `C-r`
     minibuffer history, the `rg` files prompt, `ivy-reverse-i-search`.
  2. New core subrs `all`, `any`, `take`, `drop` (and `remove`): any
     `(pcase x ... ((pred functionp) ...))` or `(functionp 'sym)` check
     on a symbol with one of these names now matches where it didn't
     before.  This broke `company-dabbrev` (upstream default
     `company-dabbrev-other-buffers = 'all` → company called
     `(all BUFFER)` → `Wrong number of arguments: #<subr all>, 1`).
  Debugging tip: signals swallowed by inner `condition-case` handlers
  can be captured pre-unwind via `signal-hook-function`; note that
  transient (`transient--get-description`) and org (`org-store-link`)
  deliberately probe function arity through caught
  `wrong-number-of-arguments` signals — those are false positives.
