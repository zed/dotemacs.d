# AGENTS.md — Emacs Configuration

This is a personal Emacs configuration (not a package). It is
bootstrapped via `el-get` and uses `use-package` for most package
declarations.

## Architecture

- **`early-init.el`** — Package archives, `el-get` bootstrap,
  native-comp settings, GC tuning, and packages installed via
  `el-get-bundle`.
- **`init.el`** — Everything else: `use-package` declarations,
  keybindings, hooks, and custom functions.
- **No `use-package` declarations after line ~1940** — the file ends
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

## Key Conventions

- `init:` prefix is used for all user-defined functions (e.g. `init:report-elapsed-time`).
- `with-eval-after-load-feature` is used instead of
  `with-eval-after-load` (provided by an `el-get` bundle).
- `lexical-binding: t` is declared in both init files.
- **Gitmoji** — Commit messages use gitmoji prefixes (e.g. `✨`, `♻️`, `🐛`).
  See existing git history for examples.

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
keyring get me .secrets.el | \
  emacs --batch --eval "
    (progn
      (setq debug-on-error nil
            package-check-signature nil
            epa-pinentry-mode 'loopback)
      (load \"~/.emacs.d/early-init.el\")
      (condition-case nil
          (load \"~/.emacs.d/init.el\")
        (error nil))
      (message \"=== INIT LOADED SUCCESSFULLY ===\"))"
```

The batch check catches syntax errors and missing autoloads. It silently
ignores `.secrets.el.gpg` decryption failures (the file is optional).

## Gotchas

- **Startup shows agenda** — `after-init-hook` runs
  `init:show-agenda`, which deletes other windows and opens
  `org-agenda`. This can be surprising when testing in a graphical
  frame.
- **Native compilation** — Emacs 28+ compiles `.elc` → `.eln`
  asynchronously. Warnings are silenced
  (`native-comp-async-report-warnings-errors: silent`).
- **GPG loopback** — In non-GUI sessions, `epa-pinentry-mode` is set
  to `loopback` to allow GPG passphrase entry via stdin (relevant for
  `~/.secrets.el.gpg`).
- **Electric pair** — Enabled in `prog-mode`, explicitly disabled in
  `org-mode`.
- **TRAMP performance** — `vc-handled-backends` is set to `nil` for
  remote files via `find-file` hook.
- **Large files** — Warning threshold is set to 1 GB (`large-file-warning-threshold`).
