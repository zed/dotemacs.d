# AGENTS.md — Emacs Configuration

This is a personal Emacs configuration (not a package). It is
bootstrapped via `el-get` and uses `use-package` for most package
declarations.

This file is loaded into every agent session — keep it to rules
needed on every task. Incident history and one-off specifics belong
in commit messages; handle recurrences when they come up, not by
pre-documenting them here.

## Architecture

- **`early-init.el`** — Package archives, `el-get` bootstrap,
  GC tuning, and packages installed via `el-get-bundle`.
- **`init.el`** — Everything else: `use-package` declarations,
  keybindings, hooks, and custom functions. The file ends with core
  Emacs settings and a final `use-package emacs` block — add new
  `use-package` declarations before that tail.
- **`~/.custom.el`** — Expected to exist (set in `early-init.el` via
  `custom-file`). It is loaded unconditionally; do not delete it.
- **`~/.secrets.el.gpg`** — GPG-encrypted secrets loaded early
  (optional, errors silently if missing).
- **Live dir reaches the repo via symlinks** — the repo lives at
  `~/private/.emacs.d`, but Emacs runs from `~/.emacs.d`. Required
  symlinks: `~/.emacs` → `init.el`, `~/.emacs.d/early-init.el` →
  `early-init.el`, `~/.emacs.d/vendor` → `vendor/`,
  `~/.emacs.d/el-get-user` → `el-get-user/`. Recreate them after a
  fresh clone.

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

**Do not reinstall `know-your-http-well` via el-get** — it is
vendored in `vendor/` (see `vendor/README.md`); the el-get package is
dropped via the recipe override `el-get-user/recipes/company-restclient.rcp`.

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
- **Obsolete alias warnings suppressed** — `(setq warning-minimum-level :error)`
  in `early-init.el` hides warnings from unmaintained packages (e.g.
  `gist`) that still use obsolete `cl` aliases.

## Maintenance playbooks (load on demand)

AGENTS.md holds always-on rules and orientation. Procedures for
package/Emacs upkeep live in repo skills — load them BEFORE the
matching task (pi auto-lists `.agents/skills/`; other harnesses: read
the SKILL.md files directly):

- **`.agents/skills/emacs-package-maintenance/`** — upgrading or
  reinstalling el-get/elpa packages; el-get "cached recipe" warnings;
  empty `.loaddefs.el` (all el-get autoloads void); keeping
  el-get↔elpa duplicate pins in sync; vendored packages; realgud
  autoloads; eieio obsolete-initarg chatter.
- **`.agents/skills/emacs-version-upgrade/`** — after an Emacs snap
  refresh: stale `.elc`/`.eln` crashes, core incompatibilities (the
  shims live in init.el), debugging swallowed signals.
