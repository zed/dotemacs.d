---
name: emacs-version-upgrade
description: Use after an Emacs version change or snap refresh (the snap tracks master) — stale .elc/.eln crashes like "Symbol's value as variable is void: <mode>--set-explicitly" in long-running daemons, new core incompatibilities breaking third-party packages (cl-remove-if-not nil predicate, new all/any/take/drop subrs), and debugging signals swallowed by condition-case handlers.
---

# Emacs version-upgrade fallout

The Emacs snap tracks master, so core can change under a running
config. After any refresh, run the batch sanity check from AGENTS.md
("Testing / Validation") and watch `*Messages*` on the next daemon
start.

## Stale .elc/.eln after a snap refresh

When a core macro changes between builds (e.g.
`define-globalized-minor-mode` grew `<mode>--set-explicitly`),
packages whose `.elc`/`.eln` were compiled under the old build crash
with `Symbol's value as variable is void: <mode>--set-explicitly` in
long-running daemons.

Fix: delete the package's `.elc` + its
`~/.emacs.d/eln-cache/<ver>/*.eln`, batch recompile
(`byte-compile-file` + `native-compile`), restart Emacs. `M-x
package-reinstall` works too.

## Emacs 31 (snap master ≥ 2026-09-04) core incompatibilities

Two classes, both shimmed in `init.el` (see "Emacs 31 (snap master)
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
   before. This broke `company-dabbrev` (upstream default
   `company-dabbrev-other-buffers = 'all` → company called
   `(all BUFFER)` → `Wrong number of arguments: #<subr all>, 1`).

Debugging tip: signals swallowed by inner `condition-case` handlers
can be captured pre-unwind via `signal-hook-function`; note that
transient (`transient--get-description`) and org (`org-store-link`)
deliberately probe function arity through caught
`wrong-number-of-arguments` signals — those are false positives.
