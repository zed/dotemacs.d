# Security

Threat model for the Emacs configuration in this repo (`early-init.el`,
`init.el`).

This is a **living document**, not a point-in-time audit. It records the
deployment, the assets, the adversaries in scope, and — explicitly — that the
**current threats are acceptable; no action is required at this time.** It
exists so that the *posture* is written down: anyone reading this knows what
is deliberately accepted, and what change in deployment would invalidate that
acceptance and force a re-evaluation.

Methodology informed by the 10-phase checklist from
[JayDoolan/security-audit](https://github.com/JayDoolan/security-audit) and
[irfad7/claude-power-skills → security-review](https://github.com/irfad7/claude-power-skills),
but the shape here differs from a server audit: this is a **single-user local
program that executes code on the user's behalf**, not a network service. The
relevant questions are supply chain, untrusted-content execution, secrets
hygiene, and the one network surface (SSH-from-iPad into the laptop's emacs
daemon).

## Current posture

**All threats identified below are accepted. No action is required.** Each
threat in the model is paired with the reason it is acceptable under this
deployment. The only thing that would change this conclusion is a change in
*deployment* — specifically the tripwires in the last section.

## Threat model

Agreed 2026-08-01. A threat is only listed if an in-scope adversary can
trigger it; every listed threat is then graded against the deployment and
either accepted or noted as a tripwire.

### Deployment

Personal Emacs configuration, single laptop, single OS user. The config is
bootstrapped via `el-get` (`early-init.el:60-65`) and uses `use-package` for
the rest. Package sources: GNU ELPA, MELPA Stable (preferred), MELPA
(fallback), plus ~30 `el-get-bundle` declarations pinned to specific commits
(`early-init.el:99-209`, 23 `:checkout` pins + 6 `:url` pins).

**Network surface — the one that matters:** Emacs runs as a **daemon** and is
driven over SSH from an iPad with a keyboard (`server-after-make-frame-hook`,
`init.el:2197-2206`). The threat here is *the SSH channel*, not Emacs itself:
if the laptop accepts SSH from anything beyond the user's own iPad on a
trusted network, anyone who reaches that SSH port effectively gets the user's
OS account. Emacs adds nothing on top of that — it is the SSH daemon's policy
that is the boundary. There is **no** Emacs TCP server
(`make-network-process`, `(server-start t)`, etc.) — `emacsclient` reaches the
daemon over a local unix socket only.

Other network egress, all initiated *by the user's own Emacs* on the user's
own behalf:

- **Package archives** (`early-init.el:43-51`) — HTTPS to `elpa.gnu.org`,
  `stable.melpa.org`, `melpa.org`. `package-check-signature` is left at its
  default (allow-signed), not forced to `nil`.
- **el-get bootstrap** (`early-init.el:60-65`) — pulls `el-get-install.el`
  from `raw.githubusercontent.com` over HTTPS on first run only.
- **ERC** (IRC client), **Slack** (`slack` package), **gptel** (LLM API) —
  all configured, all credentialed from `~/.secrets.el.gpg`.
- **TRAMP** — used regularly to edit files on remote hosts over SSH.

The **repo is public**: `github.com/zed/dotemacs.d`, `"private": false`.
Everything committed here is world-readable; this is the single fact that
elevates secrets/PII hygiene into the model.

### Assets

- **`~/.secrets.el.gpg`** — GPG-encrypted, loaded at startup
  (`init.el:13`). Holds IRC credentials (`.secrets-irc-start`,
  `init.el:1082`), Slack team registration (`.secrets-slack-register-team`,
  `init.el:1764`), and `gptel` model/backend (`init.el:1905`). **Highest
  confidentiality value.** Not in the repo; lives at `~/` outside this tree.
  Its *contents* are out of scope (the doc cannot read them), but the
  *loading path* is in scope.
- **The user's OS account / code-execution.** Emacs is a code-execution
  engine: `org-babel`, dir-locals `eval`, `auto-mode-alist`, magic-mode,
  hooks, advice, and `call-process`/`shell-command` all run arbitrary code as
  the user. RCE-as-the-user is the largest blast radius and the implicit
  default outcome of the threats below.
- **Integrity of the user's repos and committed work.** Emacs is where the
  user edits and commits code; tampering via a malicious dir-local / hook /
  package can poison commits, signing keys, or pushes.
- **What is in this public repo.** `AGENTS.md` discloses the deploy topology
  (daemon + iPad-over-SSH), the secret-loading path, and the *existence* of
  `~/.secrets.el.gpg`. Not a credential, but a reconnaissance aid — accepted,
  the topology is not secret.

### Adversaries in scope

Three, graded against the deployment above:

1. **A malicious file the user opens** — an org/repo/file fetched or cloned
   and opened in Emacs. Vectors: an org src block executed by hand
   (`org-confirm-babel-evaluate` is `nil`, `init.el:1335`), a dir-locals
   `eval`, a `safe-local-variable-values` entry that runs code
   (`init.el:2144-2161`), `auto-mode-alist`/magic-mode dispatch to a hostile
   mode, or an `org-open-at-point`/link to a `file:`/`elisp:` URL.
2. **A reader of the public repo.** Anyone on the internet reading
   `github.com/zed/dotemacs.d`. Cannot execute anything, but can read
   everything committed and use it for reconnaissance against the
   SSH-reachable laptop.
3. **A malicious IRC peer** (residual, after the ERC `notify-send` path was
   hardened). Any IRC peer can send a line that mentions the user's nick and
   reaches `erc-global-notify` (`init.el:1108-1126`); today that path invokes
   `notify-send` via `call-process` with separate argv strings, so the
   message body cannot break out into a shell. The residual threat is a
   *future* change to that function reintroducing interpolation — not a live
   exploit.

### Threats and why each is acceptable

The current threats, with the reason each is accepted under this deployment:

| Threat | Adversary | Trigger / code path | Why acceptable |
| --- | --- | --- | --- |
| **Code-exec from an opened file** (org src block, dir-locals `eval`, trusted `safe-local-variable-values` `eval`, magic-mode dispatch, `file:`/`elisp:` link) | #1 malicious file | `org-confirm-babel-evaluate` `nil` (`init.el:1335`); `safe-local-variable-values` trusts an `eval` form (`init.el:2144-2161`); `enable-local-variables :safe` (`init.el:2044`) | The user has decided that opening a file and evaluating a block / answering "yes" to a prompt is their own choice. The promptless `eval` paths are bounded by a benign current whitelist; the mechanism is the latent footgun, accepted. |
| **Malicious or compromised package** (MELPA / GNU ELPA / pinned el-get source) | supply chain | `early-init.el:43-51` (archives); `early-init.el:99-209` (bundles) | Supply-chain trust is accepted by decision. Mitigant: every `el-get-bundle` is pinned to a fixed commit (23 `:checkout` + 6 `:url`), so "what runs" is reproducible; `package-check-signature` stays at its default. |
| **Public-repo reconnaissance / PII leak** | #2 repo reader | the repo is `private: false`; `AGENTS.md` discloses topology and the secret-loading path | The topology is not secret. Verified scan finds **no** credential/env/key/token committed: `~/.secrets.el.gpg` and `~/.custom.el` both live at `~/`, *outside* the repo tree (`early-init.el:54`), so a `git add .` in the repo cannot reach them. The recon surface is accepted. |
| **ERC `notify-send` re-regression** | #3 IRC peer | `erc-global-notify` (`init.el:1108-1126`) | Currently hardened (`call-process`, no shell, separate argv). Not a live exploit; accepted as a "don't reintroduce string interpolation here" note for future edits. |
| **TRAMP autosave to an "unsafe" temp dir** | local (out of scope) | `tramp-allow-unsafe-temporary-files t` (`init.el:2043`) | Single-user trusted machine — no other OS user to race or snoop. The variable name is alarming but the impact needs a multi-user host, which is out of scope. |

### Out of scope (accepted, by decision)

- **Opening random org/repo files = trusted.** Dir-locals, `safe-local-eval`,
  and answering "yes" to local-variable prompts are the user's own choice.
- **Package supply chain = trusted.** MELPA / GNU ELPA / the pinned el-get
  sources are trusted as-is.
- **Local single-user = trusted environment.** No other OS users, no tmp-file
  race hardening, no local-privilege boundary assumed.
- **The user's own OS account on the laptop, if compromised.** Game over
  regardless; out of scope.
- **Availability / config breakage.** Not a security concern; not graded.

### Trust boundaries

| Boundary | What crosses it | Protection |
| --- | --- | --- |
| iPad → laptop SSH → emacs daemon | keystrokes / `emacsclient` over the SSH channel, then a local unix socket | **SSH policy** (not Emacs); Emacs adds no TCP server |
| Emacs → package archives | HTTPS package + recipe downloads | TLS; `package-check-signature` at default (allowed) |
| Emacs → `~/.secrets.el.gpg` | GPG-decrypt at startup (`init.el:13`) | GPG; `loopback` pinentry in TUI (`init.el:9-12`) |
| IRC server → ERC → `erc-global-notify` | an IRC `msg` reaches `call-process` | passed as separate argv, no shell (`init.el:1108-1126`) |
| Opened file → Emacs | dir-locals, `safe-local-variable-values`, org src blocks, auto-mode | prompt-by-default for non-whitelisted; whitelisted `eval` is accepted |
| Public GitHub repo → anyone | every committed file | none — it is public (`private: false`); no credentials are in tree |

### What is NOT protected, by design

No authentication or authorization *inside* Emacs — it is a single-user local
program with no network service to authenticate. The only security boundary
is "you can reach the laptop's SSH port" + "you have the user's GPG
passphrase". Everything that would be HIGH under an internet-exposed-Emacs
model is **either impossible (no TCP server) or accepted (the user trusts what
they open)**.

## Tripwires — what would invalidate this posture

The acceptance above holds **only** under the current deployment. Re-evaluate
this document (and treat the previously-accepted threats as live) if any of
these change:

1. **The laptop's SSH port becomes reachable beyond the user's own iPad on a
   trusted network** — port-forward, public IP, a cloud tunnel not scoped to
   the user's identity. Then the daemon effectively becomes a remote
   code-exec surface as the user; the boundary is the SSH daemon's policy, not
   Emacs, but the acceptance of "trusted environment, single user" flips.
   Revise *before* exposure, not after.
2. **Emacs gains a TCP server** — `(server-start t)`, `server-use-tcp`, a
   package that opens a port, an LSP/eglot server exposed outward. There is
   no Emacs-level auth model today by design; a TCP listener would need one.
3. **The repo gains a real in-tree secret** — a `.env`, a scratch secret, or
   an in-tree `custom.el`. Today `~/.custom.el` and `~/.secrets.el.gpg` live
   at `~/`, outside the repo, so there is nothing to protect; that geometry
   must be preserved (or a `.gitignore` added) if it ever changes.
4. **The whitelisted `eval` in `safe-local-variable-values` grows** beyond
   its current benign form, or `enable-local-variables` is widened from
   `:safe` to `:all`. Either re-opens promptless code-exec on file-open.
5. **`erc-global-notify` is rewritten to interpolate `nick`/`msg` into a
   shell string.** The current `call-process` form must be preserved.
