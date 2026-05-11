# Package Inventory

This document lists all packages used in this Emacs configuration, their installation source, and whether their version is pinned.

## Legend

- 📌 **Pinned** — Version locked to a specific git commit via `:checkout` in `early-init.el`
- 🔄 **Unpinned** — Installed from MELPA/MELPA Stable, updates with `package.el`
- 📄 **Single-file** — Installed from a raw URL (no version control)
- 📦 **Built-in** — Ships with Emacs

---

## el-get Packages (37 total)

### Pinned Git Packages (23)

| Package | Commit | Source | Description |
|---------|--------|--------|-------------|
| 📌 **ace-link** | `d9bd4a25` | GitHub | Jump to links in info, eww buffers |
| 📌 **ace-window** | `77115afc` | GitHub | Quick window switching |
| 📌 **avy** | `933d1f36` | GitHub | Jump to visible text using chars |
| 📌 **blacken** | `196cc080` | GitHub | Python black formatter |
| 📌 **company-restclient** | `e5a3ec54` | GitHub | Company backend for restclient |
| 📌 **expand-region** | `35127927` | GitHub | Increase selection by semantic units |
| 📌 **flx** | `4b1346eb` | GitHub | Fuzzy matching for ivy |
| 📌 **hydra** | `59a2a45a` | GitHub | Keybinding menus |
| 📌 **idle-highlight-mode** | `c466f2a9` | GitHub | Highlight word under point |
| 📌 **imenu-anywhere** | `06ec33d7` | GitHub | Cross-buffer imenu navigation |
| 📌 **jira-markup-mode** | `4fc534c4` | GitHub | JIRA markup major mode |
| 📌 **markdown-mode** | `7c51a216` | GitHub | Markdown major mode |
| 📌 **multiple-cursors** | `89f1a8df` | GitHub | Multiple cursors |
| 📌 **paredit** | `af075775` | GitHub | Structured editing for Lisp |
| 📌 **popwin** | `ec77f3f1` | GitHub | Popup window management |
| 📌 **python-black** | `4da15193` | GitHub | Python black-macchiato formatter |
| 📌 **rainbow-delimiters** | `f40ece58` | GitHub | Color-coded parentheses |
| 📌 **reformatter** | `6ac08ceb` | GitHub | Generic reformatting framework |
| 📌 **restclient** | `e2a2b134` | GitHub | HTTP REST client |
| 📌 **tdd** | `1f18a061` | GitHub | Test-driven development helper |
| 📌 **typing** | `a2ef25dd` | GitHub | Typing practice |
| 📌 **web-mode** | `994cb350` | GitHub | Web template editing |
| 📌 **yaml-mode** | `d91f8787` | GitHub | YAML major mode |

### Unpinned el-get Packages (10)

| Package | Source | Description |
|---------|--------|-------------|
| **company-mode** | el-get recipe | Auto-completion framework |
| **dash** | el-get recipe | Modern list library |
| **el-get** | bootstrap | Package manager itself |
| **htmlz-mode** | el-get recipe | HTML compression |
| **know-your-http-well** | el-get recipe | HTTP reference |
| **noflet** | el-get recipe | Local function overrides |
| **with-eval-after-load-feature** | el-get recipe | Enhanced after-load hooks |

### Single-File URL Packages (4)

| Package | URL | Description |
|---------|-----|-------------|
| 📄 **ivy-point-history** | raw GitHub | Ivy integration for point-history |
| 📄 **ox-ipynb** | raw GitHub | Org to Jupyter notebook converter |
| 📄 **point-history** | raw GitHub | Point navigation history |
| 📄 **protobuf-mode** | raw GitHub | Protocol Buffers major mode |
| 📄 **tla-mode** | raw GitHub | TLA+ specification language |
| 📄 **try** | raw GitHub | Try packages without installing |

---

## MELPA/MELPA Stable Packages (150+ total)

### Key Packages (selection)

| Package | Source | Description |
|---------|--------|-------------|
| 🔄 **amx** | MELPA | Enhanced M-x |
| 🔄 **avy-zap** | MELPA | Zap to char with avy |
| 🔄 **command-log-mode** | MELPA | Show typed keys |
| 🔄 **company** | MELPA | Auto-completion |
| 🔄 **consult** | MELPA | Search/navigation commands |
| 🔄 **counsel** | MELPA | Ivy-enhanced commands |
| 🔄 **counsel-projectile** | MELPA | Projectile + Counsel |
| 🔄 **crux** | MELPA | Useful editing commands |
| 🔄 **delight** | GNU ELPA | Clean up mode line |
| 🔄 **dockerfile-mode** | MELPA | Dockerfile editing |
| 🔄 **dumb-jump** | MELPA | Jump to definition |
| 🔄 **elpy** | MELPA | Python IDE features |
| 🔄 **embark** | MELPA | Contextual actions |
| 🔄 **envrc** | MELPA | direnv integration |
| 🔄 **ess** | MELPA | Emacs Speaks Statistics (R) |
| 🔄 **evil** | MELPA | Vim emulation |
| 🔄 **flycheck** | MELPA | Syntax checking |
| 🔄 **flymake-ruff** | MELPA | Python linting with ruff |
| 🔄 **forge** | MELPA | GitHub/GitLab integration |
| 🔄 **fzf** | MELPA | Fuzzy finder |
| 🔄 **git-gutter** | MELPA | Show git changes in fringe |
| 🔄 **git-timemachine** | MELPA | Browse git history |
| 🔄 **go-mode** | MELPA | Go language support |
| 🔄 **gptel** | MELPA | LLM/chatGPT integration |
| 🔄 **ivy** | MELPA | Completion framework |
| 🔄 **ivy-hydra** | MELPA | Hydra menus for ivy |
| 🔄 **jenkinsfile-mode** | MELPA | Jenkins pipeline editing |
| 🔄 **jupyter** | MELPA | Jupyter notebook integration |
| 🔄 **magit** | MELPA | Git interface |
| 🔄 **marginalia** | MELPA | Annotations in minibuffer |
| 🔄 **org-bullets** | MELPA | Pretty org bullets |
| 🔄 **org-jira** | MELPA | JIRA integration |
| 🔄 **org-super-agenda** | MELPA | Enhanced org agenda |
| 🔄 **pdf-tools** | MELPA | PDF viewer |
| 🔄 **php-mode** | MELPA | PHP language support |
| 🔄 **popper** | MELPA | Popup buffer management |
| 🔄 **prettier-js** | MELPA | JS/TS formatting |
| 🔄 **projectile** | MELPA | Project management |
| 🔄 **pulsar** | GNU ELPA | Pulse highlight on navigation |
| 🔄 **pyvenv** | MELPA | Python virtualenv |
| 🔄 **realgud** | MELPA | Debugger interface |
| 🔄 **rg** | MELPA | ripgrep interface |
| 🔄 **rust-mode** | MELPA | Rust language support |
| 🔄 **swiper** | MELPA | Ivy-enhanced isearch |
| 🔄 **system-packages** | GNU ELPA | System package management |
| 🔄 **telega** | MELPA | Telegram client |
| 🔄 **theme-changer** | MELPA | Day/night theme switching |
| 🔄 **tide** | MELPA | TypeScript IDE |
| 🔄 **treemacs** | MELPA | File tree explorer |
| 🔄 **vterm** | MELPA | Terminal emulator |
| 🔄 **which-key** | MELPA | Show keybindings |
| 🔄 **yasnippet** | MELPA | Text snippets |
| 🔄 **zig-mode** | MELPA | Zig language support |

---

## Version Pinning Status

| Category | Count | Pinned |
|----------|-------|--------|
| el-get (git) | 23 | ✅ 23 (100%) |
| el-get (recipe) | 10 | ❌ 0 (0%) |
| el-get (single-file) | 6 | ❌ N/A (no VCS) |
| MELPA/MELPA Stable/GNU | 150+ | ❌ 0 (0%) |

### Pinning Strategy

1. **el-get git packages**: All pinned with `:checkout` to specific commits
2. **el-get recipe packages**: Not pinned (use el-get's built-in recipes)
3. **Single-file packages**: Not versioned (raw URLs)
4. **MELPA packages**: Not pinned (rolling release)

### To Pin a MELPA Package

MELPA packages can be pinned by adding them to `package-pinned-packages`:

```elisp
(add-to-list 'package-pinned-packages '(package-name . "melpa-stable"))
```

Or by converting them to `el-get-bundle` with `:checkout`.

---

## Duplicate Packages

The following packages are installed in both `el-get/` and `elpa/`:

| Package | el-get Version | elpa Version | Load Priority |
|---------|---------------|--------------|---------------|
| ace-window | 📌 `77115afc` | 🔄 `20220911.358` | el-get |
| avy | 📌 `933d1f36` | 🔄 `20241101.1357` | el-get |
| dash | 🔄 recipe | 🔄 `20250312.1307` | el-get |
| hydra | 📌 `59a2a45a` | 🔄 `20250316.1254` | el-get |
| markdown-mode | 📌 `7c51a216` | 🔄 `20250624.631` | el-get |
| reformatter | 📌 `6ac08ceb` | 🔄 `20241204.1051` | el-get |
| restclient | 📌 `e2a2b134` | 🔄 `20250629.2016` | el-get |

These duplicates exist because some packages are installed via el-get (for pinning) but also declared in `use-package` with `:ensure t` (which installs from MELPA). The el-get version takes precedence in the load path.

---

*Last updated: 2026-05-11*
*Generated from: early-init.el (el-get), init.el (use-package)*
