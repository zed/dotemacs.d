# AGENTS.md

This document describes the Emacs configuration repository and provides guidance for AI agents working with this codebase.

## Repository Overview

This is a personal Emacs configuration (dotemacs.d) for a developer who works with Python, TypeScript, Go, Rust, C/C++, and various other languages. The configuration emphasizes:

- **Keyboard-driven workflows** using Hydra transient menus
- **Completion frameworks** (ivy/counsel/swiper ecosystem)
- **Org-mode** as a central hub for notes, agendas, and literate programming
- **Modern development tools** (LSP, tree-sitter, formatters, linters)
- **AI-assisted coding** (gptel, pi-coding-agent)

## File Structure

### Core Files

| File | Purpose |
|------|---------|
| `early-init.el` | Bootstraps package management (el-get + use-package), installs all packages, optimizes startup time |
| `init.el` | Main configuration file with all package settings, keybindings, and customizations |
| `~/.secrets.el.gpg` | Encrypted secrets (API keys, credentials) - not in this repo |
| `~/.custom.el` | Emacs customizations UI settings - not in this repo |

### Key Sections in init.el

1. **Package Configuration** - use-package declarations for ~100+ packages
2. **Completion & Navigation** - ivy, counsel, swiper, avy, embark, consult
3. **Version Control** - magit, forge, git-gutter, git-timemachine, smerge
4. **Org-mode Setup** - Agenda, capture, babel (with jupyter), export, habits
5. **Language Support** - Python, TypeScript, Go, Rust, C/C++, PHP, Zig, TLA+
6. **AI Integration** - gptel for LLM interactions, pi-coding-agent
7. **UI & UX** - Themes, fonts, keybindings, window management

## Package Management

This configuration uses **two package managers**:

### el-get (primary for bootstrapping)
- Used in `early-init.el` for packages that need to be available before init.el
- Recipes defined inline with `el-get-bundle!` macro
- Handles packages from GitHub, ELPA, and direct URLs

```elisp
(el-get-bundle package-name
  :type github
  :pkgname "owner/repo")
```

### use-package (primary for configuration)
- Used in `init.el` for declarative package configuration
- Always ensure packages are installed (`use-package-always-ensure t`)
- Integrates with system-package for binary dependencies

```elisp
(use-package package-name
  :ensure-system-package (binary-name . "install command")
  :hook (mode . function)
  :bind (:map keymap ("key" . command))
  :custom (variable value))
```

## Development Environment

### Python Development
- **IDE**: Elpy (not python-ts-mode)
- **Formatting**: blacken or ruff-format (auto-detected based on pyproject.toml)
- **Linting**: flymake-ruff (when pyproject.toml has [tool.ruff.lint])
- **Notebook**: code-cells + ox-ipynb for .ipynb ↔ .org conversion
- **Virtual envs**: pet mode for automatic virtualenv detection

### TypeScript/JavaScript Development
- **Mode**: typescript-ts-mode / tsx-ts-mode (tree-sitter based)
- **LSP**: eglot with typescript-language-server
- **Formatting**: prettier-js on save

### LSP Support
- Uses **eglot** (built-in) rather than lsp-mode
- Configured for TypeScript/TSX
- Auto-shutdown when buffers close

### Code Formatting
- Python: blacken or ruff-format (project-dependent)
- TypeScript/JS: prettier-js
- Rust: rust-format-on-save
- C/C++: custom style "my-style" based on Python indentation

## Keybindings & Navigation

### Hydra Transient Menus
The configuration extensively uses Hydra for discoverable keybindings:

| Binding | Hydra | Purpose |
|---------|-------|---------|
| `C-c w` | hydra-window | Window management |
| `C-c m` | hydra-multiple-cursors | Multiple cursors |
| `C-c r` | hydra-rectangle | Rectangle operations |
| `C-c` | hydra-zoom | Text scaling |
| `\` (in PDF) | hydra-pdftools | PDF navigation |

### Core Navigation
| Binding | Command | Purpose |
|---------|---------|---------|
| `C-s` | counsel-grep-or-swiper | Search in buffer |
| `M-x` | counsel-M-x | Execute command |
| `M-s` | avy-goto-word-1 | Jump to visible word |
| `C-=` | er/expand-region | Expand selection |
| `C-.` | embark-act | Context actions |
| `C-c p` | projectile-command-map | Project commands |

### Search & Completion
- **ivy**: Completion framework (configured with fuzzy matching via flx)
- **counsel**: Enhanced versions of Emacs commands
- **swiper**: Search with preview
- **embark**: Context-sensitive actions on completion candidates
- **consult**: Enhanced search and navigation commands

## Org-mode Configuration

### Core Settings
- Todo workflow: `TODO → NEXT → WAIT → DONE | DEFERRED | CANCELED`
- Clock persistence enabled
- Habits tracking enabled
- MobileOrg sync configured (via Dropbox)

### Babel (Code Execution)
Languages enabled for code blocks:
- Python (with jupyter integration)
- Shell/Bash
- Emacs Lisp
- PlantUML
- R
- SQLite
- REST client

### Jupyter Integration
```elisp
;; Default jupyter-python settings
(:results . "replace")
(:async . "yes")
(:session . "py3.12")
(:kernel . "python3")
```

### Org Capture Templates
| Key | Name | Target |
|-----|------|--------|
| `t` | Todo | inbox.org → Tasks |
| `j` | Journal | journal.org (datetree) |

### Export Formats
Enabled backends: md, odt, latex, icalendar, html, ascii
Additional: gfm (GitHub Markdown), jira, slack

## AI Integration

### gptel
- Default mode: org-mode
- Model and backend configured in ~/.secrets.el.gpg
- Auto-renames buffers to query summary after response

### pi-coding-agent
- Integrated as a use-package
- See pi documentation for capabilities

## Custom Functions

### Slick Copy/Cut
- `M-w` (kill-ring-save): Copies current line if no region active
- `C-w` (kill-region): Kills current line if no region active

### Smart Keyboard Quit
- `C-g`: Closes minibuffer if open, even when not focused

### Other Helpers
- `init:chmod+x-files-with-shebang`: Auto-makes scripts executable on save
- `init:vc-off-if-remote`: Disables vc for TRAMP files (performance)
- `init:toggle-frame-fullscreen`: Fixed fullscreen toggle

## Environment Variables

| Variable | Purpose |
|----------|---------|
| `ORG_MOBILE_INBOX_FOR_PULL` | MobileOrg inbox file path |
| `ORG_MOBILE_DIRECTORY` | MobileOrg directory |
| `NAND2TETRIS_CORE_BASE_DIR` | nand2tetris project location |

## Common Tasks

### Adding a New Package

1. If needed before init.el, add to `early-init.el`:
```elisp
(el-get-bundle package-name
  :type github
  :pkgname "owner/repo")
```

2. Otherwise, add to `init.el`:
```elisp
(use-package package-name
  :ensure t
  :hook (major-mode . my-hook)
  :custom (setting value))
```

### Modifying Keybindings

Global keybindings are in the `my-keys-minor-mode` map at the end of init.el:

```elisp
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "key") #'command)
    map))
```

Package-specific bindings use `:bind` in use-package declarations.

### Adding a New Language

1. Install the mode (if not built-in)
2. Configure in a `use-package` block
3. Add to `auto-mode-alist` if needed
4. Set up LSP if desired (eglot)
5. Configure formatting/linting hooks

## Testing Changes

- Use `emacs --debug-init` to see errors with backtraces
- `M-x use-package-report` shows package load statistics
- `C-h v` and `C-h f` to inspect variables and functions
- Evaluate expressions with `C-x C-e` after the closing paren

## Conventions

### Naming
- Internal functions prefixed with `init:` (e.g., `init:pet-mode-hook`)
- Some legacy functions use `qpet/` prefix
- Hydra bodies named `hydra-*`

### Configuration Style
- Prefer `:custom` keyword in use-package for setting variables
- Use `with-eval-after-load` or `:config` for lazy configuration
- Hooks added via `:hook` keyword when possible
- System dependencies declared with `:ensure-system-package`

### Comments
- Sections marked with `;; *` (level 1) and `;; **` (level 2)
- Code blocks separated by `;; ***` comments
- Original sources credited in comments when adapting code

## Dependencies

### System Packages
Required binaries (installed via system package manager or pipx):
- `ripgrep` (rg)
- `sqlite3`
- `git`
- `shellcheck`
- `hunspell` + dictionaries
- `black` / `black-macchiato` (via pipx)
- `typescript-language-server`
- `node` / `npm`

### Emacs Version
- Requires Emacs 27+ (for various features)
- Uses tree-sitter (Emacs 29+) for TypeScript modes
- Prefers built-in eglot over lsp-mode

## Troubleshooting

### Package Installation Failures
- Check `*Messages*` buffer for errors
- Try `M-x el-get-reload` or `M-x package-refresh-contents`
- Verify network connectivity to MELPA/GitHub

### Performance Issues
- Check `M-x use-package-report` for slow packages
- GC settings are optimized for startup; runtime GC threshold is 8MB
- Remote files have vc disabled automatically

### Keybinding Conflicts
- `my-keys-minor-mode` overrides major mode bindings
- Check `C-h k` followed by the key to see what's bound
- Use `C-h b` to see all bindings in current buffer
