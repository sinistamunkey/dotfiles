# Emacs Configuration

A modular Emacs configuration optimized for Python development with sophisticated LSP integration, xwidget HTML preview, PlantUML support, and session persistence.

## Requirements

### macOS

**GUI Emacs with xwidget support is required** for rich HTML preview functionality. The standard `emacs-app` cask does not include xwidget support.

Install emacs-mac (recommended):
```bash
brew tap railwaycat/emacsmacport
brew install --cask emacs-mac
```

Alternative (build from source with more options):
```bash
brew tap d12frosted/emacs-plus
brew install emacs-plus@30 --with-xwidgets --with-imagemagick
ln -s /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications/Emacs.app
```

### Verify xwidget support

```bash
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval "(princ (if (fboundp 'xwidget-webkit-browse-url) \"xwidget-supported\" \"xwidget-not-supported\"))"
```

Should output: `xwidget-supported`

## Installation

```bash
# From the dotfiles root directory:
./install.sh
```

The script will:
- Create a timestamped backup of existing `.emacs.d`
- Symlink this configuration to `~/.emacs.d`
- Packages will auto-install on first launch via `use-package`

## Features

### Python Development
- **Poetry integration**: Automatic virtualenv detection and LSP setup
- **Eglot LSP**: Uses `python-lsp-server` with ruff for linting/formatting
- **Tree-sitter**: Enhanced syntax highlighting with `python-ts-mode`
- **Auto-formatting**: Ruff format on save with import sorting

**Commands:**
- `M-x my-setup-poetry-venv` - Configure Poetry environment
- `M-x ruff-format-buffer` - Format Python code

### HTML Preview
- **Xwidget webkit**: Embedded browser for rich HTML preview
- `C-c C-v` in org-mode or html-mode - Preview in xwidget
- `C-c p` in org-mode - Toggle org-preview-html-mode (uses xwidget)
- `C-c w` globally - Browse URL in xwidget

### PlantUML Integration
- Jar file location: `~/plantuml.jar`
- Org-babel integration for PlantUML code blocks
- Automatic inline image display
- Image scaling: 420px width

### Org-mode
- Auto-insert templates with author/export settings
- Markdown export backend enabled
- Enhanced navigation with imenu
- HTML export optimized for xwidget preview

### Session Persistence
- **Desktop Save Mode**: Automatically saves and restores open files and window configuration
- **Persistent Scratch**: The `*scratch*` buffer persists across sessions
- **Auto-Revert**: Buffers automatically refresh when files change on disk

### SQL Development
- SQL mode configuration with proper indentation
- Connection management for database work

## Configuration Structure

### Module Loading Order
1. `ui.el` - Visual configuration and themes
2. `navigation.el` - IDO, completion, snippets
3. `development.el` - Git, LSP, company mode
4. `python-config.el` - Python-specific development
5. `languages.el` - Other language modes
6. `org-mode.el` - Org-mode and PlantUML
7. `platform.el` - Platform-specific settings
8. `functions.el` - Custom helper functions
9. `images.el` - GUI image display
10. `xwidget-config.el` - Xwidget webkit browser
11. `sql-config.el` - SQL development setup

### Key Custom Functions

**Python:**
- `my-poetry-venv-path()` - Get Poetry virtualenv path
- `my-get-pylsp-command()` - Find appropriate pylsp
- `my-setup-poetry-venv()` - Configure Poetry environment
- `ruff-format-buffer()` - Format with ruff

**HTML Preview:**
- `my-org-preview-xwidget()` - Export org to HTML and preview
- `my-html-preview-xwidget()` - Preview HTML files

**GitHub Integration:**
- `my-browse-file-on-github()` - Open current file on GitHub (`C-c g b`)
- `my-copy-github-url()` - Copy GitHub URL to clipboard (`C-c g c`)

**Scratch Files:**
- `my-new-scratch` - Create named scratch file (`C-c s n`)
- `my-list-scratches` - Browse and open scratch files (`C-c s l`)
- Scratch files stored in `~/.emacs.d/scratch/`
- File extension determines major mode (e.g., `.py`, `.json`, `.md`)

**Utilities:**
- `uuid()` - Generate and insert UUID
- `my-copy-buffer-file-path()` - Copy full file path (`C-c f p`)
- `my-copy-buffer-file-name()` - Copy file name only (`C-c f n`)
- `my-insert-directory-tree()` - Insert tree output (`C-c d t`)

## Adding Configuration

### New Packages
```elisp
(use-package package-name
  :config
  (configuration-here))
```

### New Modules
1. Create `config/new-module.el`
2. Add `(provide 'new-module)` at end
3. Add `(require 'new-module)` to `init.el`

## Key Bindings Reference

### Python Development
- `M-x my-setup-poetry-venv` - Configure Poetry environment
- `M-x ruff-format-buffer` - Format Python code

### HTML/Org Preview
- `C-c C-v` - Preview in xwidget (org-mode or html-mode)
- `C-c p` - Toggle org-preview-html-mode (org-mode)
- `C-c w` - Browse URL in xwidget (global)

### GitHub Integration
- `C-c g b` - Browse file on GitHub
- `C-c g c` - Copy GitHub URL

### File Utilities
- `C-c f p` - Copy full file path
- `C-c f n` - Copy file name
- `C-c d t` - Insert directory tree

### Scratch Files
- `C-c s n` - Create new scratch file
- `C-c s l` - List and open scratch files

### Snippets
- `C-c i` - Insert yasnippet

## Platform-Specific

### macOS
- `exec-path-from-shell` for PATH synchronization
- Native clipboard integration via pbcopy/pbpaste
- GUI Emacs with xwidget support for embedded webkit
- Key modifiers: Option=Meta, Command=Super

## Notes

- Configuration uses lexical binding throughout for performance
- Packages auto-install via `use-package` on first launch
- All modules fail gracefully if dependencies missing
- Poetry detection works from any subdirectory of a project
- Optimized for GUI Emacs with xwidget support
- Session state (open files, window layout) persists across restarts
- Scratch buffer content persists in `~/.emacs.d/.emacs-scratch`
