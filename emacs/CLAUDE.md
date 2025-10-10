# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

This is a modular Emacs configuration using a sophisticated architecture with `use-package` for declarative package management. The configuration is split into focused modules in the `config/` directory, each handling specific aspects of the editing environment.

**Key architectural principle**: Each module is self-contained and can be loaded independently via `(require 'module-name)` from `init.el`.

## Development Commands

### Installation
```bash
# Install entire dotfiles configuration with automatic backup
./install.sh
```

### Emacs Package Management
- Packages are automatically installed via `use-package` with `setq use-package-always-ensure t`
- Package sources: GNU ELPA and MELPA
- No manual package installation needed - add new packages using `use-package` declarations

### Python Development
```elisp
;; Auto-install LSP server in Poetry environments
M-x my-setup-poetry-venv

;; Format Python code with ruff (includes import sorting)
M-x ruff-format-buffer

;; Manual Eglot start (usually automatic)
M-x my-python-eglot-ensure
```

## Configuration Structure

### Core Files
- `init.el`: Entry point that loads all modules in specific order
- `custom.el`: Emacs custom settings (auto-generated)
- `config/`: Modular configuration directory
- `org-templates/`: Org-mode file templates (export headers, etc.)

### Module Loading Order (important for dependencies)
1. `ui.el` - Visual configuration and themes
2. `navigation.el` - IDO, completion, snippets
3. `development.el` - Git, LSP, company mode
4. `python-config.el` - Python-specific development setup
5. `languages.el` - Other language modes
6. `org-mode.el` - Org-mode and PlantUML
7. `platform.el` - Platform-specific settings
8. `functions.el` - Custom helper functions
9. `images.el` - GUI image display
10. `xwidget-config.el` - Xwidget webkit browser for HTML preview

## Python Development Integration

This configuration has sophisticated Python support:

### Poetry Integration
- Automatically detects `pyproject.toml` files
- Sets up virtual environment paths for `pyvenv-activate` and `python-shell-virtualenv-root`
- Uses Poetry-aware ruff commands: `poetry run ruff` vs global `ruff`
- Auto-prompts to install `python-lsp-server[all]` and `python-lsp-ruff` in Poetry environments

### LSP Configuration
- Uses `eglot` (built-in) with `pylsp` (python-lsp-server)
- Dynamic pylsp command detection (Poetry venv > global)
- Configured with ruff for linting/formatting, disables pycodestyle/pyflakes/mccabe
- Automatic formatting with ruff on save via `after-save-hook`

### Tree-sitter Support
- Uses `python-ts-mode` when available for better syntax highlighting
- Falls back to standard `python-mode` gracefully

## Key Custom Functions

### Python Development
- `my-poetry-venv-path()`: Gets Poetry virtualenv path for current project
- `my-get-pylsp-command()`: Finds appropriate pylsp (Poetry venv preferred)
- `my-setup-poetry-venv()`: Configures entire Poetry environment
- `ruff-format-buffer()`: Formats with import sorting, Poetry-aware
- `my-eglot-python-contact()`: Dynamic eglot contact function

### HTML Preview
- `my-org-preview-xwidget()`: Export org file to HTML and preview in xwidget-webkit
- `my-html-preview-xwidget()`: Preview HTML files in embedded webkit browser
- Keybinding: `C-c C-v` in org-mode or html-mode to preview
- Keybinding: `C-c w` globally to browse URL in xwidget

### GitHub Integration
- `my-browse-file-on-github()`: Open current file on GitHub for current branch
- `my-copy-github-url()`: Copy GitHub URL for current file to clipboard
- Keybinding: `C-c g b` to browse file on GitHub
- Keybinding: `C-c g c` to copy GitHub URL

### Utility Functions
- `uuid()`: Generate and insert UUID
- `my-copy-buffer-file-path()`: Copy full file path to clipboard (`C-c f p`)
- `my-copy-buffer-file-name()`: Copy file name to clipboard (`C-c f n`)
- `my-insert-directory-tree()`: Insert directory tree at point using `tree` command (`C-c d t`)

## PlantUML Integration

- Jar file location: `~/plantuml.jar`
- Org-babel integration for PlantUML code blocks
- Automatic inline image display in org-mode
- Image scaling set to 420px width

## Platform-Specific Features

### macOS Integration
- `exec-path-from-shell` for PATH synchronization
- Native clipboard integration via pbcopy/pbpaste
- GUI Emacs with xwidget support for embedded webkit browser
- Key modifier mappings: Option key as Meta, Command key as Super (matches OS keyboard settings)

## Adding New Configuration

### New Packages
Add to appropriate module file using pattern:
```elisp
(use-package package-name
  :config
  (configuration-here))
```

### New Modules
1. Create `config/new-module.el`
2. Add `(provide 'new-module)` at end
3. Add `(require 'new-module)` to `init.el` in appropriate order

### Python Tools
- LSP tools: Add to Poetry with `poetry add --group dev tool-name`
- Global tools: Use pipx or system package manager
- Always test both Poetry and non-Poetry workflows

## File Organization Patterns

- **UI/Visual**: `config/ui.el`
- **Development Tools**: `config/development.el` 
- **Language-specific**: `config/LANGUAGE-config.el`
- **Custom Functions**: `config/functions.el`
- **Platform code**: `config/platform.el`
- **Snippets**: `snippets/MODE-NAME/`

## Important Notes

- Configuration uses lexical binding throughout for performance
- Git ignores generated files (elpa/, auto-save-list/, etc.)
- Install script creates symlinks and automatic backups
- All modules are designed to fail gracefully if dependencies missing
- Poetry detection works from any subdirectory of a Poetry project
- Optimized for GUI Emacs with xwidget support for rich HTML preview
