# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal dotfiles repository containing configurations for Emacs and Tmux, with an automated installation script that creates symlinks and backups.

## Documentation Structure

This repository uses modular documentation that mirrors the directory structure:

- **This file (root):** Repository overview, installation, adding new tools
- **[emacs/CLAUDE.md](emacs/CLAUDE.md):** Detailed Emacs configuration
  - Architecture and module system
  - Python/Poetry/LSP integration
  - Custom functions and keybindings
  - PlantUML and xwidget setup
- **tmux/.tmux.conf:** Tmux configuration (inline comments)
  - Development session configs in `.tmux/` directory

## Installation

```bash
# Install all configurations with automatic backup
./install.sh
```

The install script:
- Creates timestamped backups in `~/.dotfiles_backup_YYYYMMDD_HHMMSS/`
- Symlinks `emacs/.emacs.d` to `~/.emacs.d`
- Symlinks `tmux/.tmux.conf` to `~/.tmux.conf`
- Symlinks `tmux/.tmux` to `~/.tmux` (if exists)

## Repository Structure

### Emacs Configuration (`emacs/`)
See `emacs/CLAUDE.md` for detailed Emacs-specific documentation.

**Key highlights:**
- Modular architecture with focused config files in `config/`
- Sophisticated Python/Poetry/LSP integration
- Xwidget webkit support for HTML preview
- PlantUML integration for diagrams
- Custom functions for GitHub integration, UUID generation, directory trees

### Tmux Configuration (`tmux/`)

**Main config:** `tmux/.tmux.conf`
- Prefix key: `C-a` (instead of default `C-b`)
- Window indexing starts at 1
- Emacs-style copy mode keybindings
- Mouse support enabled
- Pane navigation: `M-Left/Right/Up/Down`
- Pane resizing: `C-M-Left/Right/Up/Down`
- Split panes: `|` (vertical), `-` (horizontal)
- iTerm2 image protocol passthrough enabled
- Status bar with hostname and datetime
- Plugins via TPM: sensible, resurrect

**Development session:** `tmux/.tmux/development.conf`
- Preset layout with Emacs + 2 terminal panes
- Load with: `bind S` (i.e., `C-a S`)
- Creates session named "Development" with:
  - Top pane: Emacs (75% height)
  - Bottom-left: Terminal (50% width)
  - Bottom-right: Terminal (50% width)

## Adding New Configurations

### New Tool Configuration
1. Create directory for tool (e.g., `zsh/`, `vim/`)
2. Add configuration files with dotfile prefix (e.g., `.zshrc`)
3. Update `install.sh`:
   - Add install function (e.g., `install_zsh()`)
   - Add directory check and function call in `main()`
   - Use `create_symlink` helper for symlinking

### Extending Existing Configs
- **Emacs:** Add modules to `emacs/.emacs.d/config/` (see `emacs/CLAUDE.md`)
- **Tmux:** Edit `.tmux.conf` or add session configs to `.tmux/` directory

## Platform Support

Currently optimized for macOS:
- Emacs configuration uses `exec-path-from-shell` for PATH sync
- Emacs GUI with xwidget support (emacs-mac or emacs-plus)
- Tmux configured for iTerm2 integration

## Important Files to Preserve

- `install.sh`: Core installation logic
- `emacs/.emacs.d/init.el`: Emacs entry point
- `emacs/.emacs.d/config/`: All Emacs modules
- `tmux/.tmux.conf`: Main tmux configuration
