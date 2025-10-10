# Dotfiles

Personal development environment configuration for macOS, featuring a modular Emacs setup and optimized Tmux configuration.

## Quick Start

```bash
# Clone the repository
git clone <repository-url> ~/dotfiles
cd ~/dotfiles

# Install all configurations
./install.sh
```

The install script will:
- Create timestamped backups of existing configurations
- Symlink all dotfiles to their proper locations
- Preserve your existing setup in `~/.dotfiles_backup_YYYYMMDD_HHMMSS/`

## What's Included

### 🎨 Emacs Configuration
A sophisticated, modular Emacs setup optimized for Python development with LSP, xwidget HTML preview, and session persistence.

**Highlights:**
- 🐍 Advanced Python support with Poetry/LSP integration
- 🌐 Embedded webkit browser for HTML preview
- 📊 PlantUML diagram support
- 🔧 GitHub integration (browse files, copy URLs)
- 💾 Session persistence (files, windows, scratch buffer)
- 📝 Scratch file management system
- 🎯 SQL development tools

**[→ Full Emacs Documentation](emacs/README.md)**

### 🖥️ Tmux Configuration
Streamlined terminal multiplexer setup with sensible defaults and productivity enhancements.

**Highlights:**
- Custom prefix: `C-a` (instead of `C-b`)
- Intuitive pane navigation with Alt+Arrows
- Quick pane resizing with Ctrl+Alt+Arrows
- Mouse support enabled
- Development session preset (`C-a S`)
- Plugins: TPM, sensible, resurrect

**Key Bindings:**
- `C-a |` - Split vertically
- `C-a -` - Split horizontally
- `M-Left/Right/Up/Down` - Navigate panes
- `C-M-Left/Right/Up/Down` - Resize panes
- `C-a S` - Load development session layout

## Installation Details

### Emacs Setup
The script symlinks `emacs/.emacs.d` → `~/.emacs.d`

**Requirements:**
- GUI Emacs with xwidget support (emacs-mac or emacs-plus)
- Packages auto-install on first launch via `use-package`

```bash
# Install emacs-mac (recommended for macOS)
brew tap railwaycat/emacsmacport
brew install --cask emacs-mac
```

### Tmux Setup
The script symlinks:
- `tmux/.tmux.conf` → `~/.tmux.conf`
- `tmux/.tmux` → `~/.tmux`

**First-time Tmux setup:**
1. Launch tmux
2. Install TPM plugins: `C-a I` (that's capital i)

## Directory Structure

```
dotfiles/
├── install.sh              # Automated installation script
├── emacs/                  # Emacs configuration
│   ├── .emacs.d/
│   │   ├── init.el        # Entry point
│   │   ├── config/        # Modular configuration files
│   │   └── org-templates/ # Org-mode templates
│   └── README.md          # Detailed Emacs documentation
└── tmux/                   # Tmux configuration
    ├── .tmux.conf         # Main configuration
    └── .tmux/             # Additional configs and plugins
        └── development.conf
```

## Key Features

### Emacs Module Architecture
Configuration is split into focused modules:
- `ui.el` - Visual themes and appearance
- `navigation.el` - IDO, completion, snippets
- `development.el` - Git, LSP, company mode
- `python-config.el` - Python/Poetry/LSP integration
- `languages.el` - Other language modes
- `org-mode.el` - Org-mode and PlantUML
- `platform.el` - macOS-specific settings
- `functions.el` - Custom utilities
- `xwidget-config.el` - Embedded webkit browser
- `sql-config.el` - SQL development

### Tmux Development Session
Pre-configured layout with Emacs + terminal panes:
```
┌─────────────────────────┐
│                         │
│    Emacs (75%)         │
│                         │
├───────────┬─────────────┤
│  Terminal │  Terminal   │
└───────────┴─────────────┘
```

Load with: `C-a S`

## Adding New Tools

To add configuration for additional tools (zsh, vim, etc.):

1. Create a directory for the tool:
   ```bash
   mkdir -p newtool
   ```

2. Add configuration files with dotfile prefix:
   ```bash
   # Example: newtool/.newtoolrc
   ```

3. Update `install.sh`:
   ```bash
   install_newtool() {
       log_info "Installing NewTool configuration..."
       create_symlink "$DOTFILES_DIR/newtool/.newtoolrc" "$HOME/.newtoolrc"
   }

   # Add to main() function:
   if [[ -d "$DOTFILES_DIR/newtool" ]]; then
       install_newtool
   fi
   ```

## Platform Support

Currently optimized for **macOS**:
- Emacs PATH synchronization via `exec-path-from-shell`
- Native clipboard integration (pbcopy/pbpaste)
- iTerm2 integration for tmux
- GUI Emacs with xwidget support

## Backup & Recovery

All existing configurations are automatically backed up before installation:
- Backup location: `~/.dotfiles_backup_YYYYMMDD_HHMMSS/`
- Original files are never deleted
- Safe to run multiple times

To restore previous configuration:
```bash
# Example: restore previous .emacs.d
rm ~/.emacs.d
cp -r ~/.dotfiles_backup_20241010_165700/.emacs.d ~/
```

## Documentation

- **[Emacs Configuration](emacs/README.md)** - Detailed guide with all features and keybindings
- **[Emacs Development Guide](emacs/CLAUDE.md)** - Architecture and development instructions
- **[Repository Guide](CLAUDE.md)** - Contributing and extending dotfiles

## License

Personal configuration files - use and modify as you wish.
