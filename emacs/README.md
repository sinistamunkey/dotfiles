# Emacs Configuration Analysis

## Overview

This is a modular Emacs configuration designed for software development, particularly optimized for Python development with Poetry integration. The configuration uses modern tools like `use-package` for declarative package management and is split into focused modules for maintainability. Optimized for GUI Emacs with xwidget support for rich HTML preview and embedded webkit browser.

## Architecture

### Configuration Structure

```
.emacs.d/
├── init.el              # Entry point - loads all modules
├── custom.el            # Auto-generated customizations
├── config/              # Modular configuration files
│   ├── ui.el           # Visual appearance and themes
│   ├── navigation.el   # File navigation and completion
│   ├── development.el  # Development tools (LSP, Git)
│   ├── python-config.el # Python-specific configuration
│   ├── languages.el    # Other language modes
│   ├── org-mode.el     # Org-mode and PlantUML
│   ├── platform.el     # Platform-specific settings
│   ├── functions.el    # Custom helper functions
│   ├── images.el       # GUI image display
│   └── xwidget-config.el # Xwidget webkit browser
├── org-templates/      # Org-mode file templates
│   └── export-setup.org # Default export headers
├── snippets/           # YASnippet templates
└── elpa/              # Installed packages
```

### Module Loading Order

The modules are loaded in a specific order to handle dependencies:

1. **ui.el** - Visual configuration
2. **navigation.el** - IDO and completion
3. **development.el** - LSP and Git tools
4. **python-config.el** - Python development
5. **languages.el** - Other language support
6. **org-mode.el** - Org-mode features
7. **platform.el** - Platform-specific code
8. **functions.el** - Custom functions
9. **images.el** - GUI image display
10. **xwidget-config.el** - Xwidget webkit browser

## Core Customizations

### Package Management (init.el:6-22)

- **Package Archives**: GNU ELPA and MELPA
- **use-package**: Automatic installation with `use-package-always-ensure t`
- **Automatic Setup**: Packages are installed declaratively

### Session Management (init.el:24-29)

- **global-auto-revert-mode**: Automatically refresh buffers when files change on disk
- **desktop-save-mode**: Save and restore Emacs sessions (open files, window configuration)

## Module-by-Module Breakdown

### 1. UI Configuration (ui.el)

#### Visual Cleanup
- Disabled startup message, toolbar, menu bar, scroll bar
- Disabled lock files and backup files
- Enabled line numbers globally

#### Mouse Support (ui.el:14-18)
- Terminal mouse support via `xterm-mouse-mode`
- Mouse wheel scrolling in terminal Emacs

#### Window Management (ui.el:26-27)
- Prevents vertical splits (only horizontal splits)
- Width threshold of 120 characters for splitting

#### Theme (ui.el:34-38)
- **nord-theme**: Nordic color scheme
- Auto-loaded after initialization

#### Additional Packages
- **nyan-mode**: Fun progress indicator (Nyan cat in modeline)
- **auto-dim-other-buffers**: Dims inactive buffers for focus

#### Time Display (ui.el:30-31)
- Format: "Day DD Mon HH:MM" (e.g., "Wed 01 Oct 13:45")

### 2. Navigation (navigation.el)

#### IDO Mode (navigation.el:3-6)
- Flexible matching for file/buffer navigation
- Enabled everywhere in Emacs

#### Better Defaults (navigation.el:9)
- `better-defaults` package for sensible Emacs defaults

#### Snippets (navigation.el:12-15)
- **yasnippet-snippets**: Code snippet expansion
- Globally enabled

#### Custom Dired Sidebar (navigation.el:18-35)
- Custom function `my-dired-sidebar-toggle`
- Toggle with **F8**
- 35-character width sidebar
- Hidden file details for cleaner view

#### Keybindings
- **F8**: Toggle dired sidebar
- **C-c =**: Balance window sizes

### 3. Development Tools (development.el)

#### Version Control (development.el:4)
- **magit**: Git interface for Emacs

#### Code Style (development.el:7-9)
- **fill-column-indicator**: Visual line at column 88 (Python PEP 8 extended)

#### LSP Configuration (development.el:12-21)
- **eglot**: Built-in LSP client
- Tab completion integration
- Completion-at-point functions configured

#### Autocompletion (development.el:24-26)
- **company-mode**: Minimal configuration
- 0.5 second delay before suggestions
- 2 character minimum prefix

### 4. Python Configuration (python-config.el)

This is the most sophisticated module with extensive Python development support.

#### Python 2.7 Support (python-config.el:6-11, 17-36)
- **jedi**: Autocomplete for Python 2.7 projects
- Auto-detection via `.python-version` file
- Falls back to `python-mode` for Python 2.7
- Keybindings: M-. (goto-definition), M-, (go back)

#### Tree-sitter Integration (python-config.el:14, 25-27)
- Uses `python-ts-mode` when available for better syntax
- Font lock level 4 (maximum highlighting)

#### Poetry Integration (python-config.el:38-70)

##### Poetry Detection Functions
- `my-poetry-venv-path()`: Gets Poetry virtualenv path
- `my-get-pylsp-command()`: Finds pylsp (prefers Poetry venv)
- `my-setup-poetry-venv()`: Complete Poetry environment setup

##### Auto-LSP Installation
- Detects missing `python-lsp-server` in Poetry environments
- Prompts to install `python-lsp-server[all]` and `python-lsp-ruff`
- Automatic configuration of pylsp with ruff

#### LSP Configuration (python-config.el:65-69, 176-184)
- Uses **pylsp** (python-lsp-server)
- Configured with **ruff** for linting and formatting
- Disables conflicting tools: pycodestyle, pyflakes, mccabe
- Dynamic pylsp command selection (Poetry venv > global)

#### Ruff Formatting (python-config.el:73-117)

##### `ruff-format-buffer` Function
- Runs `ruff check --fix` first (linting + import sorting)
- Then runs `ruff format` (code formatting)
- Poetry-aware: uses `poetry run ruff` in Poetry projects
- Preserves cursor position and window scroll
- Automatic on save (except Python 2.7)

#### Jedi for Python 2.7 (python-config.el:120-136)

##### `my-setup-jedi-python2` Function
- Auto-detection and installation of jedi for Python 2.7
- Sets `python-shell-interpreter` to "python2.7"
- Configures completion for older Python projects

#### Python Mode Hooks (python-config.el:139-188)
- Automatic Poetry environment setup
- Fill column indicator at 88
- Trailing whitespace deletion on save
- Conditional LSP startup (skips Python 2.7)
- Auto-formatting with ruff (modern Python only)

### 5. Language Support (languages.el)

#### JSON Mode (languages.el:4-9)
- **json-mode**: JSON syntax highlighting
- 2-space indentation

#### YAML Mode (languages.el:12)
- **yaml-mode**: YAML syntax support

#### Markdown Mode (languages.el:15-34)
- **markdown-mode**: Markdown editing
- **gfm-mode**: GitHub-flavored Markdown for README files
- Automatic markdown processor detection: pandoc > multimarkdown > markdown
- Live preview mode (C-c m g)
- Visual line mode for better text wrapping

### 6. Org-mode Configuration (org-mode.el)

#### Auto-insert Templates (org-mode.el:4-15)
- Automatic template insertion for new org files
- Template includes TITLE and AUTHOR fields
- Export headers loaded from `org-templates/export-setup.org`
- Includes LaTeX, HTML, and export configuration
- Template features: Arial font, booktabs tables, minted code highlighting

#### PlantUML Integration (org-mode.el:17-58)
- **plantuml-mode**: UML diagram support
- JAR location: `~/plantuml.jar`
- Org-babel integration for PlantUML code blocks
- Automatic inline image display
- No confirmation prompts for PlantUML evaluation
- 2-space indentation

#### Export Backends (org-mode.el:60-63)
- Markdown export enabled (`ox-md`)
- HTML export without syntax highlighting (avoids encoding issues)

#### HTML Preview (org-mode.el:66-70)
- **org-preview-html**: Live HTML preview with xwidget
- Refreshes on save

#### Navigation and Display (org-mode.el:72-83)
- **imenu**: Header navigation support
- **visual-line-mode**: Word wrapping at word boundaries

#### Keybindings (org-mode.el:85-91)
- **C-c l**: Store link
- **C-c a**: Agenda
- **C-c c**: Capture
- **C-c p**: HTML preview mode
- **C-c i**: Imenu navigation
- **C-c j**: Org goto

#### Image Display (images.el:33-42)
- Image width: 420px
- `org-refresh-images()` function for manual refresh

### 7. Platform Integration (platform.el)

#### macOS PATH Sync (platform.el:4-7)
- **exec-path-from-shell**: Synchronizes PATH from shell
- Works with Terminal.app and iTerm2

#### macOS Key Modifiers (platform.el:9-13)
- Option key mapped to Meta (Alt in Emacs)
- Command key mapped to Super
- Matches OS-level keyboard remapping for external keyboards

#### Clipboard Integration (platform.el:15-34)
- Custom pbcopy/pbpaste integration
- Works in both GUI and terminal Emacs
- Bidirectional clipboard sync with macOS

### 8. Custom Functions (functions.el)

#### UUID Generator (functions.el:3-9)
- `uuid()`: Generate and insert UUID
- Automatically lowercases output

#### File Path Utilities (functions.el:11-27)
- `my-copy-buffer-file-path()`: Copy full file path to clipboard
- `my-copy-buffer-file-name()`: Copy filename only to clipboard

#### Directory Tree Insertion (functions.el:75-85)
- `my-insert-directory-tree()`: Insert ASCII file tree at point
- Uses `tree` command with configurable depth
- Default depth: 2 levels
- Prefix argument changes depth (e.g., `C-u 3 C-c d t` for 3 levels)

#### GitHub Integration (functions.el:29-73)
- `my-get-github-url()`: Get GitHub URL for current file on current branch
- `my-browse-file-on-github()`: Open file on GitHub in browser
- `my-copy-github-url()`: Copy GitHub URL to clipboard
- Supports both SSH and HTTPS remote URLs
- Automatically detects current git branch

#### Keybindings (functions.el:87-93)
- **C-c i**: Insert YASnippet
- **C-c f p**: Copy full file path
- **C-c f n**: Copy filename only
- **C-c d t**: Insert directory tree
- **C-c g b**: Browse file on GitHub
- **C-c g c**: Copy GitHub URL

### 9. Image Display (images.el)

#### GUI Image Display (images.el)
- Uses built-in `auto-image-file-mode` for GUI Emacs
- Supports PNG, JPG, JPEG, GIF formats
- Animated GIF looping enabled
- Image-dired thumbnails: 150px, 4 per row

### 10. Xwidget Webkit Browser (xwidget-config.el)

#### HTML Preview Functions
- `my-org-preview-xwidget()`: Export org to HTML and preview in embedded webkit
- `my-html-preview-xwidget()`: Preview HTML files in embedded browser
- Automatic buffer naming and window management

#### Keybindings
- **C-c C-v**: Preview current org/HTML file in xwidget (mode-specific)
- **C-c w**: Browse URL in xwidget webkit browser (global)

### 11. Custom Settings (custom.el)

#### Automatically Set Variables
- **auto-dim-other-buffers-mode**: Enabled
- **display-time-mode**: Enabled
- **python-shell-interpreter**: "python"
- **tool-bar-mode**: Disabled
- Suppressed Python shell completion warnings

#### Custom Faces
- **Default font**: Menlo, height 140
- **Background**: #2B2B2B (dark gray)
- **Foreground**: #a9b7c6 (light gray-blue)
- **Fringe**: Unspecified (inherits from background)

## Key Features Summary

### Python Development
1. **Dual Python Version Support**: Python 2.7 (jedi) and Python 3.x (LSP)
2. **Poetry Integration**: Automatic virtualenv detection and configuration
3. **Modern Tooling**: Ruff for linting and formatting
4. **LSP Support**: pylsp with ruff integration
5. **Tree-sitter**: Advanced syntax highlighting for Python 3.x
6. **Auto-formatting**: Format on save with cursor preservation

### Terminal Optimization
1. **Mouse Support**: Full mouse support in terminal
2. **Image Display**: iTerm2 inline image protocol
3. **Clipboard Integration**: macOS clipboard sync
4. **Session Persistence**: Desktop save mode

### Development Workflow
1. **Git Integration**: Magit for version control
2. **LSP**: Eglot for language server protocol
3. **Snippets**: YASnippet for code templates
4. **Completion**: Company mode with minimal configuration

### Org-mode Features
1. **PlantUML**: Diagram creation in org files
2. **HTML Preview**: Live preview mode
3. **Navigation**: Sidebar and imenu support
4. **Export**: Markdown export backend

### UI/UX Enhancements
1. **Nord Theme**: Nordic color scheme
2. **Nyan Mode**: Fun modeline indicator
3. **Auto-dim**: Focus on active buffer
4. **IDO Mode**: Flexible file/buffer navigation
5. **Custom Sidebar**: F8 toggle dired sidebar

## Notable Design Decisions

### Modular Architecture
- Each configuration aspect is isolated in its own module
- Clean separation of concerns
- Easy to maintain and extend

### Terminal-First Design
- All features work in terminal Emacs
- iTerm2 protocol for advanced features
- Graceful fallbacks for GUI mode

### Python-Centric
- Extensive Python configuration
- Poetry workflow optimization
- Backward compatibility with Python 2.7

### Minimal Interference
- No backup files or lock files
- Automatic whitespace cleanup
- Conservative completion delays

### Smart Defaults
- Better-defaults package
- Fill column at 88 (Python best practice)
- Horizontal-only splits for wide screens

## Dependencies

### Required External Tools
- **Poetry**: Python dependency management
- **ruff**: Python linting and formatting (optional but recommended)
- **pylsp**: Python LSP server
- **plantuml.jar**: UML diagram generation
- **pandoc/multimarkdown**: Markdown processing (optional)

### macOS Specific
- **pbcopy/pbpaste**: Clipboard integration (built-in macOS)
- **iTerm2**: Advanced terminal features (optional)

### Optional Tools
- **imgcat/catimg**: Image display in tmux
- **ImageMagick/Ghostscript**: PDF conversion (commented out)
- **pdfinfo**: PDF metadata (commented out)

## Global Keybindings Reference

| Keybinding | Function | Description |
|------------|----------|-------------|
| **F8** | my-dired-sidebar-toggle | Toggle file tree sidebar |
| **C-c =** | balance-windows | Balance window sizes |
| **C-c i** | yas-insert-snippet | Insert code snippet |
| **C-c f p** | my-copy-buffer-file-path | Copy full file path |
| **C-c f n** | my-copy-buffer-file-name | Copy filename only |
| **C-c d t** | my-insert-directory-tree | Insert directory tree |
| **C-c g b** | my-browse-file-on-github | Browse file on GitHub |
| **C-c g c** | my-copy-github-url | Copy GitHub URL |
| **C-c w** | xwidget-webkit-browse-url | Browse URL in xwidget |
| **C-c l** | org-store-link | Store org-mode link |
| **C-c a** | org-agenda | Open org agenda |
| **C-c c** | org-capture | Org capture |
| **C-c p** | org-preview-html-mode | HTML preview (org-mode) |
| **C-c i** | imenu | Imenu navigation (org-mode) |
| **C-c j** | org-goto | Org navigation |
| **C-c C-v** | my-org-preview-xwidget | Preview org/HTML in xwidget |

### Python-Specific (via Jedi)
| Keybinding | Function | Description |
|------------|----------|-------------|
| **M-.** | jedi:goto-definition | Jump to definition (Python 2.7) |
| **M-,** | jedi:goto-definition-pop-marker | Go back (Python 2.7) |

## Customization Philosophy

This configuration demonstrates several best practices:

1. **Declarative Package Management**: Using `use-package` for clean, readable configuration
2. **Graceful Degradation**: Features fail gracefully if dependencies are missing
3. **Context-Aware**: Different behavior for Python 2.7 vs 3.x, terminal vs GUI, etc.
4. **Non-Intrusive**: Minimal automatic behavior, user control prioritized
5. **Performance**: Lazy loading, minimal startup configuration
6. **Cross-Platform Awareness**: macOS-specific code isolated in platform.el

## Recent Notable Customizations

### GUI Emacs Optimization
- Xwidget webkit support for embedded browser and HTML preview
- Native image display for PNG, JPG, JPEG, GIF
- Removed terminal-specific workarounds (iTerm2 image protocol)

### Org-mode Template System
- External template file (`org-templates/export-setup.org`)
- Automatic insertion of export headers for new org files
- Easy maintenance of shared export configuration
- Visual line mode for word wrapping

### GitHub Integration
- Browse current file on GitHub (`C-c g b`)
- Copy GitHub URL to clipboard (`C-c g c`)
- Supports both SSH and HTTPS remotes
- Auto-detects current branch

### macOS Key Mapping
- Option key as Meta, Command key as Super
- Matches OS-level keyboard remapping for external keyboards

### Ruff Integration Enhancement
- Preserves cursor position and window scroll during formatting
- Prevents buffer from reverting to top after format
- Two-step process: lint fixes then formatting

### Session Persistence
- Automatic desktop save without prompts
- Restores open files and window configuration

## Conclusion

This is a sophisticated, production-ready Emacs configuration that balances modern development practices with backward compatibility. The modular architecture makes it easy to understand, maintain, and extend. The configuration is particularly strong for Python development while providing solid support for other languages and workflows.
