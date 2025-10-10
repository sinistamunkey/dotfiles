#!/bin/bash

# Dotfiles Installation Script
# Automatically symlinks configuration files to their proper locations

set -e

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKUP_DIR="$HOME/.dotfiles_backup_$(date +%Y%m%d_%H%M%S)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

backup_file() {
    local file="$1"
    if [[ -e "$file" ]]; then
        mkdir -p "$BACKUP_DIR"
        cp -r "$file" "$BACKUP_DIR/"
        log_warn "Backed up existing $file to $BACKUP_DIR"
    fi
}

create_symlink() {
    local source="$1"
    local target="$2"
    
    # Create target directory if it doesn't exist
    mkdir -p "$(dirname "$target")"
    
    # Backup existing file/directory
    backup_file "$target"
    
    # Remove existing file/symlink
    rm -rf "$target"
    
    # Create symlink
    ln -sf "$source" "$target"
    log_info "Linked $source -> $target"
}

# Install Emacs configuration
install_emacs() {
    log_info "Installing Emacs configuration..."
    create_symlink "$DOTFILES_DIR/emacs/.emacs.d" "$HOME/.emacs.d"
}

# Install Tmux configuration
install_tmux() {
    log_info "Installing Tmux configuration..."
    create_symlink "$DOTFILES_DIR/tmux/.tmux.conf" "$HOME/.tmux.conf"
    
    if [[ -d "$DOTFILES_DIR/tmux/.tmux" ]]; then
        create_symlink "$DOTFILES_DIR/tmux/.tmux" "$HOME/.tmux"
    fi
}

# Main installation function
main() {
    log_info "Starting dotfiles installation from $DOTFILES_DIR"
    
    # Check if we're in the right directory
    if [[ ! -d "$DOTFILES_DIR/emacs" ]] && [[ ! -d "$DOTFILES_DIR/tmux" ]]; then
        log_error "This doesn't appear to be a dotfiles directory"
        exit 1
    fi
    
    # Install configurations
    if [[ -d "$DOTFILES_DIR/emacs" ]]; then
        install_emacs
    fi
    
    if [[ -d "$DOTFILES_DIR/tmux" ]]; then
        install_tmux
    fi
    
    log_info "Dotfiles installation complete!"
    
    if [[ -d "$BACKUP_DIR" ]]; then
        log_info "Backups saved to: $BACKUP_DIR"
    fi
}

# Run main function
main "$@"