# Emacs Configuration

## A Modern, Comprehensive Emacs Setup

## Overview

This repository contains my personal Emacs configuration - a modern, feature-rich setup designed for productivity, development, and writing. The configuration emphasizes:

- **Performance**: Optimized startup times and responsive editing experience
- **Modularity**: Well-organized, maintainable structure with clear separation of concerns
- **Modern Tooling**: Integration with contemporary development tools and AI assistants
- **Custom Workflows**: Tailored workflows for different programming languages and writing tasks

## Quick Start

### Prerequisites
- Emacs 31.0 (the `master` branch)
- Git
- Basic Unix tools (curl, wget, etc.)

### Installation

```shell
# Clone the repository
git clone https://github.com/fuzy112/dotemacs.git ~/.config/emacs

# Run the bootstrap script
cd ~/.config/emacs
./bootstrap.sh
```

### First Run

After installation, start Emacs. The configuration will:
1. Install all required packages automatically
2. Set up development environments
3. Configure keybindings and themes
4. Ready your development workspace

To verify all external dependencies are available, run `M-x check-deps` to check for required system programs and their versions.

## Configuration Structure

### Core Files

| File            | Purpose                                  |
|-----------------|------------------------------------------|
| `init.el`       | Main entry point, loads all modules      |
| `early-init.el` | Early initialization, performance tweaks |
| `packages.el`   | Package management with straight.el      |
| `custom.el`     | Custom variables and settings            |

### Module Organization

The configuration is organized into logical modules:

- **`site-lisp/`**: Custom Emacs Lisp packages and utilities
- **`straight/`**: Package repositories managed by straight.el
- **`scripts/`**: Utility scripts and automation tools
- **`templates/`**: Code and file templates

## Key Features

### Package Management
- Uses `straight.el` for reproducible package management
- Pinned package versions for stability
- Automatic package installation and updates

### Development Environments

#### Programming Languages
- Python, JavaScript/TypeScript, Rust, Go, Java, C/C++
- LSP support via `eglot`
- Comprehensive debugging and testing integration

#### Writing and Documentation
- Org-mode with enhanced productivity features
- Markdown support with live preview
- LaTeX integration for academic writing

### AI Integration

#### GPTel Configuration (`site-lisp/gptel-config.el`)
- Multiple AI backends: Moonshot (Kimi), DeepSeek, Kagi, GitHub Copilot
- Extensive tool integration for filesystem operations, web search, and development
- Specialized presets for different use cases:
  - `coding-agent`: File operations and project management
  - `deepseek-translator`: High-precision translation
  - `deepseek-mathematician`: Mathematical problem solving
  - `kimi-assistant`: General assistance with web search

#### Available Tools
- File operations: `read_file`, `write_file`, `edit_file`, `create_file`
- Web search: `search_web`, `read_url`
- Development: `run_command`, `grep`, `editor_diagnostics`
- GitHub integration: Pull request review and approval

### User Interface

#### Themes and Appearance
- Multiple theme support with automatic dark/light switching
- Custom font configurations and scaling
- Minimal, distraction-free UI

#### Navigation and Editing
- Enhanced buffer management
- Advanced file navigation with `vertico` and `consult`
- Smart completion with `corfu` and `cape`
- Modal editing with `meow`

### Modal Editing with Meow

- **Dvorak-optimized**: Custom keybindings for Dvorak keyboard layout
- **Leader key**: `SPC` for extended commands with intuitive mappings:
  - `SPC <letter>` → `C-c <letter>`
  - `SPC x` → `C-x`
  - `SPC h` → `C-h`
  - `SPC m <letter>` → `M-<letter>`
  - `SPC g <letter>` → `C-M-<letter>`
- **Visual feedback**: Mode state indicator for awareness
- **Clipboard integration**: Seamless copy/paste operations
- **Custom workflows**: Optimized for efficient text editing

### Performance Optimizations

- Native compilation for faster execution
- Lazy loading of heavy packages
- Memory usage monitoring and optimization
- Startup time profiling and improvements

## Customization

### Adding New Packages

Add package recipes to `packages.el`:

```emacs-lisp
(straight-use-package 'package-name)
```

### Configuration Modules

Create new configuration modules in `site-lisp/`:

```emacs-lisp
;;; module-name.el --- Description -*- lexical-binding: t; -*-

;; Your configuration here

(provide 'module-name)
```

## Usage Guide

### Basic Workflow

1. **Modal Editing**: Use Meow for modal editing (Dvorak layout)
2. **Project Navigation**: Use project.el integration (`C-c p f` for project files)
3. **Code Editing**: Eglot-enabled editing with Corfu completions
4. **Version Control**: Magit integration (`C-x g` for quick status)
5. **AI Assistance**: `M-x gptel-agent` for AI-powered coding help
6. **Buffer Management**: `C-x b` or `C-c b` to switch buffers
7. **Search**: `M-s r` for ripgrep search, `M-s l` for line search

### Development Workflows

#### Python Development
- Virtual environment management
- pytest integration
- Black formatting

#### Web Development
- JavaScript/TypeScript support
- npm/yarn integration

### Common Commands

| Command                         | Description                     | Keybinding           |
|---------------------------------|---------------------------------|----------------------|
| **Global Commands**             |                                 |                      |
| `windmove-left`                 | Move window left                | `S-<left>`           |
| `windmove-right`                | Move window right               | `S-<right>`          |
| `windmove-up`                   | Move window up                  | `S-<up>`             |
| `windmove-down`                 | Move window down                | `S-<down>`           |
| `capitalize-dwim`               | Capitalize word                 | `M-c`                |
| `downcase-dwim`                 | Lowercase word                  | `M-l`                |
| `upcase-dwim`                   | Uppercase word                  | `M-u`                |
| `project-recompile`             | Recompile project               | `<f5>`               |
| **Navigation**                  |                                 |                      |
| `consult-buffer`                | Switch buffers                  | `C-x b` or `C-c b`   |
| `consult-project-buffer`        | Switch project buffers          | `C-x p b`            |
| `consult-bookmark`              | Jump to bookmark                | `C-x r b`            |
| `consult-goto-line`             | Go to line                      | `M-g g` or `M-g M-g` |
| `consult-imenu`                 | Navigate to symbol              | `M-g i`              |
| `consult-imenu-multi`           | Navigate multi-file symbols     | `M-g I`              |
| `consult-mark`                  | Jump to mark                    | `M-g m`              |
| `consult-global-mark`           | Jump to global mark             | `M-g k`              |
| `consult-outline`               | Navigate outline                | `M-g o`              |
| `consult-compile-error`         | Navigate compile errors         | `M-g e`              |
| `consult-flymake`               | Show diagnostics                | `M-g f`              |
| **Search**                      |                                 |                      |
| `consult-grep`                  | Search with grep                | `M-s g`              |
| `consult-git-grep`              | Search in git repo              | `M-s G`              |
| `consult-ripgrep`               | Search with ripgrep             | `M-s r`              |
| `consult-ugrep`                 | Search with ugrep               | `M-s R`              |
| `consult-line`                  | Search line in buffer           | `M-s l`              |
| `consult-line-multi`            | Search line multi-buffer        | `M-s L`              |
| `consult-find`                  | Find file                       | `M-s d`              |
| `consult-fd`                    | Find file with fd               | `M-s D`              |
| `consult-locate`                | Find file with locate           | `M-s c`              |
| `consult-keep-lines`            | Keep matching lines             | `M-s k`              |
| `consult-focus-lines`           | Focus on matching lines         | `M-s u`              |
| `consult-isearch-history`       | Browse isearch history          | `M-s e`              |
| **Version Control**             |                                 |                      |
| `magit-status-quick`            | Quick Git status                | `C-x g`              |
| `magit-dispatch`                | Magit commands                  | `C-x M-g`            |
| `magit-file-dispatch`           | Magit file commands             | `C-c M-g`            |
| `magit-project-status`          | Project magit status            | `C-c p m`            |
| **Development**                 |                                 |                      |
| `find-function`                 | Find function definition        | `C-x F`              |
| `find-variable`                 | Find variable definition        | `C-x V`              |
| `find-function-on-key`          | Find function bound to key      | `C-x K`              |
| `find-library`                  | Find library                    | `C-x L`              |
| `consult-yank-pop`              | Browse yank history             | `M-y`                |
| `consult-register-load`         | Load register                   | `M-#`                |
| `consult-register-store`        | Store register                  | `M-'`                |
| **Applications**                |                                 |                      |
| `org-agenda`                    | Open agenda                     | `C-c A`              |
| `org-capture`                   | Quick note capture              | `C-c C`              |
| `elfeed`                        | RSS feed reader                 | `C-c E`              |
| `telega`                        | Telegram client                 | `C-c T`              |
| `gnus`                          | Email and news reader           | `C-c G`              |
| `vundo`                         | Visual undo                     | `C-c V`              |
| `p-search`                      | Project search                  | `C-c P`              |
| `embark-act`                    | Contextual actions              | `C-c a`              |
| `gptel`                         | GPTel menu                      | `C-c t A`            |
| `gptel-menu`                    | GPTel quick menu                | `C-c t a`            |
| **Window Management**           |                                 |                      |
| `+toggle-side-window-left`      | Toggle left side window         | `C-c w h`            |
| `+toggle-side-window-right`     | Toggle right side window        | `C-c w l`            |
| `+toggle-side-window-above`     | Toggle top side window          | `C-c w k`            |
| `+toggle-side-window-below`     | Toggle bottom side window       | `C-c w j`            |
| `window-layout-flip-topdown`    | Flip window layout top-down     | `C-c w f 2`          |
| `window-layout-flip-leftright`  | Flip window layout left-right   | `C-c w f 3`          |
| `eat-other-window`              | Eat terminal in other window    | `C-x 4 t`            |
| `+eshell/other-window`          | Eshell in other window          | `C-x 4 e`            |
| **Vertico**                     |                                 |                      |
| `vertico-repeat`                | Repeat last vertico command     | `M-R`                |
| `vertico-repeat-previous`       | Repeat previous vertico command | `M-P`                |
| `vertico-repeat-next`           | Repeat next vertico command     | `M-N`                |
| `vertico-directory-enter`       | Enter directory in vertico      | `RET` in vertico     |
| `vertico-directory-delete-char` | Delete char in directory        | `DEL` in vertico     |
| `vertico-directory-delete-word` | Delete word in directory        | `M-DEL` in vertico   |
| `vertico-quick-insert`          | Quick insert in vertico         | `C-q` in vertico     |
| `vertico-quick-exit`            | Quick exit from vertico         | `M-q` in vertico     |
| **Corfu**                       |                                 |                      |
| `corfu-quick-insert`            | Quick insert in corfu           | `C-q` in corfu       |
| `corfu-quick-complete`          | Quick complete in corfu         | `M-q` in corfu       |
| `corfu-next`                    | Next completion in corfu        | `TAB` in corfu       |
| `corfu-previous`                | Previous completion in corfu    | `S-TAB` in corfu     |

## Maintenance

### Updating Packages

- **`C-c S x f`** (`straight-x-fetch-all`)
- **`C-c S U`** (`+straight-review-updated-repos`)

### Troubleshooting

1. Check `*Messages*` buffer for error messages
2. Use `M-x toggle-debug-on-error` to debug issues
3. Review `custom.el` for custom settings
4. Check package conflicts with `straight-check-all`

### Backup and Sync

The configuration includes:
- Automatic backup of custom settings
- Version-controlled configuration files
- Easy migration between systems

## Contributing

This is a personal configuration, but contributions and suggestions are welcome. Please:

1. Follow the existing code style and organization
2. Add appropriate documentation
3. Test changes thoroughly
4. Update the ChangeLog with your changes

## License

This configuration is distributed under the GNU General Public License v3.0. See the `LICENSE` file for details.

## Acknowledgments

- The Emacs community for excellent packages and documentation
- Prot authors for inspiration and best practices
- Various Emacs configuration repositories for reference

## Support

For issues and questions:

- Check the `AGENTS.md` file for development guidelines
- Review existing GitHub issues
- Create a new issue with detailed information

## Changelog

See `ChangeLog` for detailed history of changes and improvements.
