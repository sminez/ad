# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- Update the behaviour of `*` / double-click cursor expansion to support dashes so the behaviour
  is correct for built-in commands (#170)


## [0.4.0] - 2025-12-15

### Added

- **LSP Features**
  - LSP completion support with Insert mode binding (`A-<space>`)
  - LSP formatting support
  - LSP rename support
  - LSP `textDocument/didSave` notification
  - LSP hover displayed in transient scratch buffers
  - Autostart behaviour for LSP servers
  - Bash LSP configuration

- **Editor Features**
  - Scratch buffer (toggleable)
  - Command for saving all open buffers (`:wa`)
  - Mouse-based UI layout resizing for windows and columns
  - Bracketed paste support (#136)
  - Transient scratch buffers
  - Support for custom INSERT mode keybindings and `send_keys` command
  - Support for renaming open buffers via fsys (#77)
  - Add `plumb` command for directly plumbing strings
  - Process control for running child processes with output streaming
  - Running status indicator in status bar
  - UI column and window balancing methods

- **Configuration**
  - Allow overriding config load path (#159)
  - Support for specifying colorscheme by path
  - Support for `C-/A-/C-A-` modifiers in custom bindings
  - Support for overriding built-in key bindings
  - Arrow key support in custom keybindings (#158)
  - Renamed `languages` config section to `filetypes`

- **Syntax Highlighting**
  - Regex-based highlighting as an alternative to tree-sitter

- **Scripting & Exec**
  - Running external programs in exec scripts
  - Template variable for current match position (ROW/COL)

- **Documentation**
  - Expand docs to cover: minibuffer, mouse usage, windows/columns, virtual buffers, jump list, selecting text, debug commands, process control, fsys files

- **Testing & Quality**
  - Scenario-based testing framework with fsys support
  - Reference editing tests
  - Fuzz testing for regex compile, address parsing, gap buffer operations, editor command parsing, plumbing rules

- **Infrastructure**
  - Built-in 9p client (removes external `9p` binary dependency)
  - Add a simple watcher program (`ad_watch`)
  - Support for namespacing 9p sockets by PID (#111)
  - Updated to Rust 2024 edition

- **Performance**
  - Improved GapBuffer performance with direct byte offset lookups
  - Reduced allocations throughout: TUI renderer, exec program execution, 9p protocol layer, GapBuffer operations
  - Aho-Corasick based fast literal prefix search for regex

### Changed

- Moved to lexopt for CLI parsing
- Renamed command environment variables (removed `bufname` env var)
- Buffer filetype exposed through fsys
- POSIX line semantics enforced
- Improved minibuffer UI for empty/whitespace input
- Fast mouse wheel scrolling detection
- Exec string templating changed from `$1` to `{1}`

### Fixed

- Incorrect handling of compound address parsing
- Panic in parsing counted repetitions in regex compile
- OOB panic when stripping unused ops from compiled regex programs
- Panic in GapBuffer when removing multibyte characters
- LSP TextEdit handling for deleting single characters
- UTF-16 LSP position handling
- Jumplist jumps now clamp cursor position
- Terminal mouse events with wide characters
- Integer overflow when mapping screen coords to cursor
- Focus state issues when opening new windows (#117)


## [0.3.1] - 2025-02-20

### Fixed

- Broken match-indent behaviour

### Changed

- FreeBSD tests now only run when manually triggered
- Use `/dev/null` as test path (#93)


## [0.3.0] - 2025-02-19

### Added

- **LSP Support**
  - Initial LSP client implementation
  - LSP goto (definition, declaration, implementation, type definition, references)
  - LSP diagnostics
  - Document synchronization
  - Server capabilities display
  - Position encoding handling

- **Tree-sitter Syntax Highlighting**
  - Full tree-sitter based syntax highlighting
  - Token-based rendering with caching
  - Pretty printing of tree-sitter syntax trees
  - Standard directory structure for tree-sitter queries

- **Configuration**
  - TOML-based configuration (replacing init.conf)
  - Tree-sitter and LSP config pulled from config file

- **Infrastructure**
  - xtask for generating man pages
  - xtask for setting up dotfiles

### Changed

- Renamed `doc/` directory to `docs/`
- Improved clipboard provider setup (#61)

### Fixed

- Incorrect handling of newline insert updating tree-sitter state
- Tree-sitter state updates for insert with selection
- Handling of skipped tokens due to horizontal scrolling
- Tab rendering issues (#66, #68, #69)
- Trailing line whitespace background colour
- UTF-8 input over stdin (#65)
- Focused buffer update after closing windows/columns (#52)
- Keybindings preserved when updating config
- ANSI escape code escaping when rendering
- xclip `-quiet` flag removed
