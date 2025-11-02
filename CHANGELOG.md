# Change Log
Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

## [0.3.0] - 2025-11-02

### Added
- Added benchmark support for performance profiling
- Added configuration management system
- Added document manager for better file handling
- Added navigation state management
- Support for Minecraft Bedrock Edition 1.21.110

### Changed
- Major code refactoring for improved maintainability and performance
  - Split completer module into multiple specialized submodules (completer, indexer, types)
  - Reorganized resource files from `src/resources` to `resources` directory
- Enhanced museair integration with improved error handling
- Optimized parser with better tree-sitter integration
- Improved utility functions for better code reusability

### Removed
- Removed deprecated `file_queue` module
- Removed unused `stringpool` module
- Removed test JSON files from crate directory

## [0.2.1] - 2025-05-30

### Changed
- Fix incorrect target location in goto definition
- Fix navigating for controls with extends controls
- Fix completion for newly created UI files

## [0.2.0] - 2025-05-21

### Changed
- Major project refactoring with significant improvements in code performance and memory usage
- Replaced built-in parser with `Tree-sitter` parser
- Use `Rope` for document caching instead of built-in Document cache
- Updated UI definitions for Bedrock Edition 1.21.80

### Added
- Implemented reference lookup feature for control name
  
## [0.1.1] - 2024-12-11
### Changed
- Update Bedrock 1.21.50

## [0.0.9~0.1.0] - 2024-11-10

### Added

- Implement goto-definition for control name

## [0.0.8] - 2024-11-08

### Added

- Implement variable IntelliSense

### Changed

- Significant project refactoring, optimizing the completion mechanism and performance.

## [0.0.x] - 2024-10-01

- Initial release of jsonui-lsp