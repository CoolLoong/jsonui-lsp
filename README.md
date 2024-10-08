## JSON-UI LSP

### Introduction
This VSCode extension enhances the creation of Minecraft Bedrock JSON-UI by providing intelligent auto-completion features. It speeds up the development process and helps developers efficiently create and edit JSON-UI files.
<img src=".github/img/show1.gif" width="60%"/>

### Features
- **Property and Value Auto-Completion**: Offers efficient auto-completion for JSON-UI control properties and values.
- **Binding Completion**: Provides smart suggestions for property bindings.
- **Color Palette Integration**: Allows quick and easy selection of color variables with an integrated color palette.
- **Fast and Lightweight Completion Feedback**: Delivers quick and efficient feedback with minimal memory usage.
- **Multi-Platform Support**: Compatible with Windows, macOS, and Linux.
- **Multi-Language Completion Labels**: Supports completion labels in both Chinese and English.

### Contributing
Contributions are welcome!

#### Complie guide
Development IDE with `vscode`, prepare `rust` and `nodejs` environment.
Install pnpm package manager.
1. `pnpm i`
2. use nightly rust feature
 `rustup toolchain install nightly`
 `rustup override set nightly`
5. `cargo build`
6. debug use `F5`

### Roadmap
- Implement variable navigation (requires extended context)
- Add auto-completion for operation expressions in bindings
- ...

### License
This project is licensed under the MIT License.
