## JsonUI LSP

[![CI](https://github.com/CoolLoong/jsonui-lsp/actions/workflows/ci.yml/badge.svg)](https://github.com/CoolLoong/jsonui-lsp/actions/workflows/ci.yml)
[![GitHub License](https://img.shields.io/github/license/CoolLoong/jsonui-lsp)](LICENSE)
[![Discord](https://img.shields.io/discord/1304443010439970876?label=Discord&logo=discord)](https://discord.gg/S2ZKgGusjk)
[![Minecraft - Version](https://img.shields.io/badge/minecraft-v1.21.80.3_(Bedrock)-black)](https://feedback.minecraft.net/hc/en-us/articles)
[![Ko-Fi](https://img.shields.io/badge/Buy_Me_a_Coffee-f37574?logo=kofi&logoColor=white)](https://patreon.com/coolloong)

### Introduction
This VSCode extension enhances the creation of Minecraft Bedrock JSON-UI by providing intelligent auto-completion features. It speeds up the development process and helps developers efficiently create and edit JSON-UI files.  
<img src=".github/img/show1.gif" width="60%"/>

### Features

- **Intelligent Control Type and Value Suggestions**
- **Binding Type and Value Completion**
- **Context Variables Auto-Completion**
- **References lookup for Control Names**
- **Goto-Definition for Control Names**
- **Color Palette Integration**
- **Fast and Lightweight Completion Feedback**
- **Multi-Platform Support**
- **Multi-Language Completion Labels**

### Contributing
Contributions are welcome!

#### Complie guide
Development IDE with `vscode`, prepare `rust` and `nodejs` environment. Install `bun` package manager.
1. `bun i -D webpack-cli`
2. `bun i`
3. use nightly rust feature
 `rustup toolchain install nightly`
 `rustup override set nightly`
1. debug use `F5`

### License
This project is licensed under the MIT License.