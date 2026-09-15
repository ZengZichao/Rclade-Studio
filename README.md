# Rclade Studio

A native macOS wrapper for the [`Rclade`](https://github.com/ZengZichao/Rclade) R package. Rclade Studio does not duplicate any plotting logic; it discovers the R installation on your Mac, verifies dependencies, launches the bundled enhanced Shiny engine on a local port, and presents it as a real App window via `WKWebView`.

[`Rclade`](https://github.com/ZengZichao/Rclade) R 包的 macOS 原生客户端。Rclade Studio 自身不复制任何绘图逻辑：它自动发现系统中的 R、校验依赖、在本地端口启动内置的增强 Shiny 引擎，并通过 `WKWebView` 将其呈现为一个真正的 App 窗口。

---

## Highlights / 主要特性

- **Drag-and-drop trees / 拖入即用**: Drop a single `.nwk`, `.tre`, `.nex`, or `.nexus` file onto the window; the app records the real path so the exported code is reproducible.  
  将单个 `.nwk`、`.tre`、`.nex` 或 `.nexus` 树文件拖入窗口即可自动载入，并保留真实路径以便导出代码可复现。
- **Live preview / 实时预览**: Parameters and tree object are merged reactively with a 450 ms debounce; errors are shown inline.  
  参数与树对象响应式合并，450 ms 防抖自动重绘；错误以内联方式展示。
- **Reproducible code export / 可复现代码导出**: Only non-default parameters are exported; running the script in a clean R session reproduces the same figure.  
  仅导出非默认参数，在干净 R 会话中运行即可得到同一张图。
- **Bilingual UI / 中英双语界面**: Follows macOS system language by default; switch between Chinese and English at any time from the header or Settings menu.  
  默认跟随 macOS 系统语言；可随时通过标题栏或"设置"菜单在中英文之间切换。
- **Local-only / 完全本地**: The engine binds to `127.0.0.1` only; no network access, no external CDN or font requests.  
  引擎仅监听 `127.0.0.1`，不联网、无外部 CDN 或字体请求。

---

## System Requirements / 系统要求

- macOS ≥ 11 (Big Sur). PDF/PNG export requires macOS ≥ 11.3.  
  macOS ≥ 11；导出 PDF/PNG 需要 macOS ≥ 11.3。
- A working **R (≥ 4.1)** installation with the **`Rclade`** package installed.  
  已安装 **R (≥ 4.1)** 且已安装 **`Rclade`** 包。

```r
install.packages("Rclade")
```

`Rclade` will pull in Bioconductor dependencies such as `ggtree`, `deeptime`, and `treeio` automatically.  
`Rclade` 会自动安装 `ggtree`、`deeptime`、`treeio` 等 Bioconductor 依赖。

---

## Download / 下载

Grab the latest release from the [Releases](https://github.com/ZengZichao/Rclade-Studio/releases) page.  
从 [Releases](https://github.com/ZengZichao/Rclade-Studio/releases) 页面下载最新版本。

Current version: **v0.1.0**  
当前版本：**v0.1.0**

### First launch / 首次打开

Because the app is signed with an ad-hoc signature and is not notarized, macOS Gatekeeper may show "RcladeStudio is damaged" or "cannot be opened". To allow it:  
由于应用使用 ad-hoc 签名且未公证，macOS Gatekeeper 可能提示"已损坏，无法打开"或"无法验证开发者"。解决方法：

1. Right-click `RcladeStudio.app` → **Open** → confirm.  
   右键点击 `RcladeStudio.app` → **打开** → 确认。
2. Or run in Terminal:  
   或在终端执行：

```bash
xattr -dr com.apple.quarantine /path/to/RcladeStudio.app
```

---

## Build from Source / 从源码构建

```bash
./build_app.sh             # build RcladeStudio.app
open dist/RcladeStudio.app
./scripts/release.sh       # package dist/RcladeStudio-0.1.0-arm64.zip
```

Run the round-trip test in an environment with R + Rclade:  
在装有 R + Rclade 的环境中运行往返测试：

```bash
cd tests && Rscript test_roundtrip.R
```

---

## Project Structure / 项目结构

```
Sources/RcladeStudio/
  main.swift            # NSApp, window, menus, DropWebView, navigation/download delegates
  RLocator.swift        # Discover Rscript across frameworks / Homebrew / conda / mamba
  EngineProcess.swift   # Start/stop the R subprocess, log capture, crash recovery
  SingleInstance.swift  # Single-instance lock + activate existing instance
Resources/
  launch_engine.R       # Dependency check → load engine_app.R → runApp + handshake
  engine_app.R          # Enhanced Shiny engine UI/server
  AppIcon.icns          # App icon
Info.plist              # Bundle metadata (v0.1.0)
build_app.sh            # swiftc → assemble .app → ad-hoc codesign
scripts/release.sh      # Build .app + user guide → zip
tests/test_roundtrip.R  # Reproducibility tests
LICENSE                 # MIT
```

---

## Citation / 引用

Rclade Studio is a companion client for the **Rclade** R package. If you use this software in your research, please cite:  
Rclade Studio 是 **Rclade** R 包的配套客户端。如在研究中使用本软件，请引用：

> **Rclade**: automated taxonomic collapsing and geological-timescale annotation of time-calibrated phylogenetic trees in R.  
> https://github.com/ZengZichao/Rclade  
> DOI: https://doi.org/10.64898/2026.08.27.747462

A `CITATION` file is also included in this repository.  
本仓库同时包含 `CITATION` 文件。

---

## License / 许可

MIT License — see [LICENSE](LICENSE).  
MIT 许可证 — 详见 [LICENSE](LICENSE)。

---

## Acknowledgements / 致谢

Rclade Studio bundles [Shiny](https://shiny.posit.co/) for the UI layer and relies on [Rclade](https://github.com/ZengZichao/Rclade), [ape](https://cran.r-project.org/package=ape), [ggtree](https://bioconductor.org/packages/ggtree), and [deeptime](https://github.com/willgearty/deeptime) for tree parsing, plotting, and geological timescale annotation. Each component retains its own license.  
Rclade Studio 内置 [Shiny](https://shiny.posit.co/) 作为 UI 层，并依赖 [Rclade](https://github.com/ZengZichao/Rclade)、[ape](https://cran.r-project.org/package=ape)、[ggtree](https://bioconductor.org/packages/ggtree) 和 [deeptime](https://github.com/willgearty/deeptime) 完成树解析、绘图及地质时间轴标注。各组件保留其原有许可。
