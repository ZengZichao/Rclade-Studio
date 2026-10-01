# 贡献指南 / Contributing

感谢关注 Rclade Studio！项目目前由个人维护，欢迎 Issue 与 PR。

## 提交变更

1. 从 `main` 切出分支，命名如 `feat/xxx`、`fix/xxx`、`docs/xxx`、`ci/xxx`；
2. `main` 受分支保护：**所有变更需通过 Pull Request**，且 CI（`R round-trip test` 与 `macOS app smoke build`）必须通过；
3. PR 保持小而聚焦，合并统一采用 squash；
4. 提交信息遵循仓库现有风格：`type: 摘要`（`ci:` / `fix:` / `docs:` / `chore:`）。

## 本地开发

- 前置：macOS ≥ 11、Xcode 命令行工具、R ≥ 4.1 且已 `install.packages("Rclade")`；
- 构建 App：`./build_app.sh`（产物在 `dist/RcladeStudio.app`；非破坏式，旧构建自动移至 `.old-*`）；
- 打包 Release zip：`./scripts/release.sh`；
- R 端测试：`Rscript tests/test_roundtrip.R`（CI 在 Ubuntu 上跑同一测试）。

## 报告问题

优先使用 Issue 模板（bug / feature）。
**安全问题请勿公开提交**，见 [SECURITY.md](SECURITY.md)。
