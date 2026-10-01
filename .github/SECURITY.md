# 安全政策 / Security Policy

## 支持的版本 / Supported Versions

| 版本 / Version | 支持 / Supported |
| --- | --- |
| latest release（v0.1.0 起） | ✅ |

## 报告漏洞 / Reporting a Vulnerability

**请勿在公开 Issue 中披露安全细节。**

请通过 GitHub 的私有漏洞报告渠道私下提交：
**https://github.com/ZengZichao/Rclade-Studio/security/advisories/new**
（仓库已开启 "Private vulnerability reporting"）

Please use GitHub's **private vulnerability reporting** instead of a public issue.

- 响应目标 / Response target：**72 小时内确认**（acknowledgement within 72 hours）
- 确认后评估影响、发布补丁版本；经你同意后在本文件致谢。

## 范围说明 / Scope

Rclade Studio 是纯本地运行的 macOS 应用，设计上：

- 通过 WKWebView 加载**本机回环地址**上的 Shiny 页面，不对局域网/公网提供服务；
- 不上传用户数据，不打遥测。

以下情况请优先报告：

1. 引擎端口意外绑定到非 `127.0.0.1` 地址；
2. 任意代码执行路径（如 bundle 内脚本被替换后仍被执行）；
3. 对 R 安装路径 / 用户目录的越权读写；
4. Release 附件被替换或签名校验失效。
