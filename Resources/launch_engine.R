#!/usr/bin/env Rscript
# Rclade Studio 引擎启动脚本 —— 由 .app 外壳调用。
# 流程：校验依赖 → 起"增强引擎"（studio/Resources/engine_app.R，支持示例树/粘贴/拖入、
#       实时预览、可复现代码导出）→ 把真实 URL 写入 handshake 文件。
# 绘图/折叠逻辑零复制：engine_app.R 内全部经 Rclade::plot_timetree() 等包内导出函数。
# 用法：Rscript launch_engine.R <port> <handshake_file> [dropfile] [lang]
#       dropfile：壳写入被拖入树文件路径的文本；引擎轮询它以自动载入（M3 拖放）。
#       lang：界面语言 zh / en（缺省 zh），由壳按系统语言或用户选择传入。

# ---- 强制 UTF-8 locale ----
# macOS GUI 双击启动 .app 时进程继承的环境变量极少（无 LANG / LC_CTYPE），
# R 退回 "C" locale（codeset=US-ASCII），此时 l10n_info()$`UTF-8` 为 FALSE，
# R 把源码中的 UTF-8 多字节字符逐字节当作单字节 ASCII，
# 经 htmlEscape() 转义后变成 <e6><96><87>… 乱码。
# 这里在脚本最前面（source engine_app.R 之前）把 LC_CTYPE 设为 UTF-8 locale，
# 确保 R 正确识别 UTF-8 字符串。
.local <- function() {
  cur <- Sys.getlocale("LC_CTYPE")
  if (l10n_info()$`UTF-8`) return(invisible(NULL))  # 已经是 UTF-8，无需处理
  candidates <- c(
    Sys.getenv("LC_CTYPE"),       # 用户显式设的（非空时）
    Sys.getenv("LANG"),
    "en_US.UTF-8",                # macOS 上始终可用的 UTF-8 locale
    "C.UTF-8"                     # 部分Linux 发行版的回退
  )
  candidates <- candidates[nzchar(candidates)]
  for (lc in candidates) {
    if (tryCatch({ Sys.setlocale("LC_CTYPE", lc); TRUE }, error = function(e) FALSE)) {
      if (l10n_info()$`UTF-8`) return(invisible(NULL))
    }
  }
  # 全部失败：最后手段——直接设 C.UTF-8（conda/mamba 的 R 通常带此 locale）
  tryCatch(Sys.setlocale("LC_CTYPE", "C.UTF-8"), error = function(e) NULL)
}
.local()
rm(.local)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  cat("usage: Rscript launch_engine.R <port> <handshake_file> [dropfile] [lang]\n", file = stderr())
  quit(status = 2, save = "no")
}
port      <- suppressWarnings(as.integer(args[1]))
handshake <- args[2]
dropfile  <- if (length(args) >= 3 && nzchar(args[3])) args[3] else NULL
lang      <- if (length(args) >= 4 && args[4] %in% c("zh", "en")) args[4] else "zh"

# ---- 依赖校验：缺哪个报哪个，写 stderr 让壳层展示可执行引导 ----
need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("缺少依赖包：需要安装 '%s'（在 R 里执行 install.packages(\"%s\") 或走 Bioconductor）\n", pkg, pkg),
        file = stderr())
    quit(status = 3, save = "no")
  }
}
need("shiny")
need("Rclade")
# Rclade 的 Imports（ggtree/deeptime/... ）在 loadNamespace 时才会真正拉取，缺失即在此暴露
ok <- tryCatch({ loadNamespace("Rclade"); TRUE },
               error = function(e) { cat(conditionMessage(e), file = stderr()); FALSE })
if (!ok) quit(status = 4, save = "no")

# ---- 增强引擎：与本脚本同目录的 engine_app.R（vendored UI 层，见文件头说明）----
app <- tryCatch({
  fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_dir <- if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1]))) else getwd()
  source(file.path(script_dir, "engine_app.R"), local = globalenv())
  make_engine_app(dropfile = dropfile, lang = lang)
}, error = function(e) {
  cat("增强引擎加载失败，回退到包内 run_rclade_shiny()：", conditionMessage(e), "\n",
      file = stderr())
  Rclade::run_rclade_shiny()
})

shiny::runApp(
  app,
  port   = port,
  host   = "127.0.0.1",
  quiet  = TRUE,
  # 回调拿到实际绑定的 URL（防端口自增竞态），写 handshake；返回 FALSE 表示不开系统浏览器。
  launch.browser = function(url) {
    try(writeLines(url, con = handshake), silent = TRUE)
    FALSE
  }
)
