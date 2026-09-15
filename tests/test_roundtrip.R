#!/usr/bin/env Rscript
# Rclade Studio M2 往返测试：预览参数 -> 导出代码 -> 干净环境 eval -> 与直接调用一致。
#
# 覆盖方案 §5.2 契约：
#   - diff 基准动态取 formals(plot_timetree)，只导出非默认参数；
#   - tree 落地规则：示例树 data(example_tree) / 粘贴 ape::read.tree(text=) /
#     文件 read_tree_auto("<真实路径>")（含多树 tree_index/multi_tree_mode）；
#   - 非交互化：导出代码不含 output/overwrite；
#   - 干净会话 eval 与直接调用得到语义一致的 plot_timetree 结果。
#
# 用法：Rscript tests/test_roundtrip.R   （失败退出码非 0）

self_dir <- function() {
  fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1]))) else getwd()
}
setwd(self_dir())

failures <- character(0)
# 结果收集器：check() 内的 <- 只改局部，用环境把失败带回全局
.res <- new.env(parent = emptyenv()); .res$fail <- character(0)
check <- function(name, cond, detail = "") {
  if (isTRUE(cond)) {
    cat(sprintf("PASS  %s\n", name))
  } else {
    .res$fail <- c(.res$fail, paste0(name, " ", detail))
    cat(sprintf("FAIL  %s %s\n", name, detail))
  }
}

suppressPackageStartupMessages(library(Rclade))

# 引擎纯函数（含 .collect_params / .build_plot_code / 解析器）注入全局供测试调用
source(file.path("..", "Resources", "engine_app.R"), local = globalenv())

# ---------------------------------------------------------------- 公共工具 ----

# 模拟 UI 的 input 列表（与 server 中 .collect_params(input, ...) 同构）
base_input <- function(overrides = list()) {
  v <- list(
    rank = "phylum", clade = NULL, strict = FALSE,
    triangle_mode = "mixed", space_mode = "proportional",
    layout = "rectangular", angle = 360,
    color_palette = "viridis", color_rank = NULL, line_width = 1,
    show_tip_labels = FALSE, tip_label_size = 2,
    add_timescale = TRUE, timescale_levels = "eons,eras", unit = "auto",
    timescale_mode = "radial", timescale_position = "right",
    tree_start_position = "right", timescale_version = "ICS 2023/02",
    geo_events = FALSE, taxonomy_format = "auto",
    taxonomy_file_header = FALSE, taxonomy_file_priority = TRUE,
    taxonomy_source_priority = "auto", taxonomy_table_sep = ";",
    taxonomy_delimiter_mode = "reverse", legend_position = "bottom",
    legend_nrow = NULL, legend_ncol = NULL, legend_title = NULL,
    show_clade_label = FALSE, show_clade_count = TRUE,
    clade_label_offset = 50, clade_label_fontsize = 3,
    show_support = FALSE, support_threshold = 0.95, show_hpd = FALSE,
    hpd_color = "firebrick", main_title = NULL, sub_title = NULL,
    highlight = NULL, highlight_alpha = 0.2, theme_fun = "theme_timetree",
    width = 14, height = 10, low_memory = FALSE,
    ignore_malformed = FALSE, ignore_branch_length = FALSE
  )
  utils::modifyList(v, overrides, keep.null = TRUE)
}

# 与 server.params_all 同构：UI 字符串 -> 结构化 parsed
parsed_for <- function(v) {
  .parse_ui(color_mapping = v$color_mapping,
            taxonomy_levels = NULL,
            highlight = v$highlight,
            taxonomy_file_sep = "auto")
}

# 干净环境执行导出代码（library(Rclade) 由代码自己完成）
eval_code <- function(code) {
  env <- new.env(parent = globalenv())
  eval(parse(text = code), envir = env)
  env$p
}

# 两个 plot 对象语义一致：类、图层结构、rclade_info 元数据
plots_agree <- function(a, b) {
  ok_class <- identical(class(a), class(b))
  ok_info <- isTRUE(all.equal(attr(a, "rclade_info"), attr(b, "rclade_info"),
                              check.attributes = FALSE))
  ok_layers <- identical(length(a$layers), length(b$layers)) &&
    identical(length(a$labels), length(b$labels))
  ok_class && ok_info && ok_layers
}

# ================================================================= 场景 1 ====
# 示例树 + 默认 UI（rank=phylum, unit 自动补 Ma）→ 导出 data(example_tree) 分支

cat("== 场景 1：示例树 ==\n")
v <- base_input(list(unit = "Ma"))
params <- .collect_params(v, parsed_for(v), NULL)
code <- paste(.build_plot_code(params, "example_tree",
                               preamble = 'data(example_tree, package = "Rclade")'),
              collapse = "\n")
cat(code, "\n\n")

check("S1 导出不含 output/overwrite（非交互化）",
      !grepl("output\\s*=|overwrite\\s*=", code))
check("S1 导出含 data(example_tree) 前导", grepl("data(example_tree", code, fixed = TRUE))

p_eval <- eval_code(code)
e <- new.env(parent = globalenv()); data("example_tree", package = "Rclade", envir = e)
p_direct <- do.call(plot_timetree, c(list(tree = e$example_tree), params))
check("S1 eval 与直接调用语义一致", plots_agree(p_eval, p_direct))

# ================================================================= 场景 2 ====
# 文件源（真实路径）+ 多树参数 + 一批非默认参数 → read_tree_auto 分支
# 注意：multi_tree_mode/tree_index 导出在 read_tree_auto() 调用内（包只在该处消费它们）

cat("== 场景 2：文件源 + 非默认参数 ==\n")
tree_file <- tempfile(fileext = ".nwk")
e2 <- new.env(parent = globalenv()); data("example_tree", package = "Rclade", envir = e2)
ape::write.tree(e2$example_tree, file = tree_file)

v <- base_input(list(
  layout = "circular", color_palette = "Set1", show_clade_label = TRUE,
  main_title = "Studio roundtrip", add_timescale = FALSE,
  highlight = "LUCA, LACA"
))
params <- .collect_params(v, parsed_for(v), NULL)
code <- paste(.build_plot_code(
  params,
  paste0('read_tree_auto("', tree_file, '", multi_tree_mode = "first")')
), collapse = "\n")
cat(code, "\n\n")

check("S2 多树模式导出在 read_tree_auto 内",
      grepl('read_tree_auto\\("[^"]+", multi_tree_mode = "first"\\)', code))
check("S2 导出不含 highlight 占位遗漏（highlight 非默认被导出）",
      grepl("highlight = ", code))
check("S2 不含未修改的默认参数（如 space_mode）",
      !grepl("space_mode\\s*=", code))
check("S2 主题默认不导出", !grepl("theme_fun", code))
check("S2 导出不含 output/overwrite（非交互化）",
      !grepl("output\\s*=|overwrite\\s*=", code))

p_eval <- eval_code(code)
# 直接调用镜像导出语义：先 read_tree_auto（非默认模式）得对象，再以同样参数作图
p_direct <- do.call(plot_timetree,
                    c(list(tree = read_tree_auto(tree_file, multi_tree_mode = "first")),
                      params))
check("S2 eval 与直接调用语义一致", plots_agree(p_eval, p_direct))

# ================================================================= 场景 3 ====
# 粘贴 Newick → ape::read.tree(text=) 内联分支（含引号转义路径验证）

cat("== 场景 3：粘贴 Newick ==\n")
newick <- '(((A:0.1,B:0.2):0.3,"D_\"x\"":0.4):0.5,C:0.6);'
v <- base_input(list(add_timescale = FALSE, rank = "none"))
params <- .collect_params(v, parsed_for(v), NULL)
tree_expr <- paste0('ape::read.tree(text = ', .r_literal(newick), ')')
code <- paste(.build_plot_code(params, tree_expr), collapse = "\n")
cat(code, "\n\n")

check("S3 导出内联 Newick（ape::read.tree）", grepl("ape::read.tree", code, fixed = TRUE))

p_eval <- eval_code(code)
tree_obj <- ape::read.tree(text = newick)
p_direct <- do.call(plot_timetree, c(list(tree = tree_obj), params))
check("S3 eval 与直接调用语义一致", plots_agree(p_eval, p_direct))

# S3b（P0-3）：含换行的粘贴 Newick，导出代码必须仍可被 parse（换行需被转义进字面量）
newick_ml <- '(((A:0.1,B:0.2):0.3,\n"D_\\"x\\"":0.4):0.5,C:0.6);'
code_ml <- paste(.build_plot_code(params,
                                  paste0('ape::read.tree(text = ', .r_literal(newick_ml), ')')),
                collapse = "\n")
check("S3b 多行 Newick 导出代码可被 parse（P0-3）",
      !inherits(tryCatch(parse(text = code_ml), error = function(e) e), "error"))

# ================================================================= 场景 4 ====
# 多树文件 multi_tree_mode="all" → rclade_plot_list；并排/批处理语义一致

cat("== 场景 4：多树 all 模式 ==\n")
multi_file <- tempfile(fileext = ".nwk")
writeLines(c(ape::write.tree(ape::rtree(8)),
             ape::write.tree(ape::rtree(8))),
           multi_file)
v <- base_input(list(add_timescale = FALSE, rank = "none"))
params <- .collect_params(v, parsed_for(v), NULL)
code <- paste(.build_plot_code(
  params,
  paste0('read_tree_auto("', multi_file, '", multi_tree_mode = "all")')
), collapse = "\n")

p_eval <- eval_code(code)
p_direct <- do.call(plot_timetree,
                    c(list(tree = read_tree_auto(multi_file, multi_tree_mode = "all")),
                      params))
check("S4 both 返回 rclade_plot_list 且数量一致",
      inherits(p_eval, "rclade_plot_list") && length(p_eval) == length(p_direct))
check("S4 第 1 棵树语义一致", plots_agree(p_eval[[1]], p_direct[[1]]))

# ================================================================= 场景 5 ====
# 导出基准动态性：.same_val 顺序不敏感；默认参数全部被剔除

cat("== 场景 5：导出基准动态性 ==\n")
check("S5 timescale_levels 顺序等价", .same_val(c("eons", "eras"), c("eras", "eons")))
check("S5 NULL vs NULL 相等", isTRUE(.same_val(NULL, NULL)))
check("S5 符号不同则不相等", !.same_val(quote(bad), quote(bad2)))
# 全默认参数 → 代码只含 tree 行（除 tree 外不应有任何参数行）
v <- base_input(list(rank = "none", unit = "auto"))
params <- .collect_params(v, parsed_for(v), NULL)
code <- paste(.build_plot_code(params, "example_tree"), collapse = "\n")
arg_lines <- grep("^  [a-z_]+ =", readLines(textConnection(code)), value = TRUE)
arg_lines <- arg_lines[!grepl("^  tree =", arg_lines)]
check("S5 全默认参数只导出 tree", length(arg_lines) == 0,
      paste("残留：", paste(arg_lines, collapse = " | ")))
check("S5 全默认参数不含 rank 行", !grepl("rank\\s*=", code))

# ================================================================= 场景 6 ====
# A3：预览/导出共用的 .pick_formals —— 未知参数剔除、已知参数保留
cat("== 场景 6：.pick_formals 过滤 ==\n")
known <- intersect(names(formals(plot_timetree)), names(params))
fake <- c(params, list(totally_unknown_param_xyz = 1))
picked <- .pick_formals(fake)
check("S6 未知参数被剔除", !("totally_unknown_param_xyz" %in% names(picked)))
check("S6 已知参数全部保留",
      all(known %in% names(picked)) && length(picked) == length(known))

# ================================================================= 场景 7 ====
# P2-11：错误分类——版本兼容问题 vs 用户行格式错误
cat("== 场景 7：错误分类 .parse_err_msg ==\n")
ver_cond <- simpleError("Rclade 包缺少共享解析器 parse_plot_params()，请升级 Rclade 后重试。")
class(ver_cond) <- c("rclade_version_error", class(ver_cond))
fmt_cond <- simpleError("每行都需要 \":\" 分隔")
check("S7 版本错误分类为版本不兼容", grepl("版本不兼容", .parse_err_msg(ver_cond)))
check("S7 行格式错误分类为行格式提示", grepl("行格式", .parse_err_msg(fmt_cond)))

# ================================================================= 结果 ====
cat("\n----------------------------------------\n")
if (length(.res$fail)) {
  cat(sprintf("FAILED: %d 项\n", length(.res$fail))
  )
  for (f in .res$fail) cat("  -", f, "\n")
  quit(status = 1, save = "no")
}
cat("ALL PASS\n")
