# Copyright (c) 2026 Zeng Zichao
# Licensed under the MIT License (see LICENSE file)

# Rclade Studio 增强引擎（vendored UI 层）
#
# 来源：vendored 自 Rclade 1.1.4 的 R/shiny-app.R（run_rclade_shiny），2026-09-12。
#       壳工程不复制 Rclade 包源码，仅作为 UI/server 层随壳分发；
#       绘图/折叠/解析逻辑零复制 —— 全部调用包内导出函数：
#         plot_timetree() / read_tree_auto() / save_timetree() / summarize_timetree()
#       UI 串结构化复用包内共享解析器（与 CLI 同源）：
#         getFromNamespace("parse_plot_params", "Rclade")
#
# 相对包内 1.1.4 版 UI 的增量（方案 §5–§6，M2/M3）：
#   1) 树源三分：文件（上传/路径/拖入）/ 粘贴 Newick / 示例树，一键载入示例树；
#   2) 实时预览（去按钮化）：参数 reactive + debounce(450ms)，错误内联展示；
#   3) 可复现代码导出框（核心卖点）：diff 基准取 formals(plot_timetree) 动态默认值，
#      只导出非默认参数；tree 落地规则 —— 文件路径 read_tree_auto("<真实路径>")、
#      粘贴 ape::read.tree(text=)、示例 data(example_tree)、上传临时路径给占位符；
#      预览强制 output=NULL（不触发任何保存/交互提示）；
#   4) 参数补全：tree_index / multi_tree_mode（多树）、custom_patterns、groups、
#      taxonomy_file_priority、tree_start_position 全四向；
#   5) 导出增强：PDF/PNG 格式 + 尺寸预设（A4 横/纵、16:9、自定义）+ DPI；
#   6) 主题选择：theme_timetree（包内默认）/ ggplot2 默认。
#
# 升级契约：导出基准动态取自 formals()，包签名变化不会导致导出漂移；
#           UI 未覆盖的新参数由包默认值兜底。
#
# 测试：本文件顶部全部为纯函数（无 Shiny 依赖），tests/test_roundtrip.R 直接 source 复用。

`%||%` <- function(a, b) if (is.null(a)) b else a

.opt_null <- function(x) {
  if (is.null(x) || (is.character(x) && nchar(trimws(x)) == 0)) NULL else trimws(x)
}
.num_or_null <- function(x) {
  if (is.null(x) || is.na(x) || is.nan(x)) NULL else as.numeric(x)
}
.idx_or_null <- function(x) {
  v <- .num_or_null(x)
  if (is.null(v)) NULL else as.integer(v)
}

# 包内共享解析器（与 CLI 同源，零复制）；缺失即给出可读升级提示
.parse_ui <- function(...) {
  fn <- tryCatch(getFromNamespace("parse_plot_params", "Rclade"),
                 error = function(e) NULL)
  if (is.null(fn)) {
    cond <- simpleError(tr("err_version"))
    class(cond) <- c("rclade_version_error", class(cond))
    stop(cond, call. = FALSE)
  }
  fn(...)
}

# 自由文本 -> 命名 list（行格式 `name = value` / `Name: t1, t2`）
.parse_named_lines <- function(txt, sep_style = c("eq", "colon")) {
  sep_style <- match.arg(sep_style)
  if (is.null(txt) || !nchar(trimws(txt))) return(NULL)
  sep <- if (sep_style == "eq") "=" else ":"
  lines <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(NULL)
  has_sep <- grepl(sep, lines, fixed = TRUE)
  if (!all(has_sep)) {
    stop(sprintf(tr("parse_need_sep"), sep,
                 if (sep_style == "eq") "domain = Domain:([^|]+)" else "GroupA: t1, t2"),
         call. = FALSE)
  }
  keys <- trimws(sub(paste0("\\", sep, ".*$"), "", lines))
  vals <- trimws(sub(paste0("^[^", sep, "]+", sep, "\\s*"), "", lines))
  if (sep_style == "colon") {
    vals <- strsplit(gsub(";", ",", vals, fixed = TRUE), ",")
    vals <- lapply(vals, function(v) trimws(v[nzchar(trimws(v))]))
  }
  stats::setNames(as.list(vals), keys)
}

.timescale_levels <- function(sel) {
  v <- trimws(strsplit(sel, ",", fixed = TRUE)[[1]])
  v[nzchar(v)]
}

# 预览值 vs formals 默认值：identical 优先，原子向量再按"排序后相等"兜底
#（如 timescale_levels 的 "eons,eras" 与默认 c("eras","eons") 语义相同，顺序不同）
.same_val <- function(a, b) {
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  identical(a, b) ||
    (is.atomic(a) && is.atomic(b) &&
       isTRUE(all.equal(sort(a), sort(b), check.attributes = FALSE)))
}

# 只保留 plot_timetree 形参中存在的参数：包新增参数自动走默认值（两端一致地省略），
# 包删除/重命名参数则两端一致地忽略，真正兑现 README 宣称的"升级契约"。
# 预览与导出共用此函数，修复 A3（此前预览路径 do.call 未过滤，包签名变化会致预览崩而导出不崩）。
.pick_formals <- function(p) {
  p[intersect(names(formals(Rclade::plot_timetree)), names(p))]
}

# 把预览参数 diff 成"非默认参数"的 R 调用串（可复现契约核心）。
# 默认值基准：formals() 的默认表达式在包命名空间里求值——否则符号型默认
#（如 theme_fun = theme_timetree）与函数值 identical 不成立，会被误判为"非默认"。
.build_plot_code <- function(p, tree_expr, preamble = character(0)) {
  fml <- formals(Rclade::plot_timetree)
  .default_val <- function(nm) {
    tryCatch(eval(fml[[nm]], envir = asNamespace("Rclade")), error = function(e) fml[[nm]])
  }
  # 只保留形参里有的参数，并按签名顺序输出
  p <- p[intersect(names(fml), names(p))]
  keep <- vapply(names(p), function(nm) !.same_val(p[[nm]], .default_val(nm)), logical(1))
  keep <- names(p)[keep]
  arg_lines <- vapply(keep, function(nm) {
    txt <- paste(deparse(p[[nm]], width.cutoff = 500L, backtick = TRUE), collapse = " ")
    paste0("  ", nm, " = ", txt)
  }, character(1))
  lines <- c(paste0("  tree = ", tree_expr), arg_lines)
  if (length(lines) > 1) {
    # 除最后一行外都要带逗号
    lines <- c(paste0(lines[-length(lines)], ","), lines[length(lines)])
  }
  c(paste0("# ", tr("code_banner")),
    "",
    "library(Rclade)",
    if (length(preamble)) preamble else NULL,
    "",
    "p <- plot_timetree(",
    lines,
    ")")
}

# 生成完整合法的 R 字符串字面量（含首尾引号）。deparse 一次性正确处理换行/回车/制表符/
# 引号/反斜杠/控制字符，修复 P0-3（含换行的 Newick 内联进字面量后未闭合导致导出代码语法错误）
# 与 C1（原 .esc_quote 仅转义 \\ 与 \"，不覆盖其余转义规则）。
.r_literal <- function(s) {
  paste(deparse(as.character(s), width.cutoff = 500L), collapse = " ")
}

# 把 params_all/解析阶段抛出的条件对象翻译成中文提示（预览与导出共用，P2-11）：
# 版本过旧（缺共享解析器）与用户行格式错误分开报告，避免把兼容问题误报成"检查行格式"。
.parse_err_msg <- function(cond) {
  if (inherits(cond, "rclade_version_error")) {
    paste0(tr("err_version_prefix"), conditionMessage(cond))
  } else {
    paste0(tr("err_parse_prefix"), conditionMessage(cond))
  }
}

# 原始 UI 值 -> plot_timetree 参数表（预览与导出共用；不含 tree/tree_index/multi_tree_mode，
# 后三者由读取阶段/导出阶段按树源单独处理）。v 为 input 同构的命名列表（可测试注入）。
.collect_params <- function(v, parsed, tax_priority) {
  list(
    rank = v$rank,
    clade = .opt_null(v$clade),
    strict = isTRUE(v$strict),
    groups = parsed$groups,
    triangle_mode = v$triangle_mode,
    space_mode = v$space_mode,
    layout = v$layout,
    angle = v$angle,
    color_palette = v$color_palette,
    color_mapping = parsed[["color_mapping"]],
    color_rank = .opt_null(v$color_rank),
    line_width = v$line_width,
    show_tip_labels = isTRUE(v$show_tip_labels),
    tip_label_size = v$tip_label_size,
    add_timescale = isTRUE(v$add_timescale),
    timescale_levels = .timescale_levels(v$timescale_levels),
    unit = if (identical(v$unit, "auto")) NULL else v$unit,
    timescale_mode = v$timescale_mode,
    timescale_position = v$timescale_position,
    tree_start_position = v$tree_start_position,
    timescale_version = .opt_null(v$timescale_version),
    geo_events = isTRUE(v$geo_events),
    taxonomy_format = v$taxonomy_format,
    custom_patterns = parsed[["custom_patterns"]],
    taxonomy_file = parsed[["taxonomy_file"]],
    taxonomy_file_sep = parsed[["taxonomy_file_sep"]],
    taxonomy_file_header = isTRUE(v$taxonomy_file_header),
    taxonomy_file_priority = isTRUE(v$taxonomy_file_priority),
    taxonomy_source_priority = tax_priority,
    taxonomy_table_sep = v$taxonomy_table_sep,
    taxonomy_delimiter_mode = v$taxonomy_delimiter_mode,
    taxonomy_levels = parsed[["taxonomy_levels"]],
    legend_position = v$legend_position,
    legend_nrow = .num_or_null(v$legend_nrow),
    legend_ncol = .num_or_null(v$legend_ncol),
    legend_title = .opt_null(v$legend_title),
    show_clade_label = isTRUE(v$show_clade_label),
    show_clade_count = isTRUE(v$show_clade_count),
    clade_label_offset = v$clade_label_offset,
    clade_label_fontsize = v$clade_label_fontsize,
    show_support = isTRUE(v$show_support),
    support_threshold = v$support_threshold,
    show_hpd = isTRUE(v$show_hpd),
    hpd_color = v$hpd_color,
    main_title = .opt_null(v$main_title),
    sub_title = .opt_null(v$sub_title),
    highlight = parsed[["highlight"]],
    highlight_alpha = v$highlight_alpha,
    theme_fun = if (identical(v$theme_fun, "none")) NULL else Rclade::theme_timetree,
    width = v$width,
    height = v$height,
    low_memory = isTRUE(v$low_memory),
    ignore_malformed = isTRUE(v$ignore_malformed),
    ignore_branch_length = isTRUE(v$ignore_branch_length)
  )
}

# ==================================================================== 引擎 ====

# ---- 中英文界面语言（默认跟随系统 + 应用内可切换；切换由壳重启引擎生效，界面按启动语言一次性构建）----
.RC_LANG <- "zh"
# ---- 版本标识（须与壳 Info.plist 的 CFBundleShortVersionString 同步；架构启动时自动检测）----
.RC_VERSION <- "0.1.0"
.RC_ARCH <- local({
  a <- tolower(Sys.info()[["machine"]])
  if (a %in% c("arm64", "aarch64")) "Apple Silicon" else if (a == "x86_64") "Intel" else a
})
tr <- function(key) {
  d <- TR[[.RC_LANG]]; v <- if (is.null(d)) NULL else d[[key]]
  if (is.null(v)) key else v
}

TR <- list(
  zh = list(
    app_title = "：系统发育时间树可视化 · 工作室",
    lang_label = "界面语言",
    lang_toggle = "English",
    tree_source = "树来源", src_file = "文件（上传 / 路径 / 拖入）",
    src_paste = "粘贴 Newick", src_example = "示例树",
    btn_example = "一键载入示例树",
    example_label = "选择示例数据",
    ex_example_tree = "示例时间树（example_tree）",
    ex_mammals = "哺乳动物时间树（Ma 标度）",
    ex_primates = "灵长类动物时间树",
    ex_angiosperms = "被子植物时间树",
    ex_cyanobacteria = "蓝细菌门时间树",
    ex_support = "带节点支持值的时间树（演示支持值标注）",
    tree_file = "上传树文件", tree_path = "或树文件路径",
    tree_path_ph = "/path/to/tree.nwk（把文件拖进窗口自动填入）",
    hint_examples = "选择内置示例数据集快速体验绘图功能。",
    tree_index = "树序号（多树文件，如 BEAST 后验）",
    hint_index = "填了「树序号」即取该单棵树，下方「多树模式」将失效（树序号优先）。",
    multi_tree_mode = "多树模式",
    export_tree_path = "导出代码中的树路径（可选）",
    path_ph = "上传的临时文件请在此填真实路径",
    hint_drag = "拖入：把 .nwk/.tre/.nex 拖到窗口任意位置即自动载入并记录真实路径。",
    tree_paste = "Newick 文本",
    hint_paste = "导出代码会内联 Newick 文本（ape::read.tree(text=...)），无需文件；粘贴仅支持单棵 Newick，多树请改用文件源。",
    taxonomy_file = "外部分类学文件",
    export_taxonomy_path = "导出代码中的 taxonomy 路径（可选）",
    tab_tree = "树", tab_layout = "布局", tab_taxonomy = "分类学",
    tab_timescale = "时标", tab_colors = "配色", tab_annotations = "注释", tab_output = "输出",
    rank = "折叠分类阶元",
    hint_rank_mutex = "与「折叠分类阶元」互斥：填写分组后会自动把折叠阶元置为 none。",
    groups = "自定义分组",
    groups_ph = "GroupA: tip1, tip2\nGroupB: tip3; tip4",
    triangle_mode = "三角折叠模式", space_mode = "分支间距模式",
    clade = "指定单系类群", clade_ph = "如 Cyanobacteriota", strict = "严格单系",
    layout = "布局", angle = "扇形角度", line_width = "线宽",
    tree_start_position = "树的起始位置（圆形布局）",
    show_tip_labels = "显示末端标签", tip_label_size = "末端标签字号",
    taxonomy_format = "标签格式",
    custom_patterns = "自定义正则（custom_regex 用）",
    custom_patterns_ph = "domain = Domain:([^|]+)\nphylum = Phylum:([^|]+)",
    taxonomy_delimiter_mode = "解析策略", taxonomy_table_sep = "表格分隔符",
    taxonomy_levels = "自定义层级", taxonomy_levels_ph = "如 k:kingdom,ss:subspecies",
    taxonomy_file_header = "文件含表头", taxonomy_file_priority = "文件优先于标签",
    taxonomy_file_sep = "文件分隔符", taxonomy_source_priority = "数据源优先级",
    hint_source_priority = "「数据源优先级」选 table/embedded 时，上方「文件优先于标签」将被忽略。",
    add_timescale = "添加时标", unit = "时间单位",
    hint_unit = "选 auto 表示沿用树自身单位，此时无法叠加时标（会自动关闭「添加时标」）。",
    timescale_mode = "时标模式", timescale_position = "位置", timescale_levels = "层级",
    timescale_version = "ICS 版本", geo_events = "显示地质事件",
    color_palette = "配色", color_mapping = "手动映射",
    color_mapping_ph = "如 Proteobacteria=#E41A1C,Firmicutes=#377EB8",
    color_rank = "按阶元着色", color_rank_ph = "如 phylum",
    legend_position = "图例位置", legend_nrow = "图例行数", legend_ncol = "图例列数",
    legend_title = "图例标题", legend_title_ph = "auto",
    show_clade_label = "显示类群标签", show_clade_count = "显示物种计数",
    clade_label_offset = "类群标签偏移", clade_label_fontsize = "类群标签字号",
    show_support = "显示节点支持值", support_threshold = "支持值阈值",
    show_hpd = "显示 HPD 条", hpd_color = "HPD 颜色",
    highlight = "高亮类群", highlight_ph = "如 LUCA, LACA", highlight_alpha = "高亮透明度",
    main_title = "主标题", sub_title = "副标题", optional_ph = "可选",
    theme_fun = "主题", theme_rclade = "Rclade 主题", theme_ggplot = "ggplot2 默认",
    export_format = "导出格式", export_size = "导出尺寸预设",
    size_as_is = "跟随宽高", size_a4_land = "A4 横向 (11.7×8.3in)",
    size_a4_port = "A4 纵向 (8.3×11.7in)", size_slide = "幻灯片 16:9 (13.3×7.5in)",
    size_custom = "自定义", export_width = "导出宽 (in)", export_height = "导出高 (in)",
    export_dpi = "导出 DPI（PNG）",
    width = "宽 (in)（仅影响导出）", height = "高 (in)（仅影响导出）",
    ignore_malformed = "跳过畸形输入", ignore_branch_length = "忽略分支长度",
    low_memory = "低内存模式",
    code_header = "可复现代码",
    code_header_hint = "（只含非默认参数；在干净 R 会话可复现当前预览）",
    btn_copy = "复制代码", copy_done = "已复制 ✓",
    copy_fail = "复制失败，请手动选中复制",
    btn_script = "写入脚本 (.R)", btn_image = "导出图像",
    notif_dropped = "已载入拖入的树文件：", notif_example = "已载入示例树（example_tree，单位 Ma）",
    notif_rank_none = "检测到自定义分组 / 指定单系类群，已自动将「折叠分类阶元」置为 none（二者互斥）。",
    notif_unit = "时间单位为 auto（沿用树自身单位）时无法叠加时标，已关闭「添加时标」。",
    hint_multi_fmt = "多树模式：%d 棵树，并排预览前 %d 棵；导出图像保存第 1 棵。",
    err_paste_empty = "请粘贴 Newick 文本",
    err_file_empty = "请上传树文件、拖入文件或填写路径",
    empty_hint = "选择或载入一棵树开始出图",
    err_version = "Rclade 包缺少共享解析器 parse_plot_params()，请升级 Rclade 后重试。",
    err_parse_prefix = "参数解析错误（请检查 Custom Groups / Custom Patterns / Manual Mapping 的行格式）：",
    err_version_prefix = "Rclade 版本不兼容：",
    parse_need_sep = "每行都需要 \"%s\" 分隔，例如：%s",
    code_banner = "由 Rclade Studio 生成：在干净 R 会话中运行可复现当前预览",
    code_tree_notready = "树未就绪：",
    code_override_note = "树路径来自\"导出代码中的树路径\"，上传的临时文件不参与复现。",
    code_upload_placeholder = "<请填入树文件的实际保存路径>",
    code_upload_note = "预览用的是上传临时文件；复现前请把占位符改成真实路径。",
    code_paste_single_note = "粘贴仅支持单棵 Newick；多树文件或指定树序号请改用『文件』树源。",
    code_tax_placeholder = "<请填入 taxonomy 文件的实际保存路径>",
    code_tax_note = "taxonomy 文件为上传临时文件，复现前请把占位符改为真实路径。"
  ),
  en = list(
    app_title = ": Phylogenetic Time Tree Visualization · Studio",
    lang_label = "Interface Language",
    lang_toggle = "中文",
    tree_source = "Tree Source", src_file = "File (upload / path / drag-in)",
    src_paste = "Paste Newick", src_example = "Example tree",
    btn_example = "Load example tree",
    example_label = "Select example data",
    ex_example_tree = "Example timetree (example_tree)",
    ex_mammals = "Mammals timetree (Ma scale)",
    ex_primates = "Primates timetree",
    ex_angiosperms = "被子植物时间树",
    ex_cyanobacteria = "蓝细菌门时间树",
    ex_support = "Timetree with node support values (demo)",
    tree_file = "Upload tree file", tree_path = "Or tree file path",
    tree_path_ph = "/path/to/tree.nwk (drag a file into the window to fill this in)",
    hint_examples = "Select a built-in example dataset to quickly try plotting.",
    tree_index = "Tree index (multi-tree file, e.g. BEAST posterior)",
    hint_index = "Setting Tree index picks that single tree; the Multi-tree mode below is ignored (index takes priority).",
    multi_tree_mode = "Multi-tree mode",
    export_tree_path = "Tree path in exported code (optional)",
    path_ph = "For uploaded temp files, put the real path here",
    hint_drag = "Drag & drop: drop a .nwk/.tre/.nex anywhere in the window to load it and record the real path.",
    tree_paste = "Newick text",
    hint_paste = "Exported code inlines the Newick text (ape::read.tree(text=...)), no file needed. Pasting supports a single Newick only — use the File source for multiple trees.",
    taxonomy_file = "External taxonomy file",
    export_taxonomy_path = "Taxonomy path in exported code (optional)",
    tab_tree = "Tree", tab_layout = "Layout", tab_taxonomy = "Taxonomy",
    tab_timescale = "Timescale", tab_colors = "Colors", tab_annotations = "Annotations", tab_output = "Output",
    rank = "Collapse Rank",
    hint_rank_mutex = "Mutually exclusive with Collapse Rank: filling groups auto-sets the rank to none.",
    groups = "Custom Groups",
    groups_ph = "GroupA: tip1, tip2\nGroupB: tip3; tip4",
    triangle_mode = "Triangle Mode", space_mode = "Space Mode",
    clade = "Single Clade", clade_ph = "e.g. Cyanobacteriota", strict = "Strict monophyly",
    layout = "Layout", angle = "Fan Angle", line_width = "Line Width",
    tree_start_position = "Tree start position (circular)",
    show_tip_labels = "Show tip labels", tip_label_size = "Tip label size",
    taxonomy_format = "Label Format",
    custom_patterns = "Custom Patterns (for custom_regex)",
    custom_patterns_ph = "domain = Domain:([^|]+)\nphylum = Phylum:([^|]+)",
    taxonomy_delimiter_mode = "Parse Strategy", taxonomy_table_sep = "Table Separator",
    taxonomy_levels = "Custom Levels", taxonomy_levels_ph = "e.g. k:kingdom,ss:subspecies",
    taxonomy_file_header = "File has header", taxonomy_file_priority = "File priority over labels",
    taxonomy_file_sep = "File Separator", taxonomy_source_priority = "Source Priority",
    hint_source_priority = "When Source Priority is table/embedded, File priority above is ignored.",
    add_timescale = "Add Timescale", unit = "Time Unit",
    hint_unit = "auto uses the tree's own unit; a timescale cannot be added (Add Timescale is auto-disabled).",
    timescale_mode = "Timescale Mode", timescale_position = "Position", timescale_levels = "Levels",
    timescale_version = "ICS Version", geo_events = "Show geological events",
    color_palette = "Palette", color_mapping = "Manual Mapping",
    color_mapping_ph = "e.g. Proteobacteria=#E41A1C,Firmicutes=#377EB8",
    color_rank = "Color by Rank", color_rank_ph = "e.g. phylum",
    legend_position = "Legend Position", legend_nrow = "Legend Rows", legend_ncol = "Legend Columns",
    legend_title = "Legend Title", legend_title_ph = "auto",
    show_clade_label = "Show Clade Labels", show_clade_count = "Show Species Count",
    clade_label_offset = "Clade Label Offset", clade_label_fontsize = "Clade Label Font Size",
    show_support = "Show Node Support", support_threshold = "Support Threshold",
    show_hpd = "Show HPD Bars", hpd_color = "HPD Color",
    highlight = "Highlight Clades", highlight_ph = "e.g. LUCA, LACA", highlight_alpha = "Highlight Alpha",
    main_title = "Main Title", sub_title = "Subtitle", optional_ph = "optional",
    theme_fun = "Theme", theme_rclade = "Rclade theme", theme_ggplot = "ggplot2 default",
    export_format = "Export Format", export_size = "Export Size Preset",
    size_as_is = "Follow W×H", size_a4_land = "A4 landscape (11.7×8.3in)",
    size_a4_port = "A4 portrait (8.3×11.7in)", size_slide = "Slide 16:9 (13.3×7.5in)",
    size_custom = "Custom", export_width = "Export width (in)", export_height = "Export height (in)",
    export_dpi = "Export DPI (PNG)",
    width = "Width (in) (export only)", height = "Height (in) (export only)",
    ignore_malformed = "Skip malformed inputs", ignore_branch_length = "Ignore branch lengths",
    low_memory = "Low memory mode",
    code_header = "Reproducible code",
    code_header_hint = "(non-default params only; reproduces the current preview in a clean R session)",
    btn_copy = "Copy code", copy_done = "Copied ✓",
    copy_fail = "Copy failed — select and copy manually",
    btn_script = "Save script (.R)", btn_image = "Export image",
    notif_dropped = "Loaded dropped tree file:", notif_example = "Loaded example tree (example_tree, unit Ma)",
    notif_rank_none = "Custom groups / single clade detected — Collapse Rank was set to none automatically (they are mutually exclusive).",
    notif_unit = "A timescale cannot be added when the unit is auto (the tree's own unit) — Add Timescale was turned off.",
    hint_multi_fmt = "Multi-tree mode: %d trees, previewing the first %d; image export saves the 1st.",
    err_paste_empty = "Please paste Newick text",
    err_file_empty = "Upload a tree file, drag a file in, or enter a path",
    empty_hint = "Select or load a tree to start plotting",
    err_version = "The Rclade package lacks parse_plot_params(); please upgrade Rclade and retry.",
    err_parse_prefix = "Parameter parse error (check the line format of Custom Groups / Custom Patterns / Manual Mapping): ",
    err_version_prefix = "Rclade version incompatible: ",
    parse_need_sep = "each line needs a \"%s\" separator, e.g. %s",
    code_banner = "Generated by Rclade Studio: run in a clean R session to reproduce the current preview",
    code_tree_notready = "Tree not ready: ",
    code_override_note = "Tree path comes from 'Tree path in exported code'; uploaded temp files are not reproduced.",
    code_upload_placeholder = "<put the real saved path of the tree file>",
    code_upload_note = "Preview used an uploaded temp file; replace the placeholder with the real path before reproducing.",
    code_paste_single_note = "Pasting supports a single Newick only; use the File source for multi-tree files or a tree index.",
    code_tax_placeholder = "<put the real saved path of the taxonomy file>",
    code_tax_note = "The taxonomy file is an uploaded temp file; replace the placeholder with the real path before reproducing."
  )
)

# ---- 内置示例 Newick 字符串（作为包内 example_tree 的补充，丰富示例数据体验）----
# 这些是简化的时间树 Newick 字符串，末端标签带分类学注释，可直接用于演示。
# mammals：26 物种，Ma 标度（ultrametric，root 180 Ma，分化时间为近似真值量级）；
# support_demo：12 物种，内部节点带 bootstrap 式支持值（0-100），演示"显示节点支持值"。
.EXAMPLE_TREES <- list(
  cyanobacteria = "(Gloeobacter violaceus:2700,(Acaryochloris marina:2200,(Thermosynechococcus elongatus:2000,(((Synechococcus elongatus:900,Prochlorococcus marinus:900):600,(Synechocystis sp:700,Synechocystis sp PCC 6803:700):800,(Microcystis aeruginosa:600,Cyanobacterium sp:600,Cyanobacterium stanieri:600):900):400,((Nostoc punctiforme:300,Anabaena variabilis:300,Nostoc azollae:300):1250,(Chroococcidiopsis sp:1200,(Pleurocapsa sp:900,Xenococcus sp:900):300,(Stanieria sp:850,(Dermocarpella incrassata:800,Myxosarcina sp:800):50):350):350):350):100):200):500);",
  primates = "((((((Homo sapiens:0.006,(Pan troglodytes:0.006,Pan paniscus:0.006):0.004):0.023,Gorilla gorilla:0.029):0.025,Pongo pygmaeus:0.054):0.12,((Hylobates lar:0.18,Nomascus leucogenys:0.18):0.06,((Macaca mulatta:0.09,(Papio anubis:0.07,Macaca fascicularis:0.07):0.02):0.04,(Colobus guereza:0.13,(Semnopithecus entellus:0.12,Trachypithecus vetulus:0.12):0.01):0.02):0.03):0.05):0.5,((Lemur catta:0.65,(Eulemur fulvus:0.6,(Eulemur coronatus:0.58,(Varecia variegata:0.55,Hapalemur griseus:0.55):0.03):0.02):0.05):0.02,(Propithecus coquereli:0.62,(Indri indri:0.6,Avahi laniger:0.6):0.02):0.05):0.03):0.1,((Tarsius syrichta:0.75,(Tarsius bancanus:0.7,Tarsius spectrum:0.7):0.05):0.1,((Saimiri sciureus:0.4,(Aotus trivirgatus:0.38,(Callicebus moloch:0.35,(Pithecia pithecia:0.33,Chiropotes satanas:0.33):0.02):0.03):0.02):0.1,(Cebus capucinus:0.4,(Saimiri boliviensis:0.38,Aotus nancymaae:0.38):0.02):0.05):0.05):0.15);",
  angiosperms = "(((Annona cherimola:120,(Magnolia grandiflora:100,Liriodendron tulipifera:100):20):10,(Persea americana:115,(Cinnamomum camphora:90,(Laurus nobilis:60,Sassafras albidum:60):30):25):15):10,((Elaeis guineensis:110,Cocos nucifera:110):20,(((Oryza sativa:20,Oryza rufipogon:20,Oryza glaberrima:20,Oryza barthii:20):40,(Brachypodium distachyon:40,(Triticum aestivum:15,Hordeum vulgare:15,Aegilops tauschii:15):25):20,Phyllostachys edulis:60):30,((Zea mays:15,Sorghum bicolor:15,Setaria italica:15,Panicum hallii:15):30,(Saccharum officinarum:20,Miscanthus sinensis:20):25):45):40):10,((Vitis vinifera:15,Vitis riparia:15,Vitis labrusca:15,Vitis rotundifolia:15):110,(((((Populus trichocarpa:15,Populus deltoides:15):80,(Salix purpurea:15,Salix babylonica:15):80):5,(((Glycine max:45,(Phaseolus vulgaris:40,(Vigna radiata:38,Cajanus cajan:38):2):5):5,(Medicago truncatula:45,(Lotus japonicus:40,(Cicer arietinum:38,Trifolium pratense:38):2):5):5):40,Rubus idaeus:90):10,((Fagus sylvatica:80,((Quercus robur:50,Quercus suber:50):10,Castanea mollissima:60):20):5,((Juglans regia:65,Carya illinoinensis:65):10,(Betula pendula:60,Alnus glutinosa:60):15):10):15):10,(Arabidopsis thaliana:30,Arabidopsis lyrata:30,Brassica oleracea:30,Capsella rubella:30):80):5,(((Solanum lycopersicum:35,(Solanum tuberosum:10,Solanum melongena:10):25):60,(Capsicum annuum:30,(Nicotiana tabacum:25,Nicotiana benthamiana:25):5,Petunia hybrida:30):65,(Coffea canephora:15,Coffea arabica:15):80):10,(Helianthus annuus:90):15):10):10):15);",
  mammals = "(Ornithorhynchus anatinus:180,((Monodelphis domestica:70,Macropus eugenii:70):70,(Loxodonta africana:105,(((Lemur catta:63,(Tarsius syrichta:60,((Saimiri boliviensis:15,Aotus trivirgatus:15):28,((Macaca mulatta:5,Papio anubis:5):20,(Pongo pygmaeus:14,(Gorilla gorilla:9,(Homo sapiens:6.5,Pan troglodytes:6.5):2.5):5):11):18):17):3):27,(Oryctolagus cuniculus:88,(Mus musculus:12,Rattus norvegicus:12):76):2):5,(Myotis lucifugus:85,((Felis catus:55,(Canis lupus:45,(Ailuropoda melanoleuca:25,Ursus maritimus:25):20):10):15,(Sus scrofa:55,(Tursiops truncatus:54,(Bos taurus:25,Ovis aries:25):29):1):15):15):10):10):35):40);",
  support_demo = "(((Drosophila melanogaster:250,Anopheles gambiae:250)100:539)100:148,(Petromyzon marinus:892,(((Xenopus tropicalis:352,((Gallus gallus:88,Alligator mississippiensis:88)95:222,((Homo sapiens:6.5,Pan troglodytes:6.5)99:89.5,(Mus musculus:12,Rattus norvegicus:12)100:84)96:214)99:42)76:68,(Danio rerio:60,Oryzias latipes:60)88:360)87:130)100:342)95:45);"
)

make_engine_app <- function(dropfile = NULL, lang = "zh") {
  .RC_LANG <<- if (identical(lang, "en")) "en" else "zh"
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Shiny interface requires the shiny package: install.packages('shiny')",
         call. = FALSE)
  }

  # ---------------------------------------------------------------- UI ----

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      :root {
        --accent: #3B82F6; --accent-strong: #2563EB; --accent-active: #1D4ED8;
        --hdr-h: 56px; --plot-h: 560px; --multi-h: 480px;
      }
      * { font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
      html, body { height: 100%; margin: 0; padding: 0; overflow: hidden; }
      body { background: #F8F9FA; color: #2C2C2C; }

      /* ===== 整体布局：固定标题栏 + 固定绘图区 + 仅左侧面板滚动 ===== */
      .app-header {
        position: fixed; top: 0; left: 0; right: 0; z-index: 1000;
        background: #FFFFFF; border-bottom: 1px solid #E8ECEF;
        padding: 0 24px; margin: 0; height: var(--hdr-h);
        display: flex; align-items: center; justify-content: space-between;
      }
      .app-header h2 { font-weight: 400; font-size: 16px; color: #1A1A1A; margin: 0; letter-spacing: -0.2px; line-height: var(--hdr-h); }
      .app-header h2 b { font-weight: 600; }
      .header-right { display: flex; align-items: center; gap: 12px; }

      /* 语言切换按钮 */
      .lang-btn {
        background: #F0F4FF; border: 1px solid #C7DBFF; border-radius: 6px;
        color: var(--accent-strong); font-size: 13px; font-weight: 600;
        padding: 5px 14px; cursor: pointer; transition: all 0.15s; white-space: nowrap;
        line-height: 1.4; }
      .lang-btn:hover { background: #DCE7FF; border-color: var(--accent); }
      .lang-btn:focus { outline: none; box-shadow: 0 0 0 2px rgba(59,130,246,0.2); }

      /* 主内容区：顶部留出标题栏高度，占满视口 */
      .app-body {
        position: fixed; top: var(--hdr-h); left: 0; right: 0; bottom: 0;
        display: flex; overflow: hidden;
      }

      /* 侧栏容器 —— 仅此处允许纵向滚动，设置最小宽度防文字截断 */
      .sidebar-scroll {
        flex: 0 0 420px; min-width: 400px; max-width: 520px;
        overflow-y: auto; overflow-x: hidden;
        padding: 20px 16px 20px 24px;
        background: transparent;
        -webkit-overflow-scrolling: touch;
      }
      .sidebar-scroll::-webkit-scrollbar { width: 6px; }
      .sidebar-scroll::-webkit-scrollbar-track { background: transparent; }
      .sidebar-scroll::-webkit-scrollbar-thumb { background: #D0D0D4; border-radius: 3px; }

      /* 主面板 —— 固定不滚动 */
      .main-fixed {
        flex: 1; overflow: hidden; padding: 16px 20px 16px 12px;
        display: flex; flex-direction: column;
      }

      /* 侧栏内的 well 面板去掉最小高度，让内容自然流动 */
      .well {
        background: #FFFFFF; border: 1px solid #E8ECEF; border-radius: 8px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04); padding: 16px;
        min-height: auto;
      }

      /* Tab 标签：确保不重叠、不换行、有足够间距 */
      .nav-tabs { border-bottom: 1px solid #E8ECEF; margin-bottom: 12px; display: flex; flex-wrap: nowrap; gap: 0; overflow-x: auto; overflow-y: hidden; }
      .nav-tabs::-webkit-scrollbar { height: 4px; }
      .nav-tabs::-webkit-scrollbar-track { background: transparent; }
      .nav-tabs::-webkit-scrollbar-thumb { background: #D0D0D4; border-radius: 2px; }
      .nav-tabs > li { flex-shrink: 0; }
      .nav-tabs > li > a { font-size: 12px; font-weight: 500; color: #6B7280;
        padding: 8px 14px; border: none; border-radius: 4px 4px 0 0;
        letter-spacing: 0.3px; white-space: nowrap; display: inline-block; min-width: fit-content; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        color: #1A1A1A; background: transparent; border: none; border-bottom: 2px solid var(--accent); }
      .nav-tabs > li > a:hover { color: var(--accent); background: transparent; border: none; }

      /* 表单控件：防止文字重叠，设置合理最小宽度 */
      .form-control, .selectize-input {
        border: 1px solid #8A8A8E; border-radius: 6px; font-size: 13px; color: #2C2C2C;
        box-shadow: none; transition: border-color 0.15s; min-width: 150px; width: 100%; max-width: 100%; }
      .selectize-control { width: 100%; min-width: 150px; }
      .selectize-dropdown { font-size: 13px; min-width: 180px; }
      .form-control:focus, .selectize-input.focus {
        border-color: var(--accent); box-shadow: 0 0 0 2px rgba(59,130,246,0.1); }

      /* 标签：允许自然换行（中英文都不与相邻文字重叠），保持足够间距 */
      .control-label { font-size: 12.5px; font-weight: 500; color: #6B7280;
        margin-bottom: 4px; letter-spacing: 0.15px; white-space: normal;
        overflow-wrap: break-word; word-break: normal; line-height: 1.35;
        display: block; }
      .shiny-input-container { margin-bottom: 12px; width: 100%; }
      .input-group { display: flex; width: 100%; flex-wrap: nowrap; }
      .input-group .form-control { border-radius: 6px 0 0 6px; min-width: 100px; flex: 1; }
      .input-group-btn .btn { border-radius: 0 6px 6px 0; border: 1px solid #8A8A8E;
        background: #F5F5F7; color: #6B7280; font-size: 12.5px; font-weight: 500; white-space: nowrap; padding: 8px 14px; }
      .irs--shiny .irs-bar { background: var(--accent); border-color: var(--accent); }
      .irs--shiny .irs-handle { background: #FFFFFF; border: 2px solid var(--accent);
        box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background: var(--accent); font-size: 11px; }
      .btn-primary { background: var(--accent); border: none; border-radius: 6px;
        font-weight: 500; font-size: 13px; letter-spacing: 0.2px; color: #FFFFFF;
        padding: 8px 16px; transition: all 0.15s; white-space: nowrap; }
      .btn-primary:hover { background: var(--accent-strong); box-shadow: 0 2px 6px rgba(59,130,246,0.3); }
      .btn-primary:focus, .btn-primary.focus {
        background: var(--accent-strong); color: #FFFFFF; outline: none;
        box-shadow: 0 0 0 2px rgba(59,130,246,0.35); }
      .btn-primary:active, .btn-primary.active {
        background: var(--accent-active); color: #FFFFFF; box-shadow: none; }
      .btn-default { background: #F5F5F7; border: 1px solid #E0E0E0; border-radius: 6px;
        font-weight: 500; font-size: 12px; color: #5B6472; padding: 7px 16px; white-space: nowrap; }
      .btn-default:hover { background: #EBEBED; color: #374151; }
      .btn-default:focus, .btn-default.focus {
        background: #EBEBED; border-color: var(--accent); color: #374151; outline: none;
        box-shadow: 0 0 0 2px rgba(59,130,246,0.25); }
      .btn-default:active, .btn-default.active {
        background: #E4E4E7; border-color: #D4D4D8; color: #374151; box-shadow: none; }

      /* 复选框 / 单选框标签不换行 */
      .checkbox label { font-size: 12px; color: #4B5563; white-space: nowrap; }
      .radio-inline { margin-right: 14px; }
      .radio-inline label { font-weight: 500; white-space: nowrap; }
      hr { border-color: #E8ECEF; margin: 12px 0; }

      /* 主面板包装 */
      .main-panel-wrap {
        background: #FFFFFF; border: 1px solid #E8ECEF; border-radius: 8px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04); padding: 16px;
        flex: 1; display: flex; flex-direction: column; overflow: hidden; }
      #tree_plot, [id^='multi_plot_'] { border-radius: 4px; overflow: hidden; }
      #summary, #export_code {
        background: #F8F9FA; border: 1px solid #E8ECEF; border-radius: 6px;
        padding: 12px 16px; font-family: 'SF Mono', 'Fira Code', 'Menlo', monospace;
        font-size: 12px; color: #374151; margin-top: 8px; margin-bottom: 8px; }
      #summary { max-height: 120px; overflow-y: auto; flex-shrink: 0; }
      #export_code { max-height: 200px; white-space: pre; overflow-y: auto; overflow-x: auto;
        word-break: normal; overflow-wrap: normal; flex-shrink: 0; }
      .code-bar { display: flex; gap: 8px; align-items: center; margin-top: 8px; flex-shrink: 0; }
      .shiny-notification { border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); font-size: 13px; }
      .shiny-output-error { color: #B91C1C; font-size: 13px; padding: 12px;
        background: #FEF2F2; border-radius: 6px; border: 1px solid #FECACA; }
      .hint { font-size: 11.5px; color: #6B7280; margin-top: 2px; line-height: 1.4; }
      .loading-note { padding: 10px 0; color: #8E8E93; font-size: 14px; }

      /* 版本徽标（标题栏右侧，随包版本更新） */
      .ver-badge {
        font-size: 11px; font-weight: 600; color: #8A919E;
        background: #F0F1F3; border: 1px solid #E4E6EA;
        border-radius: 10px; padding: 2px 9px; white-space: nowrap;
      }

      /* 主区垂直分配：绘图槽位弹性占满剩余高度（不产生页面滚动），
         summary / 可复现代码框固定高度，任何窗口尺寸下完整可见 */
      #plot_slot { flex: 1 1 auto; min-height: 180px; display: flex; flex-direction: column; }
      #plot_slot > #tree_plot { flex: 1 1 auto; }
      /* 多树并排：内容必然超高，在槽位内部滚动（绘图卡片本身不动，主区不滚） */
      #multi_slot { flex: 1 1 auto; min-height: 0; overflow-y: auto; overflow-x: hidden;
        -webkit-overflow-scrolling: touch; }
      #multi_slot::-webkit-scrollbar { width: 6px; }
      #multi_slot::-webkit-scrollbar-thumb { background: #D0D0D4; border-radius: 3px; }

      /* 隐藏原本由 sidebarLayout/sidebarPanel 注入的默认样式干扰 */
      .sidebar-scroll > .well { padding: 0; border: none; background: transparent; box-shadow: none; }
    "))),

    # ---- 固定标题栏 + 版本徽标 + 语言切换按钮 ----
    shiny::div(class = "app-header",
      shiny::h2(shiny::tags$b("Rclade"), tr("app_title")),
      shiny::div(class = "header-right",
        shiny::tags$span(class = "ver-badge",
          shiny::HTML(paste0("v", .RC_VERSION, "&nbsp;&middot;&nbsp;", .RC_ARCH))),
        shiny::tags$button(id = "lang_toggle_btn", class = "lang-btn",
          onclick = paste0(
            "var cur='", .RC_LANG, "';",
            "var nv=cur==='zh'?'en':'zh';",
            "try{if(window.webkit&&window.webkit.messageHandlers&&window.webkit.messageHandlers.rcladeLang){",
            "window.webkit.messageHandlers.rcladeLang.postMessage(nv);",
            "}else{alert('Switch language from the macOS app menu.');}}catch(e){}"
          ),
          tr("lang_toggle"))
      )
    ),

    # ---- 主内容区：左侧滚动 + 右侧固定 ----
    shiny::div(class = "app-body",
      # -- 左侧面板（可滚动）--
      shiny::div(class = "sidebar-scroll",
        shiny::div(class = "well",
          shiny::radioButtons("tree_source", tr("tree_source"),
            choiceNames = c(tr("src_file"), tr("src_paste"), tr("src_example")),
            choiceValues = c("file", "paste", "example"),
            selected = "file", inline = TRUE),
          shiny::actionButton("load_example", tr("btn_example"), class = "btn-default"),
          shiny::conditionalPanel(
            condition = "input.tree_source == 'file'",
            shiny::fileInput("tree_file", tr("tree_file"),
              accept = c(".tre", ".nwk", ".newick", ".nexus", ".nex", ".nhx", ".treefile")),
            shiny::textInput("tree_path", tr("tree_path"),
              placeholder = tr("tree_path_ph")),
            shiny::numericInput("tree_index", tr("tree_index"),
              value = NULL, min = 1, step = 1),
            shiny::helpText(shiny::tags$span(class = "hint", tr("hint_index"))),
            shiny::selectInput("multi_tree_mode", tr("multi_tree_mode"),
              choices = c("error", "first", "last", "random", "all"), selected = "error"),
            shiny::textInput("export_tree_path", tr("export_tree_path"),
              placeholder = tr("path_ph")),
            shiny::helpText(shiny::tags$span(class = "hint", tr("hint_drag"))),
          ),
          shiny::conditionalPanel(
            condition = "input.tree_source == 'paste'",
            shiny::textAreaInput("tree_paste", tr("tree_paste"), rows = 6,
              placeholder = "((A:1,B:2):3,C:4);"),
            shiny::helpText(shiny::tags$span(class = "hint", tr("hint_paste")))
          ),
          shiny::conditionalPanel(
            condition = "input.tree_source == 'example'",
            shiny::selectInput("example_dataset", tr("example_label"),
              # 注意：本地化后的选项名是函数调用结果，不能用 c(name = value) 语法，
              # 必须用 setNames()（否则 R 解析期报 "unexpected '='"）。
              choices = stats::setNames(
                c("example_tree", "mammals", "primates", "angiosperms",
                  "cyanobacteria", "support_demo"),
                c(tr("ex_example_tree"), tr("ex_mammals"), tr("ex_primates"),
                  tr("ex_angiosperms"), tr("ex_cyanobacteria"), tr("ex_support"))),
              selected = "example_tree"),
            shiny::helpText(shiny::tags$span(class = "hint", tr("hint_examples")))
          ),
          shiny::fileInput("taxonomy_file", tr("taxonomy_file"),
            accept = c(".tsv", ".csv", ".txt")),
          shiny::conditionalPanel(
            condition = "input.taxonomy_file != null",
            shiny::textInput("export_taxonomy_path", tr("export_taxonomy_path"),
              placeholder = tr("path_ph"))
          ),
          shiny::hr(),
          shiny::tabsetPanel(id = "tabs",
          shiny::tabPanel(tr("tab_tree"),
            shiny::br(),
            shiny::selectInput("rank", tr("rank"),
              choices = c("none","domain","phylum","class","order","family","genus","species","subspecies"),
              selected = "phylum"),
            shiny::textAreaInput("groups", tr("groups"), rows = 3, placeholder = tr("groups_ph")),
            shiny::helpText(shiny::tags$span(class = "hint", tr("hint_rank_mutex"))),
            shiny::selectInput("triangle_mode", tr("triangle_mode"),
              choices = c("mixed","max","min","none"), selected = "mixed"),
            shiny::selectInput("space_mode", tr("space_mode"),
              choices = c("proportional","equal"), selected = "proportional"),
            shiny::textInput("clade", tr("clade"), placeholder = tr("clade_ph")),
            shiny::checkboxInput("strict", tr("strict"), FALSE)
          ),
          shiny::tabPanel(tr("tab_layout"),
            shiny::br(),
            shiny::selectInput("layout", tr("layout"),
              choices = c("rectangular","circular"), selected = "rectangular"),
            shiny::sliderInput("angle", tr("angle"), 10, 360, 360, step = 10),
            shiny::numericInput("line_width", tr("line_width"), 1, min = 0.1, max = 5, step = 0.1),
            shiny::selectInput("tree_start_position", tr("tree_start_position"),
              choices = c("right","left","top","bottom"), selected = "right"),
            shiny::hr(),
            shiny::checkboxInput("show_tip_labels", tr("show_tip_labels"), FALSE),
            shiny::numericInput("tip_label_size", tr("tip_label_size"), 2, min = 0.5, max = 10, step = 0.5)
          ),
          shiny::tabPanel(tr("tab_taxonomy"),
            shiny::br(),
            shiny::selectInput("taxonomy_format", tr("taxonomy_format"),
              choices = c("auto","GTDB","Silva","NCBI","custom_rank","custom_regex"),
              selected = "auto"),
            shiny::textAreaInput("custom_patterns", tr("custom_patterns"), rows = 3,
              placeholder = tr("custom_patterns_ph")),
            shiny::selectInput("taxonomy_delimiter_mode", tr("taxonomy_delimiter_mode"),
              choices = c("reverse","greedy","segment"), selected = "reverse"),
            shiny::textInput("taxonomy_table_sep", tr("taxonomy_table_sep"), ";"),
            shiny::textInput("taxonomy_levels", tr("taxonomy_levels"),
              placeholder = tr("taxonomy_levels_ph")),
            shiny::hr(),
            shiny::checkboxInput("taxonomy_file_header", tr("taxonomy_file_header"), FALSE),
            shiny::checkboxInput("taxonomy_file_priority", tr("taxonomy_file_priority"), TRUE),
            shiny::selectInput("taxonomy_file_sep", tr("taxonomy_file_sep"),
              choices = c("auto","tab","comma"), selected = "auto"),
            shiny::selectInput("taxonomy_source_priority", tr("taxonomy_source_priority"),
              choices = c("auto","table","embedded"), selected = "auto"),
            shiny::helpText(shiny::tags$span(class = "hint", tr("hint_source_priority")))
          ),
          shiny::tabPanel(tr("tab_timescale"),
            shiny::br(),
            shiny::checkboxInput("add_timescale", tr("add_timescale"), TRUE),
            shiny::selectInput("unit", tr("unit"),
              choices = c("auto","Ga","Ma"), selected = "Ma"),
            shiny::helpText(shiny::tags$span(class = "hint", tr("hint_unit"))),
            shiny::selectInput("timescale_mode", tr("timescale_mode"),
              choices = c("radial","linear"), selected = "radial"),
            shiny::selectInput("timescale_position", tr("timescale_position"),
              choices = c("right","left","top","bottom"), selected = "right"),
            shiny::selectInput("timescale_levels", tr("timescale_levels"),
              choices = c("eons,eras","eons,eras,periods","eras","eons","periods"),
              selected = "eons,eras"),
            shiny::textInput("timescale_version", tr("timescale_version"), "ICS 2023/02"),
            shiny::checkboxInput("geo_events", tr("geo_events"), FALSE)
          ),
          shiny::tabPanel(tr("tab_colors"),
            shiny::br(),
            shiny::selectInput("color_palette", tr("color_palette"),
              choices = c("viridis","Set1","Set2","Set3","Paired","Dark2","Accent","rainbow"),
              selected = "viridis"),
            shiny::textInput("color_mapping", tr("color_mapping"),
              placeholder = tr("color_mapping_ph")),
            shiny::textInput("color_rank", tr("color_rank"), placeholder = tr("color_rank_ph")),
            shiny::hr(),
            shiny::selectInput("legend_position", tr("legend_position"),
              choices = c("bottom","right","left","top","none"), selected = "bottom"),
            shiny::numericInput("legend_nrow", tr("legend_nrow"), value = NULL),
            shiny::numericInput("legend_ncol", tr("legend_ncol"), value = NULL),
            shiny::textInput("legend_title", tr("legend_title"), placeholder = tr("legend_title_ph"))
          ),
          shiny::tabPanel(tr("tab_annotations"),
            shiny::br(),
            shiny::checkboxInput("show_clade_label", tr("show_clade_label"), FALSE),
            shiny::checkboxInput("show_clade_count", tr("show_clade_count"), TRUE),
            shiny::numericInput("clade_label_offset", tr("clade_label_offset"), 50, min = 0, max = 5000, step = 10),
            shiny::numericInput("clade_label_fontsize", tr("clade_label_fontsize"), 3, min = 1, max = 20, step = 0.5),
            shiny::hr(),
            shiny::checkboxInput("show_support", tr("show_support"), FALSE),
            shiny::numericInput("support_threshold", tr("support_threshold"), 0.95, min = 0, max = 1, step = 0.01),
            shiny::checkboxInput("show_hpd", tr("show_hpd"), FALSE),
            shiny::textInput("hpd_color", tr("hpd_color"), "firebrick"),
            shiny::hr(),
            shiny::textInput("highlight", tr("highlight"), placeholder = tr("highlight_ph")),
            shiny::numericInput("highlight_alpha", tr("highlight_alpha"), 0.2, min = 0, max = 1, step = 0.05)
          ),
          shiny::tabPanel(tr("tab_output"),
            shiny::br(),
            shiny::textInput("main_title", tr("main_title"), placeholder = tr("optional_ph")),
            shiny::textInput("sub_title", tr("sub_title"), placeholder = tr("optional_ph")),
            shiny::selectInput("theme_fun", tr("theme_fun"),
              choices = stats::setNames(c("theme_timetree", "none"),
                                        c(tr("theme_rclade"), tr("theme_ggplot"))),
              selected = "theme_timetree"),
            shiny::hr(),
            shiny::selectInput("export_format", tr("export_format"),
              choices = c("PDF" = "pdf", "PNG" = "png"), selected = "pdf"),
            shiny::selectInput("export_size", tr("export_size"),
              choices = stats::setNames(
                c("as_is", "a4_land", "a4_port", "slide", "custom"),
                c(tr("size_as_is"), tr("size_a4_land"), tr("size_a4_port"),
                  tr("size_slide"), tr("size_custom"))),
              selected = "as_is"),
            shiny::conditionalPanel(
              condition = "input.export_size == 'custom'",
              shiny::numericInput("export_width", tr("export_width"), 14, min = 2, max = 60, step = 0.5),
              shiny::numericInput("export_height", tr("export_height"), 10, min = 2, max = 60, step = 0.5)
            ),
            shiny::selectInput("export_dpi", tr("export_dpi"),
              choices = c("300", "600"), selected = "300"),
            shiny::hr(),
            shiny::numericInput("width", tr("width"), 14, min = 4, max = 40, step = 1),
            shiny::numericInput("height", tr("height"), 10, min = 3, max = 30, step = 1),
            shiny::hr(),
            shiny::checkboxInput("ignore_malformed", tr("ignore_malformed"), FALSE),
            shiny::checkboxInput("ignore_branch_length", tr("ignore_branch_length"), FALSE),
            shiny::checkboxInput("low_memory", tr("low_memory"), FALSE)
          )
        )
        )  # /well
      ),  # /sidebar-scroll
      # ---------------------------------------------------------- 主区（固定不滚动）----
      shiny::div(class = "main-fixed",
        shiny::div(class = "main-panel-wrap",
          shiny::uiOutput("plot_visibility"),
          shiny::uiOutput("status_note"),
          shiny::div(id = "plot_slot",
            shiny::plotOutput("tree_plot", height = "100%")),
          shiny::uiOutput("multi_slot"),
          shiny::verbatimTextOutput("summary"),
          shiny::hr(),
          shiny::h4(tr("code_header"),
            shiny::tags$span(class = "hint", tr("code_header_hint"))),
          shiny::verbatimTextOutput("export_code"),
          shiny::div(class = "code-bar",
            shiny::tags$button(id = "copy_code", class = "btn btn-default",
              onclick = paste0(
                "var t=document.getElementById('export_code').textContent;",
                "var flash=function(ok){var b=document.getElementById('copy_code');",
                sprintf("b.textContent=ok?'%s':'%s';", tr("copy_done"), tr("copy_fail")),
                sprintf("setTimeout(function(){b.textContent='%s'},1500);};", tr("btn_copy")),
                "var fallback=function(){var ta=document.createElement('textarea');",
                "ta.value=t;ta.style.position='fixed';ta.style.opacity='0';",
                "document.body.appendChild(ta);ta.focus();ta.select();",
                "var ok=false;try{ok=document.execCommand('copy')}catch(e){};",
                "document.body.removeChild(ta);flash(ok);};",
                "var p=(navigator.clipboard&&navigator.clipboard.writeText)?navigator.clipboard.writeText(t):null;",
                "if(p&&p.then){p.then(function(){flash(true)}).catch(fallback)}else{fallback()};"),
              tr("btn_copy")),
            shiny::downloadButton("dl_script", tr("btn_script"), class = "btn-default"),
            shiny::downloadButton("dl_plot", tr("btn_image"), class = "btn-primary")
          )
        )
      )  # /main-fixed
    )  # /app-body
  )

  # -------------------------------------------------------------- server ----

  server <- function(input, output, session) {

    # ---- 拖入文件：壳把路径写进 dropfile，这里轮询并自动切换到文件源 ----
    if (!is.null(dropfile)) {
      dropped <- shiny::reactivePoll(500, session,
        checkFunc = function() {
          if (!file.exists(dropfile)) "" else as.character(file.info(dropfile)$mtime)
        },
        valueFunc = function() {
          if (!file.exists(dropfile)) return(NULL)
          lns <- trimws(readLines(dropfile, warn = FALSE))
          lns <- lns[nzchar(lns)]
          if (!length(lns)) NULL else lns[length(lns)]
        })
      shiny::observeEvent(dropped(), {
        p <- dropped()
        if (is.null(p)) return()
        shiny::updateTextInput(session, "tree_path", value = p)
        shiny::updateRadioButtons(session, "tree_source", selected = "file")
        shiny::showNotification(paste(tr("notif_dropped"), p), type = "message", duration = 5)
      })
    }

    # ---- 一键载入示例树：切源 + 若时间单位未定则给 Ma（example_tree 为 Ma 单位）----
    shiny::observeEvent(input$load_example, {
      shiny::updateRadioButtons(session, "tree_source", selected = "example")
      if (identical(input$unit, "auto")) {
        shiny::updateSelectInput(session, "unit", selected = "Ma")
      }
      shiny::showNotification(tr("notif_example"), duration = 4)
    })

    # ---- 示例数据集切换时也自动确保 unit=Ma ----
    shiny::observeEvent(input$example_dataset, {
      if (identical(input$tree_source, "example") && identical(input$unit, "auto")) {
        shiny::updateSelectInput(session, "unit", selected = "Ma")
      }
    })

    # ---- A2：rank 与 groups/clade 互斥（包是硬 abort），UI 主动联动置 none 并提示 ----
    shiny::observe({
      g  <- trimws(input$groups %||% "")
      c_ <- trimws(input$clade  %||% "")
      if ((nzchar(g) || nzchar(c_)) && !identical(input$rank, "none")) {
        shiny::updateSelectInput(session, "rank", selected = "none")
        shiny::showNotification(tr("notif_rank_none"), type = "warning", duration = 5)
      }
    })

    # ---- B5：unit=auto 时无法叠加时标（包硬 abort），UI 主动关闭时标并提示 ----
    shiny::observe({
      if (identical(input$unit, "auto") && isTRUE(input$add_timescale)) {
        shiny::updateCheckboxInput(session, "add_timescale", value = FALSE)
        shiny::showNotification(tr("notif_unit"), type = "warning", duration = 5)
      }
    })

    # ---- 树对象：按源读取（文件/粘贴走包内 read_tree_auto，享受格式检测与校验）----
    tree_object <- shiny::reactive({
      idx <- .idx_or_null(input$tree_index)
      mode <- input$multi_tree_mode
      src <- input$tree_source
      ex_sel <- input$example_dataset %||% "example_tree"
      tryCatch({
        if (identical(src, "example")) {
          # 判断选择了哪个示例数据集
          if (ex_sel %in% names(.EXAMPLE_TREES)) {
            # 内置 Newick 示例 —— 通过 read_tree_auto 解析
            tmp <- tempfile(fileext = ".nwk")
            writeLines(.EXAMPLE_TREES[[ex_sel]], tmp)
            on.exit(unlink(tmp), add = TRUE)
            Rclade::read_tree_auto(tmp)
          } else {
            # 默认：包内 example_tree
            e <- new.env(parent = globalenv())
            data("example_tree", package = "Rclade", envir = e)
            e$example_tree
          }
        } else if (identical(src, "paste")) {
          txt <- trimws(input$tree_paste %||% "")
          if (!nchar(txt)) {
            return(structure(list(msg = tr("err_paste_empty")),
                             class = c("tree_error", "tree_empty")))
          }
          tmp <- tempfile(fileext = ".nwk")
          writeLines(txt, tmp)
          on.exit(unlink(tmp), add = TRUE)
          # 粘贴语义 = 单棵 Newick（与导出一致，见 code_text 粘贴分支），
          # 不消费 tree_index/multi_tree_mode：多树请用文件源。
          Rclade::read_tree_auto(tmp)
        } else {
          path <- .opt_null(input$tree_path)
          if (is.null(path)) {
            up <- input$tree_file
            if (is.null(up) || is.null(up$datapath)) {
              return(structure(list(msg = tr("err_file_empty")),
                               class = c("tree_error", "tree_empty")))
            }
            path <- up$datapath
          }
          Rclade::read_tree_auto(path, tree_index = idx, multi_tree_mode = mode)
        }
      }, error = function(e) structure(list(msg = conditionMessage(e)), class = "tree_error"))
    })

    # ---- 参数收集：与预览/导出共用 .collect_params ----
    params_all <- shiny::reactive({
      parsed <- .parse_ui(
        color_mapping     = input$color_mapping,
        taxonomy_levels   = input$taxonomy_levels,
        highlight         = input$highlight,
        taxonomy_file_sep = input$taxonomy_file_sep
      )
      parsed$groups          <- .parse_named_lines(input$groups, "colon")
      parsed$custom_patterns <- .parse_named_lines(input$custom_patterns, "eq")
      tax_file <- input$taxonomy_file
      parsed$taxonomy_file <- if (!is.null(tax_file) && !is.null(tax_file$datapath)) {
        tax_file$datapath
      } else NULL
      tax_priority <- if (identical(input$taxonomy_source_priority, "auto")) {
        NULL
      } else input$taxonomy_source_priority
      .collect_params(input, parsed, tax_priority)
    })

    # ---- 实时预览：树对象 + 参数合并后 debounce，杜绝连击重绘 ----
    params_d <- shiny::debounce(shiny::reactive({
      list(tree = tryCatch(tree_object(), error = function(e)
        structure(list(msg = conditionMessage(e)), class = "tree_error")),
        params = params_all())
    }), 450)

    plot_res <- shiny::reactive({
      # 捕获条件对象本身（而非仅消息），以便区分版本兼容问题与行格式错误（P2-11）
      d <- tryCatch(params_d(), error = function(e) e)
      if (inherits(d, "condition")) {
        return(list(ok = FALSE, empty = FALSE, err = .parse_err_msg(d)))
      }
      t <- d$tree
      if (inherits(t, "tree_error")) {
        return(list(ok = FALSE, empty = inherits(t, "tree_empty"), err = t$msg))
      }
      # 预览参数经 .pick_formals 过滤，与导出一致（修复 A3）：
      # 包删除/重命名参数时两端一致忽略、新增参数两端一致走默认，兑现"升级契约"。
      tryCatch(
        list(ok = TRUE, empty = FALSE,
             p = do.call(Rclade::plot_timetree,
                         c(list(tree = t), .pick_formals(d$params)))),
        error = function(e) list(ok = FALSE, empty = FALSE, err = conditionMessage(e)))
    })

    # ---- 可复现代码：与预览同一参数源（.collect_params 的产物）----
    code_text <- shiny::reactive({
      d <- tryCatch(params_d(), error = function(e) e)
      if (inherits(d, "condition")) {
        return(paste0("# ", .parse_err_msg(d)))
      }
      t <- d$tree
      if (inherits(t, "tree_error")) {
        return(paste0("# ", tr("code_tree_notready"), d$tree$msg))
      }
      p <- d$params
      src <- input$tree_source
      preamble <- character(0)

      if (identical(src, "example")) {
        ex_sel <- input$example_dataset %||% "example_tree"
        if (ex_sel %in% names(.EXAMPLE_TREES)) {
          # 内置 Newick 示例：导出为 ape::read.tree(text=...) 形式
          tree_expr <- paste0('ape::read.tree(text = ', .r_literal(.EXAMPLE_TREES[[ex_sel]]), ')')
        } else {
          # 默认：包内 example_tree
          tree_expr <- "example_tree"
          preamble <- 'data(example_tree, package = "Rclade")'
        }
      } else if (identical(src, "paste")) {
        txt <- trimws(input$tree_paste %||% "")
        idx <- .idx_or_null(input$tree_index)
        # 粘贴语义统一定义为「单棵 Newick」：预览与导出都按单树处理，
        # 使「导出的代码在干净会话可复现预览」这一核心承诺成立（修复 B3：
        # 此前导出盲目追加 [[idx]] → 单树越界报错；NEXUS/多树预览能出但导出跑不通）。
        tree_expr <- paste0('ape::read.tree(text = ', .r_literal(txt), ')')
        if (!is.null(idx) || !identical(input$multi_tree_mode %||% "error", "error")) {
          preamble <- c(preamble, paste0("# ", tr("code_paste_single_note")))
        }
      } else {
        # 文件源：路径优先真实可留存路径（手填/拖入 > 导出覆盖 > 上传占位）。
        # tree_index/multi_tree_mode 必须放进 read_tree_auto() 调用——包只在 tree 为
        # 路径时消费它们，放在 plot_timetree() 参数里会被静默忽略。
        manual <- .opt_null(input$tree_path)
        override <- .opt_null(input$export_tree_path)
        if (!is.null(manual)) {
          path_expr <- manual
        } else if (!is.null(override)) {
          path_expr <- override
          preamble <- paste0("# ", tr("code_override_note"))
        } else {
          path_expr <- tr("code_upload_placeholder")
          preamble <- paste0("# ", tr("code_upload_note"))
        }
        ra_args <- character(0)
        idx <- .idx_or_null(input$tree_index)
        if (!is.null(idx)) ra_args <- c(ra_args, sprintf("tree_index = %d", idx))
        if (!identical(input$multi_tree_mode, "error")) {
          ra_args <- c(ra_args, sprintf('multi_tree_mode = "%s"', input$multi_tree_mode))
        }
        tree_expr <- paste0('read_tree_auto(', .r_literal(path_expr),
                            if (length(ra_args)) {
                              paste0(", ", paste(ra_args, collapse = ", "))
                            } else "",
                            ")")
      }

      # taxonomy 文件路径：预览必为上传临时路径，导出时优先用户指定的真实路径，否则占位符
      if (!is.null(p$taxonomy_file)) {
        tax_override <- .opt_null(input$export_taxonomy_path)
        if (!is.null(tax_override)) {
          p$taxonomy_file <- tax_override
        } else {
          p$taxonomy_file <- tr("code_tax_placeholder")
          preamble <- c(preamble, paste0("# ", tr("code_tax_note")))
        }
      }

      paste(.build_plot_code(p, tree_expr, preamble = preamble), collapse = "\n")
    })

    # ---- 主区动态片段（绘图/代码框/按钮已在 UI 静态常驻，见 mainPanel）----

    # 单图/多树切换 + 无可用图时禁用导出：注入极小 <style>，不动绘图 DOM（P0-4）。
    output$plot_visibility <- shiny::renderUI({
      res <- plot_res()
      multi <- isTRUE(res$ok) && inherits(res$p, "rclade_plot_list")
      css <- character(0)
      css <- c(css, if (multi) "#plot_slot{display:none;}" else "#multi_slot{display:none;}")
      if (!res$ok) {
        # C3 / P2-6：无图时导出按钮不再"可点但静默无反应"，改为视觉禁用且不可点。
        css <- c(css, ".code-bar .btn{opacity:.45;pointer-events:none;}")
      }
      shiny::tags$style(shiny::HTML(paste(css, collapse = "\n")))
    })

    # 空态 / 错误提示（P0-1：不再无保护调用 params_d()；错误时保留已画好的图，P0-4）
    output$status_note <- shiny::renderUI({
      res <- plot_res()
      if (res$ok) return(NULL)
      if (isTRUE(res$empty)) {
        return(shiny::div(class = "loading-note", `role` = "status",
                          `aria-live` = "polite", tr("empty_hint")))
      }
      shiny::div(class = "shiny-output-error", `role` = "alert",
                 `aria-live` = "assertive", res$err)
    })

    # 多树并排：仅当结果为 rclade_plot_list 时渲染 N 个 plotOutput 槽位
    output$multi_slot <- shiny::renderUI({
      res <- plot_res()
      shiny::req(res$ok, inherits(res$p, "rclade_plot_list"))
      n <- min(length(res$p), 6)
      shiny::tagList(
        shiny::helpText(shiny::tags$span(class = "hint",
          sprintf(tr("hint_multi_fmt"), length(res$p), n))),
        lapply(seq_len(n), function(i) {
          shiny::plotOutput(paste0("multi_plot_", i), height = "480px")
        })
      )
    })

    # 单图
    output$tree_plot <- shiny::renderPlot({
      res <- plot_res()
      shiny::req(res$ok, !inherits(res$p, "rclade_plot_list"))
      print(res$p)
    })
    # 多树并排（observe 重新注册输出，与 renderUI 的槽位一一对应）
    shiny::observe({
      res <- plot_res()
      shiny::req(res$ok, inherits(res$p, "rclade_plot_list"))
      n <- min(length(res$p), 6)
      for (i in seq_len(n)) {
        local({
          ii <- i
          output[[paste0("multi_plot_", ii)]] <- shiny::renderPlot({
            print(res$p[[ii]])
          })
        })
      }
    })

    output$summary <- shiny::renderPrint({
      res <- plot_res()
      shiny::req(res$ok)
      if (inherits(res$p, "rclade_plot_list")) {
        cat(sprintf("rclade_plot_list：%d 棵树\n", length(res$p)))
        Rclade::summarize_timetree(res$p[[1]])
      } else {
        Rclade::summarize_timetree(res$p)
      }
    })

    output$export_code <- shiny::renderPrint({
      cat(code_text())
    })

    # ---- 导出：写入脚本 / 图像（save_timetree 全权负责保存与格式）----
    output$dl_script <- shiny::downloadHandler(
      filename = function() paste0("rclade_plot_", Sys.Date(), ".R"),
      content  = function(file) writeLines(code_text(), file)
    )

    .export_dims <- function() {
      switch(input$export_size,
        "a4_land" = c(11.69, 8.27),
        "a4_port" = c(8.27, 11.69),
        "slide"   = c(13.33, 7.5),
        "custom"  = c(.num_or_null(input$export_width) %||% 14,
                      .num_or_null(input$export_height) %||% 10),
        c(.num_or_null(input$width) %||% 14, .num_or_null(input$height) %||% 10))
    }

    output$dl_plot <- shiny::downloadHandler(
      filename = function() {
        paste0("rclade_plot_", Sys.Date(), ".", input$export_format)
      },
      content = function(file) {
        res <- plot_res()
        shiny::req(res$ok)
        dims <- .export_dims()
        obj <- if (inherits(res$p, "rclade_plot_list")) res$p[[1]] else res$p
        # B4：不依赖 downloadHandler 临时路径的扩展名（旧版 Shiny 会剥掉，导致
        # save_timetree 设备推断退化为 PDF）。显式落到带扩展名的临时文件再拷贝。
        ext <- input$export_format
        tmp <- tempfile(fileext = paste0(".", ext))
        on.exit(unlink(tmp), add = TRUE)
        Rclade::save_timetree(obj, tmp, width = dims[1], height = dims[2],
                              dpi = as.integer(input$export_dpi), overwrite = "force")
        file.copy(tmp, file, overwrite = TRUE)
      }
    )
  }

  shiny::shinyApp(ui, server)
}
