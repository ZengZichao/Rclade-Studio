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
    # 读了树之后末端标签里的空格会被去掉/换成下划线，这里把用户输入的
    # "Homo sapiens" 也归一成 "Homo_sapiens"，否则分组永远匹配不上。
    vals <- lapply(vals, function(v) gsub(" ", "_", v, fixed = TRUE))
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
.collect_params <- function(v, parsed, tax_priority, unit_resolved = NULL) {
  list(
    rank = v$rank,
    clade = .opt_null(v$clade),
    strict = isTRUE(v$strict),
    groups = parsed$groups,
    groups_label_only = isTRUE(v$groups_label_only),
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
    # unit 是"分支长度单位"（Ga 会被包内换算成 Ma，×1000）。
    # 选 auto 时用调用方按树自身量级识别出的单位，而不是 NULL——NULL 会直接关掉时标。
    unit = if (identical(v$unit, "auto")) unit_resolved else v$unit,
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

# 带 data-i18n 的文案节点：切换语言时由前端就地替换，无需重启引擎
lbl <- function(key) shiny::tags$span(`data-i18n` = key, tr(key))

TR <- list(
  zh = list(
    app_title = "系统发育时间树可视化",
    lang_label = "界面语言",
    lang_toggle = "English",
    plate_preview = "预览", plate_auto = "参数改变后自动重绘",
    empty_title = "把一棵树放进来",
    empty_body = "把 .nwk / .tre / .nex 文件拖到窗口任意位置即可出图；也可以从下面挑一个示例直接开始。",
    chip_upload = "上传文件…", chip_paste = "粘贴 Newick",
    grp_source = "树来源", grp_collapse = "折叠与分组", grp_layout = "布局",
    grp_timescale = "时标", grp_taxonomy = "分类学解析", grp_colors = "配色",
    grp_annot = "注释与标注", grp_output = "输出与主题",
    tree_source = "树来源", src_file = "文件（上传 / 路径 / 拖入）",
    src_file_s = "文件", src_paste_s = "粘贴", src_example_s = "示例",
    file_browse = "选择文件", file_none = "未选择文件",
    src_paste = "粘贴 Newick", src_example = "示例树",
    btn_example = "一键载入示例树",
    example_label = "选择示例数据",
    ex_example_tree = "示例时间树（example_tree）",
    ex_a = "时间树 A · 26 末端 · 根 180 Ma（常规）",
    ex_b = "时间树 B · 32 末端 · 枝长非时间（无时标）",
    ex_c = "时间树 C · 63 末端 · 最密（试折叠）",
    ex_d = "时间树 D · 19 末端 · 跨 27 亿年（深时）",
    ex_e = "时间树 E · 12 末端 · 带支持值",
    tree_file = "上传树文件", tree_path = "或树文件路径",
    tree_path_ph = "/path/to/tree.nwk（把文件拖进窗口自动填入）",
    hint_examples = "内置示例的末端都是匿名名称（Taxon_A…），拓扑与枝长只用于演示界面功能，不对应任何真实类群。",
    tree_index = "树序号",
    hint_index = "多树文件（如 BEAST 后验）填序号即取该单棵树；填了序号后「多树模式」失效，序号优先。",
    multi_tree_mode = "多树模式",
    export_tree_path = "导出代码中的树路径（可选）",
    path_ph = "上传的临时文件请在此填真实路径",
    hint_drag = "拖入：把 .nwk/.tre/.nex 拖到窗口任意位置即自动载入并记录真实路径。",
    tree_paste = "Newick 文本",
    hint_paste = "导出代码会内联 Newick 文本（ape::read.tree(text=...)），无需文件；粘贴仅支持单棵 Newick，多树请改用文件源。",
    taxonomy_file = "外部分类学文件",
    export_taxonomy_path = "导出代码中的 taxonomy 路径（可选）",
    ex_desc_example_tree = "入门示例，结构最简",
    ex_desc_a = "26 末端，根 180 Ma：试时标/标签",
    ex_desc_b = "32 末端，枝长非时间：试布局/折叠",
    ex_desc_c = "63 末端，最密集：试折叠/配色",
    ex_desc_d = "19 末端，跨 27 亿年：看深时分带",
    ex_desc_e = "12 末端，节点带支持值",
    ext_example_tree = "示例时间树", ext_a = "A · 常规",
    ext_b = "B · 无时标", ext_c = "C · 最密",
    ext_d = "D · 深时", ext_e = "E · 带支持值",
    rank = "折叠分类阶元",
    hint_rank_mutex = "填了分组就会自动把「折叠分类阶元」置为「不折叠」——两者只能选一个，避免重复折叠。",
    hint_tip_labels = "打开后每个末端分支右侧显示它自己的名字；字号按上面的数值，越长越会自动给右侧留白。",
    clade_label_tip_hint = "当前没有可标注的折叠类群——末端分支自己的名字请用上面的「显示末端标签」。",
    tip_label_fix = "开启「显示末端标签」",
    groups_label_only = "分组只标注、不折叠",
    hint_label_only = "打开后，分组里的分支保持完整展开，只在类群旁边写上组名，不画三角形。",
    tri_none_body = "「不画三角」会把折叠的类群整体收成一个点：内部的末端分支和标签都不再绘制，画面上看起来像末梢消失了。想看到末端分支，请把「折叠分类阶元」设为「不折叠」，或改用「自定义分组」只折叠部分类群。",
    tri_none_fix = "改为不折叠（显示全部末端分支）",
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
    hint_unit = "单位指「这棵树的枝长本身是什么单位」，Ga 会被换算成 Ma（×1000）再画时标；不确定就保持自动识别。",
    unit_auto = "自动识别（按树自身量级）",
    unit_note_auto = "分支长度按%s解读最合理，根年龄 ≈ %s。",
    unit_note_ok = "按%s解读：根年龄 ≈ %s。",
    unit_note_bad = "这棵树实际是%s标度。按%s解读会把所有枝长换算成 Ma（×1000），根年龄变成 %s，超出地球年龄 4600 Ma，时标会被撑爆。",
    clade_label_none = "类群标签只标注「被折叠的类群」。当前没有任何折叠：请在「02 折叠与分组」里填「自定义分组」（如 GroupA: 物种1, 物种2），它会把这几支收成一个三角并写上组名。",
    clade_label_tax = "标签只出现在被折叠的类群旁。内置示例的末端标签没有分类学注释（只有一个物种名），所以「折叠分类阶元」找不到可折叠的类群，也就没有标签可写。想立刻看到文字标签，请在「02 折叠与分组」填写自定义分组；想给整棵树加名字，用「08 输出与主题 → 主标题」。",
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
    app_title = "Phylogenetic timetree visualization",
    lang_label = "Interface Language",
    lang_toggle = "中文",
    plate_preview = "Preview", plate_auto = "Redraws automatically on every change",
    empty_title = "Drop in a tree",
    empty_body = "Drag a .nwk / .tre / .nex file anywhere into the window, or start from one of the examples below.",
    chip_upload = "Choose a file…", chip_paste = "Paste Newick",
    grp_source = "Tree source", grp_collapse = "Collapsing & groups", grp_layout = "Layout",
    grp_timescale = "Timescale", grp_taxonomy = "Taxonomy parsing", grp_colors = "Colors",
    grp_annot = "Annotations", grp_output = "Output & theme",
    tree_source = "Tree Source", src_file = "File (upload / path / drag-in)",
    src_file_s = "File", src_paste_s = "Paste", src_example_s = "Example",
    file_browse = "Choose", file_none = "No file selected",
    src_paste = "Paste Newick", src_example = "Example tree",
    btn_example = "Load example tree",
    example_label = "Select example data",
    ex_example_tree = "Example timetree (example_tree)",
    ex_a = "Timetree A · 26 tips · root 180 Ma (standard)",
    ex_b = "Timetree B · 32 tips · branch lengths, no time axis",
    ex_c = "Timetree C · 63 tips · densest (try collapsing)",
    ex_d = "Timetree D · 19 tips · spans 2.7 Gyr (deep time)",
    ex_e = "Timetree E · 12 tips · with support values",
    tree_file = "Upload tree file", tree_path = "Or tree file path",
    tree_path_ph = "/path/to/tree.nwk (drag a file into the window to fill this in)",
    hint_examples = "Built-in examples use anonymous tip names (Taxon_A…). Topology and branch lengths only demonstrate the interface — they are not real taxa.",
    tree_index = "Tree index",
    hint_index = "For multi-tree files (e.g. BEAST posterior), set an index to pick that one tree; Multi-tree mode is then ignored.",
    multi_tree_mode = "Multi-tree mode",
    export_tree_path = "Tree path in exported code (optional)",
    path_ph = "For uploaded temp files, put the real path here",
    hint_drag = "Drag & drop: drop a .nwk/.tre/.nex anywhere in the window to load it and record the real path.",
    tree_paste = "Newick text",
    hint_paste = "Exported code inlines the Newick text (ape::read.tree(text=...)), no file needed. Pasting supports a single Newick only — use the File source for multiple trees.",
    taxonomy_file = "External taxonomy file",
    export_taxonomy_path = "Taxonomy path in exported code (optional)",
    ex_desc_example_tree = "Simplest starting point",
    ex_desc_a = "26 tips, root 180 Ma: try timescale/labels",
    ex_desc_b = "32 tips, no time axis: try layout/collapse",
    ex_desc_c = "63 tips, densest: try collapse/colors",
    ex_desc_d = "19 tips, spans 2.7 Gyr: deep-time bands",
    ex_desc_e = "12 tips, nodes carry support values",
    ext_example_tree = "Example tree", ext_a = "A · standard",
    ext_b = "B · no time axis", ext_c = "C · densest",
    ext_d = "D · deep time", ext_e = "E · support",
    rank = "Collapse Rank",
    hint_rank_mutex = "Filling in groups automatically sets Collapse Rank to none — the two are mutually exclusive.",
    hint_tip_labels = "Shows each terminal branch's own name to its right; longer names get more whitespace automatically.",
    clade_label_tip_hint = "Nothing is collapsed, so there is no clade label to draw — use Show tip labels above for the branch names.",
    tip_label_fix = "Turn on Show tip labels",
    groups_label_only = "Label groups without collapsing",
    hint_label_only = "The grouped branches stay fully expanded; only the group name is written next to the clade — no triangle is drawn.",
    tri_none_body = "\"No triangle\" absorbs each collapsed clade into a single point: the terminal branches and labels inside it are no longer drawn, so the tips appear to vanish. To see terminal branches, set Collapse Rank to none, or use Custom Groups to collapse only some clades.",
    tri_none_fix = "Set Collapse Rank to none (show all tips)",
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
    hint_unit = "This is the unit of the tree's own branch lengths. Ga is converted to Ma (x1000) before the timescale is drawn — keep Auto-detect unless you are sure.",
    unit_auto = "Auto-detect (from the tree)",
    unit_note_auto = "Branch lengths read best as %s (root age ≈ %s).",
    unit_note_ok = "Read as %s: root age ≈ %s.",
    unit_note_bad = "This tree is in %s. Reading it as %s multiplies every branch length by 1000 — root age becomes %s, older than Earth (4600 Ma) — and the timescale blows up.",
    clade_label_none = "Clade labels only annotate 'collapsed clades'. Nothing is collapsed yet: fill in Custom Groups (e.g. GroupA: tip1, tip2) — those tips collapse into one triangle carrying the group name.",
    clade_label_tax = "Labels appear only next to collapsed clades. The built-in examples carry no taxonomy in their tip labels (just species names), so Collapse Rank finds nothing to collapse and no label is drawn. Fill in Custom Groups to get labels right away, or use Output & theme -> Main title for the tree's own name.",
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

# ---- 下拉选项显示名：值保持英文机器串（供 R 端消费），只换展示文案 ----
# 缺失的键自动回退为原值，包新增取值也不会导致 UI 崩。
OPT <- list(
  zh = list(
    rank = c(none = "不折叠", domain = "域", phylum = "门", class = "纲",
             order = "目", family = "科", genus = "属", species = "种",
             subspecies = "亚种"),
    triangle = c(mixed = "自适应（按类群大小）", max = "整段范围",
                 min = "最小范围", none = "不画三角（类群整体收起）"),
    space = c(proportional = "按末端数比例", equal = "平均分配"),
    layout = c(rectangular = "矩形", circular = "环形"),
    pos = c(right = "3 点钟方向", top = "12 点钟方向",
            bottom = "6 点钟方向", left = "9 点钟方向"),
    multimode = c(error = "报错提示（推荐）", first = "取第一棵",
                  last = "取最后一棵", random = "随机取一棵", all = "全部并排画"),
    taxformat = c(auto = "自动识别", GTDB = "GTDB", Silva = "Silva", NCBI = "NCBI",
                  custom_rank = "自定义层级", custom_regex = "自定义正则"),
    taxdelim = c(reverse = "从右往左", greedy = "从左往右",
                 segment = "按分隔符逐段提取"),
    taxsep = c(auto = "自动判断", tab = "制表符 Tab", comma = "逗号"),
    taxprio = c(auto = "自动", table = "仅用表格文件", embedded = "仅用标签内嵌"),
    unit = c(auto = "自动识别（按树自身量级）", Ga = "Ga（十亿年）", Ma = "Ma（百万年）"),
    tsmode = c(radial = "环形色带（圆形布局）", linear = "线性时间轴"),
    tslevels = c(`eons,eras` = "宙 + 代", `eons,eras,periods` = "宙 + 代 + 纪",
                 eras = "仅代", eons = "仅宙", periods = "仅纪"),
    palette = c(viridis = "viridis（连续）", Set1 = "Set1", Set2 = "Set2",
                Set3 = "Set3", Paired = "Paired", Dark2 = "Dark2",
                Accent = "Accent", rainbow = "rainbow"),
    legend = c(bottom = "下方", right = "右侧", left = "左侧",
               top = "上方", none = "不显示"),
    theme = c(theme_timetree = "Rclade 主题", none = "ggplot2 默认"),
    expsize = c(as_is = "跟随宽高", a4_land = "A4 横向 (11.7×8.3in)",
                a4_port = "A4 纵向 (8.3×11.7in)", slide = "幻灯片 16:9 (13.3×7.5in)",
                custom = "自定义"),
    example = c(example_tree = "示例时间树（example_tree）",
                tree_a = "时间树 A（26 个末端，Ma 标度）",
                tree_b = "时间树 B（32 个末端）",
                tree_c = "时间树 C（63 个末端）",
                tree_d = "时间树 D（19 个末端，深时间）",
                tree_e = "时间树 E（12 个末端，带支持值）")
  ),
  en = list(
    rank = c(none = "None", domain = "Domain", phylum = "Phylum", class = "Class",
             order = "Order", family = "Family", genus = "Genus",
             species = "Species", subspecies = "Subspecies"),
    triangle = c(mixed = "Adaptive (by clade size)", max = "Full range",
                 min = "Minimal range", none = "No triangle (clade absorbed)"),
    space = c(proportional = "Proportional to tips", equal = "Equal space"),
    layout = c(rectangular = "Rectangular", circular = "Circular"),
    pos = c(right = "3 o'clock", top = "12 o'clock",
            bottom = "6 o'clock", left = "9 o'clock"),
    multimode = c(error = "Error (default)", first = "First tree",
                  last = "Last tree", random = "Random tree",
                  all = "Plot all"),
    taxformat = c(auto = "Auto-detect", GTDB = "GTDB", Silva = "Silva", NCBI = "NCBI",
                  custom_rank = "Custom ranks", custom_regex = "Custom regex"),
    taxdelim = c(reverse = "Right to left", greedy = "Left to right",
                 segment = "Segment by delimiter"),
    taxsep = c(auto = "Auto", tab = "Tab", comma = "Comma"),
    taxprio = c(auto = "Auto", table = "Table only", embedded = "Embedded only"),
    unit = c(auto = "Auto-detect", Ga = "Ga (billion yr)", Ma = "Ma (million yr)"),
    tsmode = c(radial = "Radial bands (circular)", linear = "Linear axis"),
    tslevels = c(`eons,eras` = "Eons + eras", `eons,eras,periods` = "Eons + eras + periods",
                 eras = "Eras only", eons = "Eons only", periods = "Periods only"),
    palette = c(viridis = "viridis (continuous)", Set1 = "Set1", Set2 = "Set2",
                Set3 = "Set3", Paired = "Paired", Dark2 = "Dark2",
                Accent = "Accent", rainbow = "rainbow"),
    legend = c(bottom = "Bottom", right = "Right", left = "Left",
               top = "Top", none = "Hidden"),
    theme = c(theme_timetree = "Rclade theme", none = "ggplot2 default"),
    expsize = c(as_is = "Follow W×H", a4_land = "A4 landscape (11.7×8.3in)",
                a4_port = "A4 portrait (8.3×11.7in)", slide = "Slide 16:9 (13.3×7.5in)",
                custom = "Custom"),
    example = c(example_tree = "Example timetree (example_tree)",
                tree_a = "Timetree A (26 tips, Ma scale)",
                tree_b = "Timetree B (32 tips)",
                tree_c = "Timetree C (63 tips)",
                tree_d = "Timetree D (19 tips, deep time)",
                tree_e = "Timetree E (12 tips, support values)")
  )
)

.fmt_age <- function(ma) {
  if (!is.finite(ma)) return("-")
  v <- if (ma >= 100) round(ma) else signif(ma, 3)
  paste0(format(v, big.mark = ",", scientific = FALSE, trim = TRUE), " Ma")
}

# 当前语言下某个取值的显示名（用于拼接动态提示）
.opt_label <- function(group, value) {
  d <- OPT[[.RC_LANG]][[group]]
  if (!is.null(d) && !is.null(d[[value]])) d[[value]] else value
}

.opt <- function(group, keys) {
  d <- OPT[[.RC_LANG]][[group]]
  labs <- if (is.null(d)) rep(NA_character_, length(keys)) else unname(d[keys])
  miss <- is.na(labs) | !nzchar(labs)
  labs[miss] <- keys[miss]
  stats::setNames(keys, labs)
}

# ---- 前端热切换语言所需的映射表（不重启引擎，由 JS 就地改文案）----
# input id -> placeholder 文案 key
.PH_MAP <- c(
  tree_path = "tree_path_ph", export_tree_path = "path_ph",
  export_taxonomy_path = "path_ph", groups = "groups_ph",
  custom_patterns = "custom_patterns_ph", taxonomy_levels = "taxonomy_levels_ph",
  clade = "clade_ph", color_mapping = "color_mapping_ph",
  color_rank = "color_rank_ph", legend_title = "legend_title_ph",
  highlight = "highlight_ph", main_title = "optional_ph", sub_title = "optional_ph"
)

# select id -> OPT 分组（用于按 option 的 value 重新贴显示名）
.SEL_OPT <- c(
  rank = "rank", triangle_mode = "triangle", space_mode = "space",
  layout = "layout", tree_start_position = "pos", timescale_position = "pos",
  multi_tree_mode = "multimode", taxonomy_format = "taxformat",
  taxonomy_delimiter_mode = "taxdelim", taxonomy_file_sep = "taxsep",
  taxonomy_source_priority = "taxprio", unit = "unit", timescale_mode = "tsmode",
  timescale_levels = "tslevels", color_palette = "palette",
  legend_position = "legend", theme_fun = "theme", export_size = "expsize",
  example_dataset = "example"
)

.js_enc <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub('"', '\\\\"', x)
  x <- gsub("\n", "\\\\n", x)
  x <- gsub("\r", "", x)
  paste0('"', x, '"')
}

# 把两套文案 + 选项名 + 映射表一次性发给前端（体积约 12KB，远小于重启引擎的代价）
.i18n_payload <- function() {
  dict <- function(d) {
    paste0("{", paste(sprintf('"%s":%s', names(d),
      vapply(d, .js_enc, character(1), USE.NAMES = FALSE)), collapse = ","), "}")
  }
  opts <- function(lang) {
    g <- OPT[[lang]]
    paste0("{", paste(vapply(names(g), function(k) {
      sprintf('"%s":%s', k, dict(as.list(g[[k]])))
    }, character(1), USE.NAMES = FALSE), collapse = ","), "}")
  }
  kv <- function(v) paste(sprintf('"%s":%s', names(v),
    vapply(v, .js_enc, character(1), USE.NAMES = FALSE)), collapse = ",")
  paste0(
    "window.RCLADE_STR={zh:", dict(TR$zh), ",en:", dict(TR$en), "};",
    "window.RCLADE_OPT={zh:", opts("zh"), ",en:", opts("en"), "};",
    "window.RCLADE_PH={", kv(.PH_MAP), "};",
    "window.RCLADE_OPTID={", kv(.SEL_OPT), "};",
    "window.RCLADE_I18N={fileBrowse:", .js_enc(tr("file_browse")),
    ",fileNone:", .js_enc(tr("file_none")), "};",
    "window.__RCLADE_LANG=", .js_enc(.RC_LANG), ";"
  )
}

# ---- 示例数据缩略图（内联 SVG 线描：不引入任何外部资源，随界面上色）----
.GLYPH <- list(
  rect = paste0(
    '<svg viewBox="0 0 44 32" class="glyph" aria-hidden="true" fill="none" ',
    'stroke="currentColor" stroke-width="1.25" stroke-linecap="round">',
    '<path d="M7 5v22"/><path d="M7 8h7M7 16h7M7 24h7"/>',
    '<path d="M14 6v4M14 14v4M14 22v4"/>',
    '<path d="M14 6h9M14 10h7M14 14h9M14 18h7M14 22h9M14 26h7"/>',
    '</svg>'),
  circ = paste0(
    '<svg viewBox="0 0 44 32" class="glyph" aria-hidden="true" fill="none" ',
    'stroke="currentColor" stroke-width="1.25" stroke-linecap="round">',
    '<circle cx="22" cy="16" r="10.5"/>',
    '<circle cx="22" cy="16" r="3"/>',
    '<path d="M25 16h7.5M23.5 13.4l5.3-5.3M22 13v-7.5M20.5 13.4l-5.3-5.3M19 16h-7.5M20.5 18.6l-5.3 5.3M23.5 18.6l5.3 5.3M22 19v7.5"/>',
    '</svg>'),
  scale = paste0(
    '<svg viewBox="0 0 44 32" class="glyph" aria-hidden="true" fill="none" ',
    'stroke="currentColor" stroke-width="1.25" stroke-linecap="round">',
    '<rect x="6" y="2" width="8" height="3.4" fill="#7FA056" stroke="none"/>',
    '<rect x="14" y="2" width="13" height="3.4" fill="#34B2C9" stroke="none"/>',
    '<rect x="27" y="2" width="11" height="3.4" fill="#F04028" stroke="none"/>',
    '<path d="M7 10v18"/><path d="M7 13h7M7 21h7"/>',
    '<path d="M14 11v4M14 19v4"/>',
    '<path d="M14 11h9M14 15h7M14 19h9M14 23h7"/>',
    '</svg>')
)

# 示例数据集：key 与 .EXAMPLE_TREES / 包内 example_tree 对齐
# label 用短名（卡片宽度有限），desc 一句话说明"这棵树适合看什么"
.EX_TILES <- list(
  list(key = "example_tree", label = "ext_example_tree", desc = "ex_desc_example_tree",
       glyph = "rect",   title = "Example timetree"),
  list(key = "tree_a", label = "ext_a", desc = "ex_desc_a",
       glyph = "scale",  title = "Timetree A"),
  list(key = "tree_b", label = "ext_b", desc = "ex_desc_b",
       glyph = "rect",   title = "Timetree B"),
  list(key = "tree_c", label = "ext_c", desc = "ex_desc_c",
       glyph = "scale",  title = "Timetree C"),
  list(key = "tree_d", label = "ext_d", desc = "ex_desc_d",
       glyph = "circ",   title = "Timetree D"),
  list(key = "tree_e", label = "ext_e", desc = "ex_desc_e",
       glyph = "rect",   title = "Timetree E")
)

.ex_title <- function(k) {
  hit <- Filter(function(t) identical(t$key, k), .EX_TILES)
  if (length(hit)) hit[[1]]$title else ""
}

.ex_tile <- function(t) {
  shiny::tags$button(
    type = "button", class = "tile", `data-k` = t$key,
    onclick = sprintf("Shiny.setInputValue('pick_example','%s',{priority:'event'});", t$key),
    shiny::HTML(.GLYPH[[t$glyph]]),
    shiny::tags$span(class = "tile-txt",
      shiny::tags$span(class = "tile-name", lbl(t$label)),
      shiny::tags$span(class = "tile-desc", lbl(t$desc)))
  )
}

# 调色板色卡（用于「配色」分组下的实时预览条）
.PALETTE_SWATCHES <- list(
  viridis = c("#440154", "#3B528B", "#21918C", "#5EC962", "#FDE725"),
  Set1    = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
  Set2    = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"),
  Set3    = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3"),
  Paired  = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99"),
  Dark2   = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"),
  Accent  = c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0"),
  rainbow = c("#FF0000", "#FF9900", "#FFFF00", "#00CC00", "#0000FF")
)

# ---- 内置示例 Newick 字符串（作为包内 example_tree 的补充）----
# 全部使用匿名末端标签（Taxon_A、Taxon_B…）。拓扑与枝长只用于演示界面功能，
# 不对应任何真实类群，也不含可引用的分化时间——请勿当作数据使用。
.EXAMPLE_TREES <- list(
  tree_d = "(Taxon_A:2700,(Taxon_B:2200,(Taxon_C:2000,(((Taxon_D:900,Taxon_E:900):600,(Taxon_F:700,Taxon_G:700):800,(Taxon_H:600,Taxon_I:600,Taxon_J:600):900):400,((Taxon_K:300,Taxon_L:300,Taxon_M:300):1250,(Taxon_N:1200,(Taxon_O:900,Taxon_P:900):300,(Taxon_Q:850,(Taxon_R:800,Taxon_S:800):50):350):350):350):100):200):500);",
  tree_b = "((((((Taxon_A:0.006,(Taxon_B:0.006,Taxon_C:0.006):0.004):0.023,Taxon_D:0.029):0.025,Taxon_E:0.054):0.12,((Taxon_F:0.18,Taxon_G:0.18):0.06,((Taxon_H:0.09,(Taxon_I:0.07,Taxon_J:0.07):0.02):0.04,(Taxon_K:0.13,(Taxon_L:0.12,Taxon_M:0.12):0.01):0.02):0.03):0.05):0.5,((Taxon_N:0.65,(Taxon_O:0.6,(Taxon_P:0.58,(Taxon_Q:0.55,Taxon_R:0.55):0.03):0.02):0.05):0.02,(Taxon_S:0.62,(Taxon_T:0.6,Taxon_U:0.6):0.02):0.05):0.03):0.1,((Taxon_V:0.75,(Taxon_W:0.7,Taxon_X:0.7):0.05):0.1,((Taxon_Y:0.4,(Taxon_Z:0.38,(Taxon_AA:0.35,(Taxon_AB:0.33,Taxon_AC:0.33):0.02):0.03):0.02):0.1,(Taxon_AD:0.4,(Taxon_AE:0.38,Taxon_AF:0.38):0.02):0.05):0.05):0.15);",
  tree_c = "(((Taxon_A:120,(Taxon_B:100,Taxon_C:100):20):10,(Taxon_D:115,(Taxon_E:90,(Taxon_F:60,Taxon_G:60):30):25):15):10,((Taxon_H:110,Taxon_I:110):20,(((Taxon_J:20,Taxon_K:20,Taxon_L:20,Taxon_M:20):40,(Taxon_N:40,(Taxon_O:15,Taxon_P:15,Taxon_Q:15):25):20,Taxon_R:60):30,((Taxon_S:15,Taxon_T:15,Taxon_U:15,Taxon_V:15):30,(Taxon_W:20,Taxon_X:20):25):45):40):10,((Taxon_Y:15,Taxon_Z:15,Taxon_AA:15,Taxon_AB:15):110,(((((Taxon_AC:15,Taxon_AD:15):80,(Taxon_AE:15,Taxon_AF:15):80):5,(((Taxon_AG:45,(Taxon_AH:40,(Taxon_AI:38,Taxon_AJ:38):2):5):5,(Taxon_AK:45,(Taxon_AL:40,(Taxon_AM:38,Taxon_AN:38):2):5):5):40,Taxon_AO:90):10,((Taxon_AP:80,((Taxon_AQ:50,Taxon_AR:50):10,Taxon_AS:60):20):5,((Taxon_AT:65,Taxon_AU:65):10,(Taxon_AV:60,Taxon_AW:60):15):10):15):10,(Taxon_AX:30,Taxon_AY:30,Taxon_AZ:30,Taxon_BA:30):80):5,(((Taxon_BB:35,(Taxon_BC:10,Taxon_BD:10):25):60,(Taxon_BE:30,(Taxon_BF:25,Taxon_BG:25):5,Taxon_BH:30):65,(Taxon_BI:15,Taxon_BJ:15):80):10,(Taxon_BK:90):15):10):10):15);",
  tree_a = "(Taxon_A:180,((Taxon_B:70,Taxon_C:70):70,(Taxon_D:105,(((Taxon_E:63,(Taxon_F:60,((Taxon_G:15,Taxon_H:15):28,((Taxon_I:5,Taxon_J:5):20,(Taxon_K:14,(Taxon_L:9,(Taxon_M:6.5,Taxon_N:6.5):2.5):5):11):18):17):3):27,(Taxon_O:88,(Taxon_P:12,Taxon_Q:12):76):2):5,(Taxon_R:85,((Taxon_S:55,(Taxon_T:45,(Taxon_U:25,Taxon_V:25):20):10):15,(Taxon_W:55,(Taxon_X:54,(Taxon_Y:25,Taxon_Z:25):29):1):15):15):10):10):35):40);",
  tree_e = "(((Taxon_A:250,Taxon_B:250)100:539)100:148,(Taxon_C:892,(((Taxon_D:352,((Taxon_E:88,Taxon_F:88)95:222,((Taxon_G:6.5,Taxon_H:6.5)99:89.5,(Taxon_I:12,Taxon_J:12)100:84)96:214)99:42)76:68,(Taxon_K:60,Taxon_L:60)88:360)87:130)100:342)95:45);"
)

make_engine_app <- function(dropfile = NULL, lang = "zh") {
  .RC_LANG <<- if (identical(lang, "en")) "en" else "zh"
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Shiny interface requires the shiny package: install.packages('shiny')",
         call. = FALSE)
  }

  # ---------------------------------------------------------------- UI ----

  # ---- 表单控件工厂：label 传"文案 key"，统一 width、关闭 selectize（原生下拉更快更易读）----
  sel <- function(id, key, choices, selected = NULL) {
    shiny::selectInput(id, lbl(key), choices = choices, selected = selected,
                       selectize = FALSE, width = "100%")
  }
  num <- function(id, key, value, min = NULL, max = NULL, step = NULL) {
    # 只传非 NULL 的边界：numericInput(min = NULL) 会在内部 is.na() 处报错
    args <- list(inputId = id, label = lbl(key), value = value, width = "100%")
    if (!is.null(min))  args$min  <- min
    if (!is.null(max))  args$max  <- max
    if (!is.null(step)) args$step <- step
    do.call(shiny::numericInput, args)
  }
  txt <- function(id, key, value = "", placeholder = NULL) {
    shiny::textInput(id, lbl(key), value = value, placeholder = placeholder, width = "100%")
  }
  area <- function(id, key, rows, placeholder = NULL) {
    shiny::textAreaInput(id, lbl(key), rows = rows, placeholder = placeholder, width = "100%")
  }
  chk <- function(id, key, value = FALSE) shiny::checkboxInput(id, lbl(key), value)
  hint <- function(key) {
    shiny::helpText(shiny::tags$span(class = "hint", `data-i18n` = key, tr(key)))
  }

  # ---- 可折叠分组（原生 <details>，无 JS 依赖、可键盘操作）----
  grp <- function(idx, key, ..., open = FALSE) {
    attrs <- if (open) list(open = "open") else list()
    do.call(shiny::tags$details, c(
      list(class = "grp"), attrs, list(
        shiny::tags$summary(
          shiny::tags$span(class = "grp-idx", idx),
          shiny::tags$span(class = "grp-name", lbl(key))),
        shiny::div(class = "grp-body", ...))))
  }

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$meta(name = "color-scheme", content = "light"),
      shiny::tags$style(shiny::HTML(r"---(
      :root{
        --paper:#FBFAF7; --surface:#FFFFFF;
        --ink:#1A2420; --ink-2:#45504A; --muted:#6B746E;
        --line:#E3E1DA; --line-2:#C9C6BC;
        --accent:#0B6E5F; --accent-deep:#095A4E; --wash:#EAF2EF;
        --warm:#C2410C; --warm-deep:#9A340A;
        --danger:#B3241C; --danger-wash:#FDF3F1;
        --serif:"New York","Songti SC","STSong",Georgia,serif;
        --sans:-apple-system,BlinkMacSystemFont,"SF Pro Text","PingFang SC","Hiragino Sans GB",sans-serif;
        --mono:"SF Mono",ui-monospace,Menlo,Consolas,monospace;
        --hdr-h:52px; --rail-w:400px; --r:3px;
      }
      html,body{height:100%;margin:0;padding:0;overflow:hidden;}
      body{background:var(--paper);color:var(--ink);font-family:var(--sans);
        font-size:13px;-webkit-font-smoothing:antialiased;}
      ::selection{background:var(--wash);color:var(--ink);}

      /* ===== 标题栏 ===== */
      .app-header{
        position:fixed;top:0;left:0;right:0;z-index:1000;height:var(--hdr-h);
        background:var(--paper);border-bottom:1px solid var(--line);
        display:flex;align-items:center;justify-content:space-between;
        padding:0 18px 0 20px;}
      .brand{display:flex;align-items:baseline;gap:10px;min-width:0;}
      .brand-name{font-family:var(--serif);font-size:17px;font-weight:600;
        color:var(--ink);white-space:nowrap;}
      .brand-rule{width:1px;height:13px;background:var(--line-2);flex:0 0 1px;
        align-self:center;}
      .brand-sub{font-size:11.5px;color:var(--muted);white-space:nowrap;
        overflow:hidden;text-overflow:ellipsis;}
      .header-right{display:flex;align-items:center;gap:10px;flex:0 0 auto;}
      .ver-badge{font-family:var(--mono);font-size:10.5px;color:var(--muted);
        background:var(--surface);border:1px solid var(--line);border-radius:2px;
        padding:2px 7px;white-space:nowrap;}
      .lang-btn{background:transparent;border:1px solid var(--line-2);
        border-radius:var(--r);color:var(--ink-2);font-size:11.5px;font-weight:600;
        padding:4px 11px;cursor:pointer;transition:background .12s,border-color .12s,color .12s;}
      .lang-btn:hover{background:var(--surface);border-color:var(--ink-2);color:var(--ink);}
      .lang-btn:focus-visible{outline:2px solid var(--accent);outline-offset:1px;}

      /* ===== 主体：左轨道 + 右图版 ===== */
      .app-body{position:fixed;top:var(--hdr-h);left:0;right:0;bottom:0;
        display:flex;overflow:hidden;}
      .rail{flex:0 0 var(--rail-w);max-width:var(--rail-w);background:var(--surface);
        border-right:1px solid var(--line);overflow-y:auto;overflow-x:hidden;
        -webkit-overflow-scrolling:touch;}
      .rail::-webkit-scrollbar{width:9px;}
      .rail::-webkit-scrollbar-thumb{background:var(--line-2);border-radius:5px;
        border:2px solid var(--surface);}
      .rail::-webkit-scrollbar-track{background:transparent;}

      /* 分组 */
      .grp{border-bottom:1px solid var(--line);}
      .grp>summary{list-style:none;cursor:pointer;display:flex;align-items:center;
        gap:9px;padding:11px 18px;user-select:none;}
      .grp>summary::-webkit-details-marker{display:none;}
      .grp>summary:hover{background:#FAF9F5;}
      .grp>summary:focus-visible{outline:2px solid var(--accent);outline-offset:-2px;}
      .grp-idx{font-family:var(--mono);font-size:10px;color:var(--line-2);
        letter-spacing:.06em;flex:0 0 auto;padding-top:1px;}
      .grp[open]>summary .grp-idx{color:var(--accent);}
      .grp-name{font-family:var(--serif);font-size:13.5px;font-weight:600;
        color:var(--ink);flex:1;min-width:0;}
      .grp>summary::after{content:"";flex:0 0 auto;width:6px;height:6px;
        border-right:1.4px solid var(--muted);border-bottom:1.4px solid var(--muted);
        transform:rotate(45deg) translateY(-2px);transition:transform .16s;}
      .grp[open]>summary::after{transform:rotate(225deg) translateY(-1px);}
      .grp-body{padding:2px 18px 16px;}

      /* ===== 表单 ===== */
      .grp-body .form-group{margin-bottom:13px;}
      .control-label{display:block;font-size:11px;font-weight:600;color:var(--ink-2);
        letter-spacing:.04em;line-height:1.45;margin-bottom:5px;white-space:normal;
        overflow-wrap:break-word;}
      .form-control{border:1px solid var(--line-2);border-radius:var(--r);
        background:var(--surface);color:var(--ink);font-size:12.5px;
        height:32px;padding:5px 9px;width:100%;max-width:100%;min-width:0;
        box-shadow:none;transition:border-color .12s,box-shadow .12s;}
      select.form-control{height:32px;padding-right:6px;}
      textarea.form-control{height:auto;min-height:70px;font-family:var(--mono);
        font-size:11.5px;line-height:1.6;resize:vertical;}
      .form-control:focus{border-color:var(--accent);outline:none;
        box-shadow:0 0 0 3px rgba(11,110,95,.16);}
      .form-control::placeholder{color:#7D877F;opacity:1;}
      .form-control[disabled]{background:#F6F4EF;color:var(--muted);}
      .grp-body .input-group{display:flex;width:100%;}
      /* Bootstrap 给 .input-group-btn 设了 width:1%（表格布局遗产），
         在 flex 下会把浏览按钮压成 3px，导致整组缩到 300px */
      .grp-body .input-group>.input-group-btn{width:auto;flex:0 0 auto;
        display:block;font-size:0;}
      .grp-body .input-group>.form-control{flex:1 1 0;width:auto;min-width:0;
        border-radius:0 var(--r) var(--r) 0;}
      .has-error .form-control{border-color:var(--danger);}

      .hint{display:block;font-size:11px;color:var(--muted);line-height:1.55;
        margin-top:4px;}
      .help-block{margin-top:0;margin-bottom:12px;}
      .grp-body>.help-block:last-child{margin-bottom:0;}
      hr{border:0;border-top:1px solid var(--line);margin:14px 0;}
      /* 文件输入的进度条：空闲时几乎不可见，上传时是一条细线 */
      .shiny-input-container .progress{height:3px;background:transparent;
        box-shadow:none;margin:6px 0 0;border-radius:0;}
      .shiny-input-container .progress .bar,
      .shiny-input-container .progress-bar{background:var(--accent);
        box-shadow:none;}

      .grid2{display:grid;grid-template-columns:1fr 1fr;gap:0 10px;}
      .grid2>.form-group,.grid2>.shiny-input-container{min-width:0;}

      .checkbox{margin-top:0;margin-bottom:11px;}
      .checkbox label{font-size:12px;color:var(--ink-2);line-height:1.5;
        padding-left:20px;}
      .checkbox input[type=checkbox]{margin-left:-20px;margin-top:3px;
        accent-color:var(--accent);}

      /* 分段控件（树来源）：Shiny 的 label.radio-inline 就是按钮本身 */
      .seg .shiny-options-group{display:flex;border:1px solid var(--line-2);
        border-radius:var(--r);overflow:hidden;}
      .seg .radio-inline{flex:1 1 0;margin:0;padding:7px 4px;position:relative;
        min-width:0;text-align:center;font-size:11.5px;font-weight:600;
        color:var(--ink-2);cursor:pointer;border-left:1px solid var(--line-2);
        background:var(--surface);white-space:nowrap;overflow:hidden;
        text-overflow:ellipsis;transition:background .12s,color .12s;}
      .seg .radio-inline:first-child{border-left:none;}
      .seg .radio-inline:hover{background:#FAF9F5;}
      .seg .radio-inline input[type=radio]{position:absolute;opacity:0;
        width:0;height:0;pointer-events:none;}
      .seg .radio-inline.is-on{background:var(--accent);color:#fff;}
      .seg .radio-inline.is-on:hover{background:var(--accent-deep);}
      .seg .radio-inline:focus-within{outline:2px solid var(--accent);
        outline-offset:-2px;}

      /* 文件输入：原生控件文案本地化（见页尾脚本），按钮与输入框必须同高 */
      .grp-body .input-group{height:32px;}
      .btn-file{background:var(--surface)!important;border:1px solid var(--line-2)!important;
        border-radius:var(--r) 0 0 var(--r)!important;color:var(--ink-2)!important;
        font-size:11.5px!important;font-weight:600!important;height:32px!important;
        display:inline-flex!important;align-items:center!important;
        padding:0 12px!important;line-height:1!important;box-shadow:none!important;
        margin:0!important;}
      .btn-file:hover{background:#F5F3EE!important;border-color:var(--ink-2)!important;}
      .grp-body .input-group input[type=text][readonly]{border-left:none;background:#F7F5F1;
        color:var(--muted);height:32px;}
      /* 空输出收成 0 高（不能用 display:none：Shiny 不会向不可见元素渲染输出，会死锁） */
      #summary.is-empty,#export_code.is-empty{height:0;padding:0;margin:0;border:0;
        overflow:hidden;min-height:0;}
      .input-group input[type=text][readonly]::placeholder{color:#9BA39D;}

      /* 父开关关闭时的子控件：视觉与交互一并锁住（不改 disabled，避免影响取值） */
      .form-group.is-off .control-label{color:#9BA39D;}
      .form-group.is-off .form-control,
      .form-group.is-off .shiny-options-group,
      .form-group.is-off .input-group{opacity:.42;pointer-events:none;}

      /* ===== 按钮 ===== */
      .btn{border-radius:var(--r);font-size:12px;font-weight:600;letter-spacing:.02em;
        padding:7px 14px;border:1px solid transparent;line-height:1.35;
        transition:background .12s,border-color .12s,color .12s;}
      .btn:focus-visible{outline:2px solid var(--accent);outline-offset:1px;
        box-shadow:none;}
      .btn-default{background:var(--surface);border-color:var(--line-2);
        color:var(--ink-2);}
      .btn-default:hover,.btn-default:focus,.btn-default:active{
        background:#F5F3EE;border-color:var(--ink-2);color:var(--ink);
        box-shadow:none;}
      .btn.btn-warm,.btn.btn-primary{background:var(--warm);border-color:var(--warm);
        color:#fff;}
      .btn.btn-warm:hover,.btn.btn-warm:focus,.btn.btn-primary:hover,
      .btn.btn-primary:focus{background:var(--warm-deep);border-color:var(--warm-deep);
        color:#fff;box-shadow:none;}
      .plate-actions.is-off .btn{opacity:.4;pointer-events:none;}

      /* 滑块 */
      .irs--shiny .irs-bar{background:var(--accent);border-color:var(--accent);}
      .irs--shiny .irs-handle{background:var(--surface);border:2px solid var(--accent);
        box-shadow:none;}
      .irs--shiny .irs-single,.irs--shiny .irs-from,.irs--shiny .irs-to{
        background:var(--accent);font-size:10.5px;padding:1px 5px;}
      .irs--shiny .irs-line{background:var(--line);border:none;}
      .irs--shiny .irs-grid-text{font-size:9.5px;color:var(--muted);}
      .irs--shiny{margin-top:26px;}

      /* ===== 右图版 ===== */
      .main{flex:1;min-width:0;display:flex;flex-direction:column;padding:14px;
        gap:12px;overflow:hidden;}
      .plate{flex:1;min-height:0;display:flex;flex-direction:column;
        background:var(--surface);border:1px solid var(--line);border-radius:var(--r);}
      .plate-bar{display:flex;align-items:center;justify-content:space-between;
        gap:12px;padding:9px 14px;border-bottom:1px solid var(--line);}
      .plate-title{font-family:var(--serif);font-size:13.5px;font-weight:600;
        color:var(--ink);}
      .plate-sub{font-size:11px;color:var(--muted);margin-left:9px;}
      .plate-actions{display:flex;align-items:center;gap:8px;}
      .plate-canvas{flex:1;min-height:300px;position:relative;
        padding:10px;display:flex;flex-direction:column;}
      #plot_slot{flex:1;min-height:0;display:flex;flex-direction:column;}
      #tree_plot{flex:1;min-height:0;}
      #multi_slot{flex:1;min-height:0;overflow-y:auto;overflow-x:hidden;}
      #multi_slot::-webkit-scrollbar{width:8px;}
      #multi_slot::-webkit-scrollbar-thumb{background:var(--line-2);border-radius:4px;}
      .multi-note{font-size:11px;color:var(--muted);margin-bottom:6px;}
      .multi-out{border:1px solid var(--line);border-radius:var(--r);
        margin-bottom:10px;padding:6px;background:var(--surface);}
      .recalculating{opacity:.35;transition:opacity .18s;}

      /* 空态 */
      .empty{position:absolute;inset:10px;border:1px dashed var(--line-2);
        border-radius:var(--r);background:var(--paper);
        display:flex;flex-direction:column;align-items:center;justify-content:center;
        text-align:center;padding:22px 24px;}
      .empty-mark{width:52px;height:52px;color:var(--line-2);margin-bottom:14px;}
      .empty-title{font-family:var(--serif);font-size:19px;font-weight:600;
        color:var(--ink);margin:0 0 7px;}
      .empty-body{font-size:12.5px;color:var(--muted);line-height:1.7;
        max-width:46ch;margin:0 0 20px;}
      .tiles{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));
        gap:8px;width:100%;max-width:620px;}
      .tile{display:flex;align-items:center;gap:9px;text-align:left;
        padding:9px 10px;background:var(--surface);border:1px solid var(--line);
        border-radius:var(--r);cursor:pointer;
        transition:border-color .12s,background .12s,transform .12s;}
      .tile:hover{border-color:var(--accent);background:var(--wash);}
      .tile:active{transform:translateY(1px);}
      .tile:focus-visible{outline:2px solid var(--accent);outline-offset:1px;}
      .glyph{width:32px;height:23px;flex:0 0 32px;color:var(--muted);}
      .tile:hover .glyph{color:var(--accent);}
      .tile-txt{display:flex;flex-direction:column;min-width:0;}
      .tile-name{font-size:11.5px;font-weight:600;color:var(--ink);line-height:1.3;
        overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}
      /* 示例卡的说明要能读完：允许两行，超出才省略 */
      .tile-desc{font-size:10.5px;color:var(--muted);line-height:1.4;
        display:-webkit-box;-webkit-line-clamp:2;-webkit-box-orient:vertical;
        overflow:hidden;white-space:normal;}
      .empty-actions{margin-top:16px;}

      /* 配色色卡 */
      .swatch{display:flex;height:20px;border-radius:var(--r);overflow:hidden;
        border:1px solid var(--line);margin:-4px 0 12px;}
      .swatch-dot{flex:1 1 0;}

      /* 错误条 */
      .note{font-size:11px;line-height:1.6;margin:-2px 0 12px;padding:7px 9px;
        border-radius:var(--r);background:var(--wash);color:var(--ink-2);
        border:1px solid #D6E3DE;}
      .note-warn{background:#FDF6EC;border-color:#EBD9BE;color:#8A5A16;}
      .note .btn{width:100%;white-space:normal;text-align:center;font-size:11.5px;
        padding:6px 10px;}
      .err{border:1px solid #E8C8C3;background:var(--danger-wash);
        border-radius:var(--r);padding:11px 13px;color:var(--danger);
        font-size:12px;line-height:1.65;margin-bottom:10px;flex:0 0 auto;}

      /* 代码抽屉 */
      .drawer{flex:0 0 auto;background:var(--surface);border:1px solid var(--line);
        border-radius:var(--r);}
      .drawer>summary{list-style:none;cursor:pointer;display:flex;align-items:center;
        gap:10px;padding:9px 14px;}
      .drawer>summary::-webkit-details-marker{display:none;}
      .drawer>summary:hover{background:#FAF9F5;}
      .drawer>summary:focus-visible{outline:2px solid var(--accent);outline-offset:-2px;}
      .drawer-title{font-family:var(--serif);font-size:12.5px;font-weight:600;
        color:var(--ink);flex:0 0 auto;}
      .drawer-hint{font-size:11px;color:var(--muted);flex:1;min-width:0;
        overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}
      .drawer>summary::after{content:"";flex:0 0 auto;width:6px;height:6px;
        border-right:1.4px solid var(--muted);border-bottom:1.4px solid var(--muted);
        transform:rotate(45deg) translateY(-2px);transition:transform .16s;}
      .drawer[open]>summary::after{transform:rotate(225deg) translateY(-1px);}
      .drawer[open]>summary{border-bottom:1px solid var(--line);}
      .drawer-body{padding:12px 14px 14px;max-height:min(30vh,260px);overflow:auto;
        -webkit-overflow-scrolling:touch;}
      .drawer-body::-webkit-scrollbar{width:9px;}
      .drawer-body::-webkit-scrollbar-thumb{background:var(--line-2);border-radius:5px;
        border:2px solid var(--surface);}
      .drawer-body::-webkit-scrollbar-track{background:transparent;}
      #summary,#export_code{margin:0 0 10px;background:var(--paper);
        border:1px solid var(--line);border-radius:var(--r);padding:10px 12px;
        font-family:var(--mono);font-size:11px;line-height:1.65;color:var(--ink-2);
        white-space:pre;overflow:auto;}
      /* 单层滚动：pre 不再各自限高，统一由抽屉体滚动 */
      #summary,#export_code{max-height:none;overflow:visible;max-width:100%;}
      .code-bar{display:flex;gap:8px;align-items:center;}

      /* 通知 */
      .shiny-notification{background:var(--ink);color:#F4F2EC;border:none;
        border-radius:var(--r);font-size:12px;padding:10px 13px;width:auto;
        max-width:380px;box-shadow:0 8px 24px rgba(26,36,32,.22);}
      .shiny-notification-close{color:#B7BFB9;opacity:1;}
      .shiny-notification-close:hover{color:#fff;}
      )---"))
    ),

    # ---- 标题栏 ----
    shiny::div(class = "app-header",
      shiny::div(class = "brand",
        shiny::tags$span(class = "brand-name", "Rclade Studio"),
        shiny::tags$span(class = "brand-rule"),
        shiny::tags$span(class = "brand-sub", lbl("app_title"))
      ),
      shiny::div(class = "header-right",
        shiny::tags$span(class = "ver-badge",
          shiny::HTML(paste0("v", .RC_VERSION, " · ", .RC_ARCH))),
        shiny::tags$button(id = "lang_toggle_btn", class = "lang-btn",
          `aria-label` = tr("lang_label"),
          onclick = "window.rcladeToggleLang&&window.rcladeToggleLang();",
          lbl("lang_toggle"))
      )
    ),

    # ---- 主体 ----
    shiny::div(class = "app-body",

      # ==== 左轨道：折叠分组 ====
      shiny::div(class = "rail",

        # 01 树来源
        grp("01", "grp_source", open = TRUE,
          shiny::div(class = "seg",
            shiny::radioButtons("tree_source", lbl("tree_source"),
              choiceNames = list(lbl("src_file_s"), lbl("src_paste_s"), lbl("src_example_s")),
              choiceValues = c("file", "paste", "example"),
              selected = "file", inline = TRUE)),
          shiny::conditionalPanel(
            condition = "input.tree_source == 'file'",
            shiny::fileInput("tree_file", lbl("tree_file"), width = "100%",
              accept = c(".tre", ".nwk", ".newick", ".nexus", ".nex", ".nhx", ".treefile")),
            txt("tree_path", "tree_path", placeholder = tr("tree_path_ph")),
            shiny::div(class = "grid2",
              num("tree_index", "tree_index", value = NULL, min = 1, step = 1),
              sel("multi_tree_mode", "multi_tree_mode", .opt("multimode", c("error","first","last","random","all")), "error")),
            hint("hint_index"),
            txt("export_tree_path", "export_tree_path", placeholder = tr("path_ph")),
            hint("hint_drag")
          ),
          shiny::conditionalPanel(
            condition = "input.tree_source == 'paste'",
            area("tree_paste", "tree_paste", rows = 6,
                 placeholder = "((A:1,B:2):3,C:4);"),
            hint("hint_paste")
          ),
          shiny::conditionalPanel(
            condition = "input.tree_source == 'example'",
            sel("example_dataset", "example_label",
              .opt("example", c("example_tree", "tree_a", "tree_b",
                                "tree_c", "tree_d", "tree_e")),
              "example_tree"),
            hint("hint_examples"),
            shiny::actionButton("load_example", lbl("btn_example"), class = "btn-default")
          )
        ),

        # 02 折叠与分组
        grp("02", "grp_collapse",
          sel("rank", "rank",
            .opt("rank", c("none","domain","phylum","class","order","family","genus","species","subspecies")),
            "phylum"),
          area("groups", "groups", rows = 3, placeholder = tr("groups_ph")),
          hint("hint_rank_mutex"),
          chk("groups_label_only", "groups_label_only", FALSE),
          hint("hint_label_only"),
          sel("triangle_mode", "triangle_mode",
            .opt("triangle", c("mixed","max","min","none")), "mixed"),
          shiny::uiOutput("triangle_note"),
          sel("space_mode", "space_mode",
            .opt("space", c("proportional","equal")), "proportional"),
          txt("clade", "clade", placeholder = tr("clade_ph")),
          chk("strict", "strict", FALSE)
        ),

        # 03 布局
        grp("03", "grp_layout",
          sel("layout", "layout",
            .opt("layout", c("rectangular","circular")), "rectangular"),
          shiny::sliderInput("angle", lbl("angle"), 10, 360, 360, step = 10, width = "100%"),
          num("line_width", "line_width", 1, min = 0.1, max = 5, step = 0.1),
          sel("tree_start_position", "tree_start_position",
            .opt("pos", c("right","left","top","bottom")), "right")
        ),

        # 04 时标
        grp("04", "grp_timescale",
          chk("add_timescale", "add_timescale", TRUE),
          sel("unit", "unit", .opt("unit", c("auto","Ga","Ma")), "auto"),
          hint("hint_unit"),
          shiny::uiOutput("unit_note"),
          sel("timescale_mode", "timescale_mode",
            .opt("tsmode", c("radial","linear")), "radial"),
          sel("timescale_position", "timescale_position",
            .opt("pos", c("right","left","top","bottom")), "right"),
          sel("timescale_levels", "timescale_levels",
            .opt("tslevels", c("eons,eras","eons,eras,periods","eras","eons","periods")),
            "eons,eras"),
          txt("timescale_version", "timescale_version", "ICS 2023/02"),
          chk("geo_events", "geo_events", FALSE)
        ),

        # 05 分类学解析
        grp("05", "grp_taxonomy",
          shiny::fileInput("taxonomy_file", lbl("taxonomy_file"), width = "100%",
            accept = c(".tsv", ".csv", ".txt")),
          shiny::conditionalPanel(
            condition = "input.taxonomy_file != null",
            txt("export_taxonomy_path", "export_taxonomy_path", placeholder = tr("path_ph"))
          ),
          sel("taxonomy_format", "taxonomy_format",
            .opt("taxformat", c("auto","GTDB","Silva","NCBI","custom_rank","custom_regex")),
            "auto"),
          area("custom_patterns", "custom_patterns", rows = 3,
               placeholder = tr("custom_patterns_ph")),
          sel("taxonomy_delimiter_mode", "taxonomy_delimiter_mode",
            .opt("taxdelim", c("reverse","greedy","segment")), "reverse"),
          txt("taxonomy_table_sep", "taxonomy_table_sep", ";"),
          txt("taxonomy_levels", "taxonomy_levels", placeholder = tr("taxonomy_levels_ph")),
          shiny::hr(),
          chk("taxonomy_file_header", "taxonomy_file_header", FALSE),
          chk("taxonomy_file_priority", "taxonomy_file_priority", TRUE),
          sel("taxonomy_file_sep", "taxonomy_file_sep",
            .opt("taxsep", c("auto","tab","comma")), "auto"),
          sel("taxonomy_source_priority", "taxonomy_source_priority",
            .opt("taxprio", c("auto","table","embedded")), "auto"),
          hint("hint_source_priority")
        ),

        # 06 配色
        grp("06", "grp_colors",
          sel("color_palette", "color_palette",
            .opt("palette", c("viridis","Set1","Set2","Set3","Paired","Dark2","Accent","rainbow")),
            "viridis"),
          shiny::uiOutput("palette_swatch"),
          txt("color_mapping", "color_mapping", placeholder = tr("color_mapping_ph")),
          txt("color_rank", "color_rank", placeholder = tr("color_rank_ph")),
          sel("legend_position", "legend_position",
            .opt("legend", c("bottom","right","left","top","none")), "bottom"),
          shiny::div(class = "grid2",
            num("legend_nrow", "legend_nrow", value = NULL),
            num("legend_ncol", "legend_ncol", value = NULL)),
          txt("legend_title", "legend_title", placeholder = tr("legend_title_ph"))
        ),

        # 07 注释与标注
        grp("07", "grp_annot",
          chk("show_tip_labels", "show_tip_labels", FALSE),
          num("tip_label_size", "tip_label_size", 3, min = 0.5, max = 10, step = 0.5),
          hint("hint_tip_labels"),
          shiny::hr(),
          chk("show_clade_label", "show_clade_label", FALSE),
          shiny::uiOutput("clade_label_note"),
          chk("show_clade_count", "show_clade_count", TRUE),
          shiny::div(class = "grid2",
            num("clade_label_offset", "clade_label_offset", 0, min = 0, max = 5000, step = 10),
            num("clade_label_fontsize", "clade_label_fontsize", 3, min = 1, max = 20, step = 0.5)),
          shiny::hr(),
          chk("show_support", "show_support", FALSE),
          num("support_threshold", "support_threshold", 0.95, min = 0, max = 1, step = 0.01),
          shiny::hr(),
          chk("show_hpd", "show_hpd", FALSE),
          txt("hpd_color", "hpd_color", "firebrick"),
          shiny::hr(),
          txt("highlight", "highlight", placeholder = tr("highlight_ph")),
          num("highlight_alpha", "highlight_alpha", 0.2, min = 0, max = 1, step = 0.05)
        ),

        # 08 输出与主题
        grp("08", "grp_output",
          txt("main_title", "main_title", placeholder = tr("optional_ph")),
          txt("sub_title", "sub_title", placeholder = tr("optional_ph")),
          sel("theme_fun", "theme_fun",
            .opt("theme", c("theme_timetree", "none")), "theme_timetree"),
          shiny::hr(),
          sel("export_format", "export_format", c("PDF" = "pdf", "PNG" = "png"), "pdf"),
          sel("export_size", "export_size",
            .opt("expsize", c("as_is", "a4_land", "a4_port", "slide", "custom")),
            "as_is"),
          shiny::conditionalPanel(
            condition = "input.export_size == 'custom'",
            shiny::div(class = "grid2",
              num("export_width", "export_width", 14, min = 2, max = 60, step = 0.5),
              num("export_height", "export_height", 10, min = 2, max = 60, step = 0.5))
          ),
          sel("export_dpi", "export_dpi", c("300", "600"), "300"),
          shiny::div(class = "grid2",
            num("width", "width", 14, min = 4, max = 40, step = 1),
            num("height", "height", 10, min = 3, max = 30, step = 1)),
          shiny::hr(),
          chk("ignore_malformed", "ignore_malformed", FALSE),
          chk("ignore_branch_length", "ignore_branch_length", FALSE),
          chk("low_memory", "low_memory", FALSE)
        )
      ),

      # ==== 右图版：预览优先 ====
      shiny::div(class = "main",
        shiny::div(class = "plate",
          shiny::div(class = "plate-bar",
            shiny::div(
              shiny::tags$span(class = "plate-title", lbl("plate_preview")),
              shiny::tags$span(class = "plate-sub", lbl("plate_auto"))),
            shiny::div(class = "plate-actions",
              shiny::downloadButton("dl_plot", lbl("btn_image"), class = "btn-warm"))
          ),
          shiny::div(class = "plate-canvas",
            shiny::uiOutput("plot_visibility"),
            shiny::uiOutput("status_note"),
            shiny::div(id = "plot_slot",
              shiny::plotOutput("tree_plot", height = "100%")),
            shiny::uiOutput("multi_slot")
          )
        ),
        shiny::tags$details(class = "drawer",
          shiny::tags$summary(
            shiny::tags$span(class = "drawer-title", lbl("code_header")),
            shiny::tags$span(class = "drawer-hint", lbl("code_header_hint"))),
          shiny::div(class = "drawer-body",
            shiny::verbatimTextOutput("summary"),
            shiny::verbatimTextOutput("export_code"),
            shiny::div(class = "code-bar",
              shiny::tags$button(id = "copy_code", type = "button",
                class = "btn btn-default",
                onclick = "window.rcladeCopy&&window.rcladeCopy();",
                lbl("btn_copy")),
              shiny::downloadButton("dl_script", lbl("btn_script"), class = "btn-default")
            )
          )
        )
      )
    ),

    # ---- 轻量交互层：语言热切换 + 分段控件状态 + 父开关联动 + 空态入口 ----
    shiny::tags$script(shiny::HTML(.i18n_payload())),
    shiny::tags$script(shiny::HTML(r"---(
    (function(){
      function I18N(){ return window.RCLADE_I18N ||
        {fileBrowse:'Choose', fileNone:'No file selected'}; }
      var DEPS = [
        ['show_support',     ['support_threshold']],
        ['show_hpd',         ['hpd_color']],
        ['show_clade_label', ['show_clade_count','clade_label_offset','clade_label_fontsize']],
        ['add_timescale',    ['timescale_mode','timescale_position','timescale_levels',
                              'timescale_version','geo_events']]
      ];
      function val(id){
        var e = document.getElementById(id);
        if (!e) return null;
        return e.type === 'checkbox' ? e.checked : e.value;
      }
      function box(el){
        while (el && el !== document.body) {
          if (el.classList && el.classList.contains('form-group')) return el;
          el = el.parentNode;
        }
        return null;
      }
      // 把 Shiny 文件输入的 Browse... / No file selected 换成当前语言
      function fixFile(){
        var btns = document.querySelectorAll('.btn-file');
        for (var i = 0; i < btns.length; i++) {
          var b = btns[i];
          for (var j = b.childNodes.length - 1; j >= 0; j--) {
            if (b.childNodes[j].nodeType === 3) b.removeChild(b.childNodes[j]);
          }
          var lab = b.querySelector('.flabel');
          if (!lab) {
            lab = document.createElement('span');
            lab.className = 'flabel';
            b.insertBefore(lab, b.firstChild);
          }
          lab.textContent = I18N().fileBrowse;
          var wrap = b.closest ? b.closest('.input-group') : null;
          var txt = wrap ? wrap.querySelector('input[type=text]') : null;
          if (txt) txt.placeholder = I18N().fileNone;
        }
      }
      // 空的 <pre>（无摘要 / 无代码）不占位，避免出现一条空灰条
      function tidyEmpty(){
        ['summary', 'export_code'].forEach(function(id){
          var e = document.getElementById(id);
          if (!e) return;
          e.classList.toggle('is-empty', !e.textContent.replace(/\s/g, ''));
        });
      }
      function sync(){
        fixFile();
        tidyEmpty();
        DEPS.forEach(function(p){
          var on = !!val(p[0]);
          p[1].forEach(function(c){
            var e = document.getElementById(c);
            if (!e) return;
            var b = box(e);
            if (b) b.classList.toggle('is-off', !on);
            e.setAttribute('aria-disabled', on ? 'false' : 'true');
          });
        });
        var segs = document.querySelectorAll('.seg .radio-inline');
        for (var i = 0; i < segs.length; i++) {
          var inp = segs[i].querySelector('input[type=radio]');
          segs[i].classList.toggle('is-on', !!(inp && inp.checked));
        }
      }
      function later(){ setTimeout(sync, 0); }
      document.addEventListener('DOMContentLoaded', sync);
      document.addEventListener('shiny:connected', sync);
      document.addEventListener('shiny:inputchanged', later);
      // 原生 change/input 兜底：Shiny 尚未绑定或事件名变化时，开关联动依然生效
      document.addEventListener('change', later, true);
      document.addEventListener('input', later, true);
      window.addEventListener('load', sync);
      setTimeout(sync, 250);
      setTimeout(sync, 900);

      ['summary', 'export_code'].forEach(function(id){
        var e = document.getElementById(id);
        if (e && window.MutationObserver) {
          new MutationObserver(tidyEmpty).observe(e, {childList: true, characterData: true, subtree: true});
        }
      });
      document.addEventListener('shiny:value', function(ev){
        if (ev.target && /^(summary|export_code)$/.test(ev.target.id)) setTimeout(tidyEmpty, 0);
      });

      // ---- 语言热切换：就地替换文案，不重启引擎、不丢当前绘图 ----
      function applyLang(lang){
        var S = (window.RCLADE_STR || {})[lang];
        if (!S) return false;
        document.documentElement.setAttribute('lang', lang === 'zh' ? 'zh-CN' : 'en');
        var nodes = document.querySelectorAll('[data-i18n]');
        for (var i = 0; i < nodes.length; i++){
          var k = nodes[i].getAttribute('data-i18n');
          if (k && S[k] != null) nodes[i].textContent = S[k];
        }
        var PH = window.RCLADE_PH || {};
        for (var id in PH){
          var e0 = document.getElementById(id);
          if (e0 && S[PH[id]] != null) e0.placeholder = S[PH[id]];
        }
        var O = (window.RCLADE_OPT || {})[lang] || {};
        var M = window.RCLADE_OPTID || {};
        for (var sid in M){
          var sl = document.getElementById(sid);
          var d = O[M[sid]];
          if (!sl || !d) continue;
          for (var j = 0; j < sl.options.length; j++){
            var v = sl.options[j].value;
            if (d[v] != null) sl.options[j].text = d[v];
          }
        }
        window.RCLADE_I18N = {fileBrowse: S.file_browse, fileNone: S.file_none};
        fixFile();
        window.__RCLADE_LANG = lang;
        return true;
      }
      window.rcladeToggleLang = function(){
        window.rcladeSetLang(window.__RCLADE_LANG === 'zh' ? 'en' : 'zh');
      };
      window.rcladeSetLang = function(lang){
        if (lang !== 'zh' && lang !== 'en') return;
        if (window.__RCLADE_LANG === lang) return;
        if (!applyLang(lang)) return;
        // 通知服务端：之后由 R 渲染的内容（导出代码注释、动态提示）用新语言
        try { Shiny.setInputValue('ui_lang', lang, {priority:'event'}); } catch (e) {}
        // 通知原生壳：只持久化偏好 + 换菜单，不重启引擎
        try {
          if (window.webkit && window.webkit.messageHandlers &&
              window.webkit.messageHandlers.rcladeLang) {
            window.webkit.messageHandlers.rcladeLang.postMessage(lang);
          }
        } catch (e) {}
      };

      window.rcladeCopy = function(){
        var el = document.getElementById('export_code');
        var b = document.getElementById('copy_code');
        if (!el || !b) return;
        var S = (window.RCLADE_STR || {})[window.__RCLADE_LANG || 'zh'] || {};
        var done = S.copy_done || 'OK';
        var fail = S.copy_fail || 'Failed';
        var idle = S.btn_copy || 'Copy';
        var t = el.textContent;
        function setIdle(txt){
          var sp = b.querySelector('[data-i18n]');
          if (sp) sp.textContent = txt; else b.textContent = txt;
        }
        function flash(ok){ setIdle(ok ? done : fail); setTimeout(function(){ setIdle(idle); }, 1500); }
        function fallback(){
          var ta = document.createElement('textarea');
          ta.value = t; ta.style.position = 'fixed'; ta.style.opacity = '0';
          document.body.appendChild(ta); ta.focus(); ta.select();
          var ok = false; try { ok = document.execCommand('copy'); } catch (e) {}
          document.body.removeChild(ta); flash(ok);
        }
        var p = (navigator.clipboard && navigator.clipboard.writeText)
          ? navigator.clipboard.writeText(t) : null;
        if (p && p.then) { p.then(function(){ flash(true); }).catch(fallback); } else { fallback(); }
      };

      window.rcladePickFile = function(){
        var inp = document.querySelector('.rail input[type=file]') ||
                  document.querySelector('input[type=file]');
        if (inp) { try { inp.click(); return; } catch (e) {} }
        try {
          Shiny.setInputValue('want_file', Date.now(), {priority:'event'});
        } catch (e) {}
      };
    })();
    )---"))
  )

  # -------------------------------------------------------------- server ----

  server <- function(input, output, session) {

    # ---- 界面语言热切换：页面侧已就地换文案，这里只更新后续渲染所用的语言 ----
    # priority 高于默认输出：保证同一帧内先换语言、再渲染带文案的输出
    shiny::observeEvent(input$ui_lang, {
      .RC_LANG <<- if (identical(input$ui_lang, "en")) "en" else "zh"
    }, ignoreNULL = TRUE, priority = 10)

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
      fill_example_title(input$example_dataset %||% "example_tree")
      shiny::showNotification(tr("notif_example"), duration = 4)
    })

    # ---- 示例本身就是某个类群的树：自动给图一个主标题（用户已填过就不覆盖）----
    fill_example_title <- function(k) {
      if (nzchar(trimws(input$main_title %||% ""))) return(invisible(NULL))
      t <- .ex_title(k)
      if (nzchar(t)) shiny::updateTextInput(session, "main_title", value = t)
    }
    shiny::observeEvent(input$example_dataset, {
      if (identical(input$tree_source, "example")) fill_example_title(input$example_dataset)
    }, ignoreInit = TRUE)

    # ---- 空态示例卡：点一下直接切源并选中该数据集（不必先认识下拉框）----
    shiny::observeEvent(input$pick_example, {
      k <- input$pick_example
      shiny::req(is.character(k), nzchar(k))
      shiny::updateRadioButtons(session, "tree_source", selected = "example")
      shiny::updateSelectInput(session, "example_dataset", selected = k)
      fill_example_title(k)
    })

    # ---- 空态「上传文件…」兜底：直接唤起系统文件选择失败时，至少切回文件源 ----
    shiny::observeEvent(input$want_file, {
      shiny::updateRadioButtons(session, "tree_source", selected = "file")
    })

    # ---- 配色色卡：让"选调色板"从背名字变成看颜色 ----
    output$palette_swatch <- shiny::renderUI({
      cols <- .PALETTE_SWATCHES[[input$color_palette %||% "viridis"]]
      if (is.null(cols)) return(NULL)
      shiny::div(class = "swatch",
        lapply(cols, function(cl) shiny::tags$span(class = "swatch-dot",
          style = sprintf("background:%s;", cl))))
    })

    # ---- 时标单位读数：把"选错单位会 ×1000"这件事在控件下方说清楚 ----
    output$unit_note <- shiny::renderUI({
      input$ui_lang                     # 语言切换时就地重算
      sc <- tree_scale()
      if (is.null(sc)) return(NULL)
      u <- input$unit %||% "auto"
      det <- sc$unit
      if (identical(u, "auto")) {
        return(shiny::div(class = "note",
          sprintf(tr("unit_note_auto"), .opt_label("unit", det), .fmt_age(sc$root))))
      }
      ma <- if (identical(u, "Ga")) sc$root * 1000 else sc$root
      if (ma > 4600 || ma < 0.001) {
        return(shiny::div(class = "note note-warn",
          sprintf(tr("unit_note_bad"), .opt_label("unit", det),
                  .opt_label("unit", u), .fmt_age(ma))))
      }
      shiny::div(class = "note",
        sprintf(tr("unit_note_ok"), .opt_label("unit", u), .fmt_age(ma)))
    })

    # ---- 三角模式=「不画三角」时：折叠的类群会被整体收掉，末端分支消失 ----
    # 包内实现是 ggtree::collapse(mode = "none")：类群收成一个点且不画三角形，
    # 内部的末端分支与标签都不再绘制。这里提前说明后果，并给一键改回来的出口。
    output$triangle_note <- shiny::renderUI({
      input$ui_lang
      if (!identical(input$triangle_mode %||% "mixed", "none")) return(NULL)
      g <- trimws(input$groups %||% "")
      c_ <- trimws(input$clade %||% "")
      rank_on <- !identical(input$rank %||% "none", "none")
      if (!rank_on && !nzchar(g) && !nzchar(c_)) return(NULL)   # 没有折叠，就没影响
      shiny::div(class = "note note-warn",
        shiny::tags$p(style = "margin:0;", tr("tri_none_body")),
        # 只有「折叠分类阶元」在起作用时，一键改回不折叠才真的能解决问题
        if (rank_on && !nzchar(g) && !nzchar(c_)) {
          shiny::div(style = "margin-top:8px;",
            shiny::actionButton("tri_fix_rank", lbl("tri_none_fix"), class = "btn-default"))
        })
    })

    shiny::observeEvent(input$tri_fix_rank, {
      shiny::updateSelectInput(session, "rank", selected = "none")
    })

    # ---- 类群标签提示：说明它只标注"被折叠的类群" ----
    output$clade_label_note <- shiny::renderUI({
      input$ui_lang
      if (!isTRUE(input$show_clade_label)) return(NULL)
      rank_on <- !identical(input$rank %||% "none", "none")
      has_grp <- nzchar(trimws(input$groups %||% "")) || nzchar(trimws(input$clade %||% ""))
      show_tip <- isTRUE(input$show_tip_labels)

      txt <- if (has_grp) {
        NULL                                   # 有分组：标签正常画，不需要解释
      } else if (rank_on) {
        tr("clade_label_tax")                  # 靠阶元折叠：解释为什么没有类群可标
      } else {
        tr("clade_label_tip_hint")             # 完全不折叠：指向末端标签
      }
      if (is.null(txt) && show_tip) return(NULL)

      shiny::div(class = paste("note", if (!has_grp && !rank_on) "note-warn" else ""),
        if (!is.null(txt)) shiny::tags$p(style = "margin:0;", txt),
        if (!show_tip) shiny::div(
          style = if (is.null(txt)) "" else "margin-top:8px;",
          shiny::actionButton("tip_fix", lbl("tip_label_fix"), class = "btn-default")))
    })

    shiny::observeEvent(input$tip_fix, {
      shiny::updateCheckboxInput(session, "show_tip_labels", value = TRUE)
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
            return(structure(list(msg = tr("err_paste_empty"), msg_key = "err_paste_empty"),
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
              return(structure(list(msg = tr("err_file_empty"), msg_key = "err_file_empty"),
                               class = c("tree_error", "tree_empty")))
            }
            path <- up$datapath
          }
          Rclade::read_tree_auto(path, tree_index = idx, multi_tree_mode = mode)
        }
      }, error = function(e) structure(list(msg = conditionMessage(e)), class = "tree_error"))
    })

    # ---- 树自身的时间量级：用来识别"分支长度单位"，并在手动选错时给出警告 ----
    # 背景：Rclade 的 unit 表示"输入树的分支长度单位"，unit="Ga" 会把所有分支长度 ×1000
    # 再按 Ma 绘制。所以一棵 Ma 树被标成 Ga，时标会直接放大 1000 倍（180 Ma → 180,000 Ma）。
    tree_scale <- shiny::reactive({
      t <- tryCatch(tree_object(), error = function(e) NULL)
      if (is.null(t) || inherits(t, "tree_error")) return(NULL)
      if (inherits(t, "multiPhylo")) t <- t[[1]]
      if (!inherits(t, "phylo")) return(NULL)
      ra <- tryCatch(max(ape::node.depth.edgelength(t), na.rm = TRUE),
                     error = function(e) NA_real_)
      if (!is.finite(ra) || ra <= 0) return(NULL)
      # 根年龄 >= 10 → 分支长度本身就是 Ma（显生宙以来的树都落在这档）；
      # 否则按 Ga 解读。仅用于默认值与提示，用户仍可手动覆盖。
      list(root = ra, unit = if (ra >= 10) "Ma" else "Ga")
    })

    unit_resolved <- shiny::reactive({
      u <- input$unit %||% "auto"
      if (!identical(u, "auto")) return(u)
      sc <- tree_scale()
      if (is.null(sc)) "Ma" else sc$unit
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
      .collect_params(input, parsed, tax_priority, unit_resolved = unit_resolved())
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
        return(list(ok = FALSE, empty = FALSE, cond = d, err = .parse_err_msg(d)))
      }
      t <- d$tree
      if (inherits(t, "tree_error")) {
        return(list(ok = FALSE, empty = inherits(t, "tree_empty"),
                    err = t$msg, msg_key = t$msg_key))
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
      input$ui_lang          # 语言切换后重算导出代码里的注释
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
        # C3 / P2-6：无图时「导出图像」视觉禁用且不可点。
        # 注意：只锁导出图像——复制代码/写脚本与是否有图无关，不应连带禁用。
        css <- c(css, ".plate-actions{opacity:.4;pointer-events:none;}")
      }
      shiny::tags$style(shiny::HTML(paste(css, collapse = "\n")))
    })

    # 空态 / 错误提示（P0-1：不再无保护调用 params_d()；错误时保留已画好的图，P0-4）
    output$status_note <- shiny::renderUI({
      input$ui_lang          # 语言切换后按新语言重新生成提示
      res <- plot_res()
      if (res$ok) return(NULL)
      if (isTRUE(res$empty)) {
        # 空态承担引导职责：拖放说明 + 六个可点击示例卡（识别优于回忆）
        return(shiny::div(class = "empty", `role` = "status", `aria-live` = "polite",
          shiny::HTML(paste0(
            '<svg class="empty-mark" viewBox="0 0 52 52" fill="none" ',
            'stroke="currentColor" stroke-width="1.4" stroke-linecap="round">',
            '<path d="M9 6v40"/><path d="M9 14h12M9 26h12M9 38h12"/>',
            '<path d="M21 10v8M21 22v8M21 34v8"/>',
            '<path d="M21 10h13M21 18h11M21 26h13M21 34h11M21 42h13"/>',
            '</svg>')),
          shiny::tags$h3(class = "empty-title", lbl("empty_title")),
          shiny::tags$p(class = "empty-body", lbl("empty_body")),
          shiny::div(class = "tiles", lapply(.EX_TILES, .ex_tile)),
          shiny::div(class = "empty-actions",
            shiny::tags$button(type = "button", class = "btn btn-default",
              onclick = "window.rcladePickFile&&window.rcladePickFile();",
              lbl("chip_upload")))
        ))
      }
      msg <- if (inherits(res$cond, "rclade_version_error")) {
        # 版本类错误的条件对象里存的是"当初那种语言"的消息，这里按当前语言重建
        paste0(tr("err_version_prefix"), tr("err_version"))
      } else if (inherits(res$cond, "condition")) {
        .parse_err_msg(res$cond)
      } else if (!is.null(res$msg_key)) {
        tr(res$msg_key)
      } else {
        res$err
      }
      shiny::div(class = "err", `role` = "alert", `aria-live` = "assertive", msg)
    })

    # 多树并排：仅当结果为 rclade_plot_list 时渲染 N 个 plotOutput 槽位
    output$multi_slot <- shiny::renderUI({
      res <- plot_res()
      shiny::req(res$ok, inherits(res$p, "rclade_plot_list"))
      n <- min(length(res$p), 6)
      shiny::tagList(
        shiny::div(class = "multi-note",
          sprintf(tr("hint_multi_fmt"), length(res$p), n)),
        lapply(seq_len(n), function(i) {
          shiny::div(class = "multi-out",
            shiny::plotOutput(paste0("multi_plot_", i), height = "440px"))
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
