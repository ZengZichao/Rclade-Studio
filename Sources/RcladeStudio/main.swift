import Cocoa
import WebKit
import UniformTypeIdentifiers

/// 界面语言：默认跟随系统 + 应用内可切换（切换值持久化到偏好文件，壳与引擎共用同一来源）。
enum Lang {
    static var code: String = current()          // "zh" / "en"，进程内单一真源
    static let prefPath = NSHomeDirectory()
        + "/Library/Application Support/RcladeStudio/language.txt"
    static func detectSystem() -> String {
        (Locale.preferredLanguages.first ?? "").hasPrefix("zh") ? "zh" : "en"
    }
    static func current() -> String {
        if let s = try? String(contentsOfFile: prefPath, encoding: .utf8)
            .trimmingCharacters(in: .whitespacesAndNewlines),
           s == "zh" || s == "en" { return s }
        return detectSystem()
    }
    static func write(_ v: String) {
        let dir = (prefPath as NSString).deletingLastPathComponent
        try? FileManager.default.createDirectory(atPath: dir, withIntermediateDirectories: true)
        try? v.write(toFile: prefPath, atomically: true, encoding: .utf8)
    }
}

/// 原生壳侧可本地化字符串（菜单 / 弹窗 / 占位页 / 拖放标题），zh / en 两套纯术语。
enum S {
    static let zh: [String: String] = [
        "appName": "Rclade Studio",
        "m_about": "关于 Rclade Studio", "m_quit": "退出 Rclade Studio",
        "m_services": "服务", "m_hide": "隐藏 Rclade Studio",
        "m_hide_others": "隐藏其他", "m_show_all": "全部显示",
        "m_edit": "编辑", "m_view": "视图", "m_settings": "设置",
        "m_window": "窗口", "m_help": "帮助",
        "e_undo": "撤销", "e_redo": "重做", "e_cut": "剪切", "e_copy": "拷贝",
        "e_paste": "粘贴", "e_select_all": "全选",
        "v_reload": "重新加载页面", "s_choose_r": "选择 Rscript…",
        "s_clear_r": "清除 R 记忆并重新检测",
        "s_toggle_lang": "切换到 English",
        "w_minimize": "最小化", "w_zoom": "缩放", "w_front": "前置全部窗口",
        "ph_locating": "正在定位 R 环境并启动引擎，首次启动可能需要 1–2 分钟…",
        "ph_starting": "正在启动 R 引擎并加载依赖（首次可能偏慢）…",
        "ph_restarting": "正在重启 R 引擎…",
        "fail_no_port": "无法分配本地端口，请重试。",
        "fail_no_r": "未检测到 R。请先安装 R (≥ 4.1)，并在其中安装 Rclade 包：\n\n    install.packages(\"Rclade\")\n\nconda/mamba 用户也可在环境中安装后重启本应用。",
        "fail_engine_start": "R 引擎进程启动失败：",
        "fail_title": "Rclade Studio 无法启动", "quit": "退出",
        "died_title": "Rclade 引擎已退出（exit %d）",
        "died_body": "引擎日志（末尾）：\n%@\n\n按上方日志安装缺失依赖后可重启引擎。重启会丢失当前绘图。",
        "died_restart": "重启引擎",
        "fail_timeout": "等待引擎就绪超时。\n\n引擎日志（末尾）：\n%@",
        "choose_panel": "选择 Rscript",
        "choose_bad_title": "不是有效的 Rscript",
        "choose_bad_body": "%@\n\n`Rscript --version` 未通过，请选择 R 安装目录下的 bin/Rscript。",
        "choose_saved_title": "已记住 R 路径",
        "choose_saved_body": "%@\n\n引擎正在运行：要立即用新路径重启引擎吗？",
        "choose_restart_now": "立即重启引擎", "choose_later": "稍后（下次启动生效）",
        "cleared_title": "已清除 R 记忆",
        "cleared_body": "下次启动将重新扫描并探测装有 Rclade 的 R。", "ok": "好",
        "drop_loaded": "%@ — 已载入 %@", "drop_failed": "%@ — 拖入文件写入失败",
        "download_fail": "下载失败",
    ]
    static let en: [String: String] = [
        "appName": "Rclade Studio",
        "m_about": "About Rclade Studio", "m_quit": "Quit Rclade Studio",
        "m_services": "Services", "m_hide": "Hide Rclade Studio",
        "m_hide_others": "Hide Others", "m_show_all": "Show All",
        "m_edit": "Edit", "m_view": "View", "m_settings": "Settings",
        "m_window": "Window", "m_help": "Help",
        "e_undo": "Undo", "e_redo": "Redo", "e_cut": "Cut", "e_copy": "Copy",
        "e_paste": "Paste", "e_select_all": "Select All",
        "v_reload": "Reload Page", "s_choose_r": "Choose Rscript…",
        "s_clear_r": "Clear R memory & re-detect",
        "w_minimize": "Minimize", "w_zoom": "Zoom", "w_front": "Bring All to Front",
        "ph_locating": "Locating R and starting the engine — first launch may take 1–2 minutes…",
        "ph_starting": "Starting the R engine and loading dependencies (first run may be slow)…",
        "ph_restarting": "Restarting the R engine…",
        "fail_no_port": "Could not allocate a local port. Please retry.",
        "fail_no_r": "R was not detected. Install R (≥ 4.1) and then the Rclade package:\n\n    install.packages(\"Rclade\")\n\nconda/mamba users can install it into an environment and relaunch.",
        "fail_engine_start": "Failed to start the R engine process:",
        "fail_title": "Rclade Studio cannot start", "quit": "Quit",
        "died_title": "The Rclade engine exited (exit %d)",
        "died_body": "Engine log (tail):\n%@\n\nInstall the missing dependencies per the log, then restart the engine. Restarting discards the current plot.",
        "died_restart": "Restart engine",
        "fail_timeout": "Timed out waiting for the engine to become ready.\n\nEngine log (tail):\n%@",
        "choose_panel": "Choose Rscript",
        "choose_bad_title": "Not a valid Rscript",
        "choose_bad_body": "%@\n\n`Rscript --version` failed. Choose bin/Rscript inside an R installation.",
        "choose_saved_title": "R path remembered",
        "choose_saved_body": "%@\n\nThe engine is running: restart it now with the new path?",
        "choose_restart_now": "Restart engine now", "choose_later": "Later (next launch)",
        "cleared_title": "R memory cleared",
        "cleared_body": "The next launch will rescan and probe for an R with Rclade installed.", "ok": "OK",
        "drop_loaded": "%@ — Loaded %@", "drop_failed": "%@ — failed to write dropped file",
        "download_fail": "Download failed",
    ]
}
/// 按当前语言取原生壳字符串（缺键回退键名）。
func L(_ key: String) -> String { (S.zh[key] != nil && Lang.code == "zh" ? S.zh[key] : S.en[key]) ?? key }

/// 把增强引擎页面发来的语言切换消息转发给 AppController（弱引用，避免循环）。
final class LangMessageHandler: NSObject, WKScriptMessageHandler {
    weak var owner: AppController?
    func userContentController(_ uc: WKUserContentController, didReceive message: WKScriptMessage) {
        if let v = message.body as? String { owner?.handleLanguageChange(v) }
    }
}

/// 接收文件拖放的 WKWebView：拖 .nwk/.tre/.nex 进窗口即把路径交给壳层
///（壳写入 dropfile，引擎轮询自动载入；原路径被记录，导出代码可复现）。
/// NSView 已隐式遵循 NSDraggingDestination，这里只覆盖拖放方法。
final class DropWebView: WKWebView {
    var onDropFiles: (([String]) -> Void)?

    /// P2-7：仅接受受支持的树文件扩展名；其余类型（PDF/图片等）直接忽略，不把无效路径灌给引擎。
    private let allowedExts: Set<String> =
        ["nwk", "newick", "tre", "tree", "treefile", "nex", "nexus", "nhx"]

    override init(frame: NSRect, configuration: WKWebViewConfiguration) {
        super.init(frame: frame, configuration: configuration)
        registerForDraggedTypes([.fileURL])
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

    private func fileURLs(_ sender: NSDraggingInfo) -> [URL] {
        (sender.draggingPasteboard.readObjects(forClasses: [NSURL.self],
            options: [.urlReadingFileURLsOnly: true]) as? [URL]) ?? []
    }

    private func accepted(_ urls: [URL]) -> [URL] {
        urls.filter { allowedExts.contains($0.pathExtension.lowercased()) }
    }

    /// P2-7：拖入悬停时的高亮反馈。
    private func highlight(_ on: Bool) {
        wantsLayer = true
        layer?.borderColor = (on ? NSColor.controlAccentColor.cgColor : NSColor.clear.cgColor)
        layer?.borderWidth = on ? 3 : 0
    }

    override func draggingEntered(_ sender: NSDraggingInfo) -> NSDragOperation {
        let ok = !accepted(fileURLs(sender)).isEmpty
        highlight(ok)
        return ok ? .copy : []
    }

    override func draggingExited(_ sender: NSDraggingInfo?) { highlight(false) }
    override func draggingEnded(_ sender: NSDraggingInfo) { highlight(false) }

    override func performDragOperation(_ sender: NSDraggingInfo) -> Bool {
        highlight(false)
        let urls = accepted(fileURLs(sender))
        guard let first = urls.first else { return false }
        onDropFiles?([first.path])   // B10：单文件语义，只取第一个，避免"最后一个生效却暗示复数"
        return true   // 拦截：不让 WKWebView 把拖放当页面导航
    }
}

/// Rclade Studio 薄壳：不绘图、不复制任何逻辑，只负责
///   发现 R → 校验依赖 → 以本地端口起包内 Shiny 引擎 → 用 WKWebView 呈现为真正的 App 窗口。
/// 引擎就绪以 handshake 文件为准（launch_engine.R 的 launch.browser 回调写真实 URL），
/// 规避"外壳选好端口→释放→R 重新绑定"之间的 TOCTOU 竞态。
final class AppController: NSObject, NSApplicationDelegate, NSWindowDelegate {

    let window: NSWindow
    let web: DropWebView
    let engine = EngineProcess()
    private var handshakePath = ""
    private var port: UInt16 = 0
    /// 用户主动关窗/退出或启动失败——抑制"引擎异常退出"弹窗
    private var shuttingDown = false
    /// 语言切换消息处理器（由 userContentController 强引用，这里再持一份保险）
    private var langHandlerRef: LangMessageHandler?
    /// 拖入文件路径的落地文件，引擎轮询读取（launch_engine.R 第 3 个参数）
    private var dropfilePath: String {
        NSHomeDirectory() + "/Library/Application Support/RcladeStudio/drop.txt"
    }

    override init() {
        let frame = NSRect(x: 0, y: 0, width: 1180, height: 800)
        let config = WKWebViewConfiguration()
        let langHandler = LangMessageHandler()
        config.userContentController.add(langHandler, name: "rcladeLang")
        web = DropWebView(frame: frame, configuration: config)
        window = NSWindow(contentRect: frame,
                          styleMask: [.titled, .closable, .miniaturizable, .resizable],
                          backing: .buffered, defer: false)
        super.init()
        langHandler.owner = self
        langHandlerRef = langHandler
        window.title = L("appName")
        window.minSize = NSSize(width: 1100, height: 720)   // 固定布局：左侧 420px 最小 + 右侧绘图区
        window.center()
        window.delegate = self
        window.contentView = web
        web.navigationDelegate = self
    }

    func applicationDidFinishLaunching(_ note: Notification) {
        // 第二实例启动 → 本窗口置前
        DistributedNotificationCenter.default().addObserver(
            forName: SingleInstance.activateNotification, object: nil, queue: .main
        ) { [weak self] _ in
            self?.window.makeKeyAndOrderFront(nil)
            NSApp.activate(ignoringOtherApps: true)
        }
        buildMenus()
        window.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)

        // M3 拖放：把拖入的树文件路径（单个）写入 dropfile，引擎轮询自动载入（原路径留存，导出可复现）
        web.onDropFiles = { [weak self] paths in
            guard let self = self, let first = paths.first else { return }
            let dir = (self.dropfilePath as NSString).deletingLastPathComponent
            try? FileManager.default.createDirectory(atPath: dir, withIntermediateDirectories: true)
            let written = (try? first.write(toFile: self.dropfilePath,
                                            atomically: true, encoding: .utf8)) != nil
            let name = (first as NSString).lastPathComponent
            self.window.title = written ? self.localizedLoaded(name) : self.localizedLoadFailed()
            DispatchQueue.main.asyncAfter(deadline: .now() + 2.5) { [weak self] in
                self?.window.title = L("appName")
            }
        }

        launchEngine()
    }

    /// 组装"已载入 <文件>"标题（zh: "%@ — 已载入 %@"；en: "%@ — Loaded %@"），按语言用 String(format:)。
    private func localizedLoaded(_ file: String) -> String {
        String(format: L("drop_loaded"), L("appName"), file)
    }
    private func localizedLoadFailed() -> String {
        String(format: L("drop_failed"), L("appName"))
    }

    // MARK: 引擎生命周期

    private func showPlaceholder(_ text: String) {
        web.loadHTMLString("""
            <div style="font-family:-apple-system,sans-serif;color:#6B7280;\
            display:flex;height:100vh;align-items:center;justify-content:center;">\
            \(text)</div>
            """, baseURL: nil)
    }

    private func launchEngine() {
        // 清掉上次运行遗留的拖入记录，避免旧路径自动灌进新会话
        try? FileManager.default.removeItem(atPath: dropfilePath)
        guard let free = RLocator.findFreePort() else {
            return fail(L("fail_no_port"))
        }
        port = free
        if !handshakePath.isEmpty { try? FileManager.default.removeItem(atPath: handshakePath) }
        handshakePath = NSTemporaryDirectory() + "rclade_engine_handshake_\(UUID().uuidString).txt"
        try? FileManager.default.removeItem(atPath: handshakePath)

        // P0-5/B6：先给出可见占位页，再在后台队列探测 R（含逐个 requireNamespace，最坏可达约一分钟），
        // 避免这段耗时同步阻塞主线程导致窗口白屏冻结、被系统判"无响应"。
        showPlaceholder(L("ph_locating"))
        RLocator.findRscriptAsync { [weak self] rscript in
            guard let self = self, !self.shuttingDown else { return }
            guard let rscript = rscript else {
                return self.fail(L("fail_no_r"))
            }
            self.startEngine(with: rscript)
        }
    }

    /// 真正起 R 子进程并开始等待握手就绪。
    private func startEngine(with rscript: String) {
        showPlaceholder(L("ph_starting"))
        engine.onUnexpectedExit = { [weak self] code in self?.engineDied(code) }
        guard engine.start(rscript: rscript, port: port, handshake: handshakePath,
                           dropfile: dropfilePath, lang: Lang.code) else {
            return fail("\(L("fail_engine_start"))\n\n\(engine.log)")
        }
        poll(attempts: 250) // ≈ 100 秒（Bioconductor 依赖的首次 loadNamespace 可能偏慢）
    }

    /// 就地重启引擎（异常退出恢复 / 设置新 R 路径后生效），换新端口与握手文件。
    private func restartEngine() {
        // P2-5：先渲染占位页，再异步 stop+launch。若在 loadHTMLString 后同步 stop（最长阻塞 5s），
        // WebKit 的导航排在主线程 run loop 上，占位页根本画不出来，用户只看到卡住的旧页面。
        showPlaceholder(L("ph_restarting"))
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) { [weak self] in
            guard let self = self, !self.shuttingDown else { return }
            self.engine.stop()
            self.launchEngine()
        }
    }

    /// 引擎在非用户操作下退出（缺依赖/崩溃/端口被占）：给"重启引擎"出路而非直接死掉。
    private func engineDied(_ code: Int32) {
        guard !shuttingDown else { return }
        if code == 3 || code == 4 { RLocator.invalidateCache() }   // 缺依赖：缓存的环境可能坏了，下次重扫
        let a = NSAlert()
        a.alertStyle = .warning
        a.messageText = String(format: L("died_title"), code)
        a.informativeText = String(format: L("died_body"), excerpt(engine.log))
        a.addButton(withTitle: L("died_restart"))
        a.addButton(withTitle: L("quit"))
        if a.runModal() == .alertFirstButtonReturn {
            restartEngine()
        } else {
            shuttingDown = true
            NSApp.terminate(nil)
        }
    }

    private func poll(attempts: Int) {
        let raw = try? String(contentsOfFile: handshakePath, encoding: .utf8)
        if let s = raw?.trimmingCharacters(in: .whitespacesAndNewlines),
           !s.isEmpty, let url = URL(string: s) {
            web.load(URLRequest(url: url))
            return
        }
        if attempts <= 0 { return fail(String(format: L("fail_timeout"), excerpt(engine.log))) }
        // 引擎已自行退出（缺依赖/崩溃）：engineDied 弹窗接管 UI，停止本轮轮询
        if engine.hasExited { return }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.4) { [weak self] in
            guard let self = self, !self.shuttingDown else { return }
            self.poll(attempts: attempts - 1)
        }
    }

    private func fail(_ msg: String) {
        shuttingDown = true
        engine.stop()
        let a = NSAlert()
        a.alertStyle = .critical
        a.messageText = L("fail_title")
        a.informativeText = msg
        a.addButton(withTitle: L("quit"))
        a.runModal()
        NSApp.terminate(nil)
    }

    private func excerpt(_ s: String, max: Int = 1200) -> String {
        s.count > max ? "…\(s.suffix(max))" : s
    }

    // MARK: 菜单
    // 编辑菜单缺位会导致 WKWebView 文本框没有 ⌘C/⌘V/⌘X 快捷键（快捷键由菜单经 responder 链分发）。

    private func buildMenus() {
        let main = NSMenu()

        let appItem = NSMenuItem()
        let appMenu = NSMenu()
        appMenu.addItem(withTitle: L("m_about"),
                        action: #selector(NSApplication.orderFrontStandardAboutPanel(_:)), keyEquivalent: "")
        appMenu.addItem(.separator())
        let services = NSMenu(title: L("m_services"))
        NSApp.servicesMenu = services
        let servicesItem = appMenu.addItem(withTitle: L("m_services"), action: nil, keyEquivalent: "")
        servicesItem.submenu = services
        appMenu.addItem(.separator())
        appMenu.addItem(withTitle: L("m_hide"),
                        action: #selector(NSApplication.hide(_:)), keyEquivalent: "h")
        let hideOthers = appMenu.addItem(withTitle: L("m_hide_others"),
                        action: #selector(NSApplication.hideOtherApplications(_:)), keyEquivalent: "h")
        hideOthers.keyEquivalentModifierMask = [.command, .option]
        appMenu.addItem(withTitle: L("m_show_all"),
                        action: #selector(NSApplication.unhideAllApplications(_:)), keyEquivalent: "")
        appMenu.addItem(.separator())
        appMenu.addItem(withTitle: L("m_quit"),
                        action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
        appItem.submenu = appMenu
        main.addItem(appItem)

        let editItem = NSMenuItem()
        let editMenu = NSMenu(title: L("m_edit"))
        editMenu.addItem(withTitle: L("e_undo"), action: Selector(("undo:")), keyEquivalent: "z")
        editMenu.addItem(withTitle: L("e_redo"), action: Selector(("redo:")), keyEquivalent: "Z")
        editMenu.addItem(.separator())
        editMenu.addItem(withTitle: L("e_cut"), action: #selector(NSText.cut(_:)), keyEquivalent: "x")
        editMenu.addItem(withTitle: L("e_copy"), action: #selector(NSText.copy(_:)), keyEquivalent: "c")
        editMenu.addItem(withTitle: L("e_paste"), action: #selector(NSText.paste(_:)), keyEquivalent: "v")
        editMenu.addItem(.separator())
        editMenu.addItem(withTitle: L("e_select_all"), action: #selector(NSText.selectAll(_:)), keyEquivalent: "a")
        editItem.submenu = editMenu
        main.addItem(editItem)

        let viewItem = NSMenuItem()
        let viewMenu = NSMenu(title: L("m_view"))
        let reload = viewMenu.addItem(withTitle: L("v_reload"), action: #selector(reloadPage), keyEquivalent: "r")
        reload.target = self
        viewItem.submenu = viewMenu
        main.addItem(viewItem)

        let setItem = NSMenuItem()
        let setMenu = NSMenu(title: L("m_settings"))
        let pick = setMenu.addItem(withTitle: L("s_choose_r"), action: #selector(chooseRscript(_:)), keyEquivalent: "")
        pick.target = self
        let clear = setMenu.addItem(withTitle: L("s_clear_r"), action: #selector(clearRCache(_:)), keyEquivalent: "")
        clear.target = self
        setMenu.addItem(.separator())
        // 界面语言切换（网页 header 按钮之外的备用入口；与按钮同走 handleLanguageChange）
        let langItem = setMenu.addItem(withTitle: L("s_toggle_lang"),
                                       action: #selector(toggleLanguage(_:)), keyEquivalent: "")
        langItem.target = self
        setItem.submenu = setMenu
        main.addItem(setItem)

        let windowItem = NSMenuItem()
        let windowMenu = NSMenu(title: L("m_window"))
        windowMenu.addItem(withTitle: L("w_minimize"),
                           action: #selector(NSWindow.performMiniaturize(_:)), keyEquivalent: "m")
        windowMenu.addItem(withTitle: L("w_zoom"),
                           action: #selector(NSWindow.performZoom(_:)), keyEquivalent: "")
        windowMenu.addItem(.separator())
        windowMenu.addItem(withTitle: L("w_front"),
                           action: #selector(NSApplication.arrangeInFront(_:)), keyEquivalent: "")
        NSApp.windowsMenu = windowMenu
        windowItem.submenu = windowMenu
        main.addItem(windowItem)

        let helpItem = NSMenuItem()
        let helpMenu = NSMenu(title: L("m_help"))
        NSApp.helpMenu = helpMenu   // 系统会自动注入帮助搜索框
        helpItem.submenu = helpMenu
        main.addItem(helpItem)

        NSApp.mainMenu = main
    }

    /// 来自增强引擎页面的语言切换请求（WKScriptMessageHandler → 此）。持久化并以新语言重启引擎、重建原生菜单。
    @objc func handleLanguageChange(_ new: String) {
        let l = (new == "en") ? "en" : "zh"
        guard l != Lang.code else { return }
        Lang.code = l
        Lang.write(l)
        buildMenus()                 // 原生菜单即刻换新语言
        window.title = L("appName")  // 复位窗口标题（拖放临时标题已过时）
        restartEngine()              // 引擎按新语言重启（会重置绘图会话，属语言切换的预期代价）
    }

    /// 设置菜单"切换界面语言"项：与网页 header 按钮等效，走同一条切换链路。
    @objc private func toggleLanguage(_ sender: Any?) {
        handleLanguageChange(Lang.code == "zh" ? "en" : "zh")
    }

    // MARK: 设置：手动指定 R（写入缓存即全局生效；引擎运行中可立即重启生效）

    @objc private func chooseRscript(_ sender: Any?) {
        let panel = NSOpenPanel()
        panel.title = L("choose_panel")
        panel.canChooseFiles = true
        panel.canChooseDirectories = false
        panel.allowsMultipleSelection = false
        panel.directoryURL = URL(fileURLWithPath: "/opt/homebrew/bin")
        panel.beginSheetModal(for: window) { resp in
            guard resp == .OK, let url = panel.url else { return }
            let path = url.path
            guard RLocator.rscriptVersionPublic(path) != nil else {
                let a = NSAlert()
                a.alertStyle = .warning
                a.messageText = L("choose_bad_title")
                a.informativeText = String(format: L("choose_bad_body"), path)
                a.runModal()
                return
            }
            RLocator.saveCache(path)
            let a = NSAlert()
            a.alertStyle = .informational
            a.messageText = L("choose_saved_title")
            a.informativeText = String(format: L("choose_saved_body"), path)
            a.addButton(withTitle: L("choose_restart_now"))
            a.addButton(withTitle: L("choose_later"))
            if a.runModal() == .alertFirstButtonReturn { self.restartEngine() }
        }
    }

    @objc private func clearRCache(_ sender: Any?) {
        RLocator.invalidateCache()
        let a = NSAlert()
        a.alertStyle = .informational
        a.messageText = L("cleared_title")
        a.informativeText = L("cleared_body")
        a.addButton(withTitle: L("ok"))
        a.runModal()
    }

    /// ⌘R 会整页重载：Shiny 会话重置（输入回默认、当前绘图清空）。绘图参数量级小，重画成本低，
    /// 保留标准浏览器行为并在方案文档注明。
    @objc private func reloadPage() { web.reload() }

    // MARK: 窗口/应用生命周期

    func windowWillClose(_ n: Notification) {
        shuttingDown = true
        engine.stop()
    }
    func applicationShouldTerminateAfterLastWindowClosed(_ s: NSApplication) -> Bool { true }
    func applicationWillTerminate(_ notification: Notification) {
        shuttingDown = true
        SingleInstance.release()
        if !handshakePath.isEmpty { try? FileManager.default.removeItem(atPath: handshakePath) }
        engine.stop()
    }
}

// MARK: - 导航策略：WKWebView 只允许本机引擎，页面外链走系统默认浏览器

extension AppController: WKNavigationDelegate {
    func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction,
                 decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        guard let url = navigationAction.request.url else { decisionHandler(.cancel); return }
        let isLocalHost = url.host == "127.0.0.1" || url.host == "localhost"
        let isInternalScheme = url.scheme == "about" || url.scheme == "data" || url.scheme == "blob"
        if isLocalHost || isInternalScheme {
            decisionHandler(.allow)
        } else {
            // B9/P2-21：非本机一律不外泄到壳内。用户点击的外链交系统浏览器，其余（302/JS 跳转/表单提交）取消。
            if navigationAction.navigationType == .linkActivated {
                NSWorkspace.shared.open(url)
            }
            decisionHandler(.cancel)
        }
    }
}

// MARK: - 下载：downloadHandler 的 PDF 等经 WKDownloadDelegate 落到系统保存面板（macOS 11.3+）

@available(macOS 11.3, *)
extension AppController: WKDownloadDelegate {

    func webView(_ webView: WKWebView, navigationResponse: WKNavigationResponse,
                 didBecome download: WKDownload) {
        download.delegate = self
    }

    func webView(_ webView: WKWebView, navigationAction: WKNavigationAction,
                 didBecome download: WKDownload) {
        download.delegate = self
    }

    func download(_ download: WKDownload, decideDestinationUsing response: URLResponse,
                  suggestedFilename: String,
                  completionHandler: @escaping @MainActor @Sendable (URL?) -> Void) {
        let panel = NSSavePanel()
        panel.canCreateDirectories = true
        panel.nameFieldStringValue = suggestedFilename.isEmpty ? "rclade_plot.pdf" : suggestedFilename
        if (suggestedFilename as NSString).pathExtension.lowercased() == "pdf" {
            panel.allowedContentTypes = [.pdf]
        }
        panel.begin { resp in
            if resp == .OK, let url = panel.url {
                completionHandler(url)
            } else {
                completionHandler(nil)   // 用户取消 = 放弃下载
            }
        }
    }

    func downloadDidFinish(_ download: WKDownload) {
        // 保存面板已给用户确认，成功后不再打扰
    }

    func download(_ download: WKDownload, didFailWithError error: Error, resumeData: Data?) {
        guard !shuttingDown else { return }
        let a = NSAlert()
        a.alertStyle = .warning
        a.messageText = L("download_fail")
        a.informativeText = error.localizedDescription
        a.runModal()
    }
}

// MARK: - 入口

let app = NSApplication.shared
app.setActivationPolicy(.regular)
guard SingleInstance.acquire() else { exit(0) }   // 已有实例：已通知其置前
let controller = AppController()
app.delegate = controller
app.run()
