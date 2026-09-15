import Foundation
import Darwin

/// 管理 R 引擎子进程的生命周期：起 `Rscript launch_engine.R <port> <handshake>`，
/// 收集 stderr 供错误展示；退出回收（SIGTERM → 最多等 3 秒 → SIGKILL 兜底），
/// 确保不留僵尸进程、不占端口；非用户操作下的引擎退出回调给 UI 弹"重启/退出"引导。
final class EngineProcess {

    private var process: Process?
    private var userInitiatedStop = false

    /// P1-8/B7：log 由后台 readabilityHandler 写、主线程读，需串行队列保护；
    /// 并限制只保留末尾若干字节，避免长期运行无界增长。
    private let logQueue = DispatchQueue(label: "io.github.zengzichao.rclade-studio.engine.log")
    private var _log = ""
    private let logCap = 64 * 1024   // 64KB

    /// 线程安全的日志快照（主线程读取用于错误弹窗）。
    var log: String { logQueue.sync { _log } }

    private func appendLog(_ s: String) {
        logQueue.async {
            self._log += s
            if self._log.count > self.logCap {
                self._log = String(self._log.suffix(self.logCap))
            }
        }
    }
    private func resetLog() { logQueue.sync { _log = "" } }

    /// 进程是否已（异常）退出。start 成功前返回 false。
    var hasExited: Bool {
        guard let p = process else { return false }
        return !p.isRunning
    }

    /// 引擎在非用户操作下自行退出时回调（主线程），如缺依赖、崩溃、端口被占。
    var onUnexpectedExit: ((Int32) -> Void)?

    @discardableResult
    func start(rscript: String, port: UInt16, handshake: String, dropfile: String? = nil,
               lang: String = "zh") -> Bool {
        guard let script = Self.engineScriptURL() else {
            appendLog("包内未找到 launch_engine.R（构建产物缺失）。")
            return false
        }
        userInitiatedStop = false
        resetLog()

        let p = Process()
        p.executableURL = URL(fileURLWithPath: rscript)
        p.arguments = [script.path, String(port), handshake]
        p.arguments?.append(dropfile ?? "")   // 第 4 位：拖入文件路径（空串=无）
        p.arguments?.append(lang)             // 第 5 位：界面语言 zh / en
        p.currentDirectoryURL = FileManager.default.temporaryDirectory

        let pipe = Pipe()
        p.standardError = pipe
        p.standardOutput = FileHandle.nullDevice
        pipe.fileHandleForReading.readabilityHandler = { [weak self] h in
            let d = h.availableData
            if !d.isEmpty { self?.appendLog(String(data: d, encoding: .utf8) ?? "") }
        }
        p.terminationHandler = { [weak self] proc in
            pipe.fileHandleForReading.readabilityHandler = nil
            guard let self = self, !self.userInitiatedStop else { return }
            DispatchQueue.main.async { self.onUnexpectedExit?(proc.terminationStatus) }
        }

        do {
            try p.run()
            process = p
            return true
        } catch {
            appendLog(error.localizedDescription)
            return false
        }
    }

    /// 定位包内 launch_engine.R。Bundle API 的查找对扩展名大小写/注册时机不够可靠，
    /// 依次回退到 Resources 固定路径与可执行文件相对路径。
    private static func engineScriptURL() -> URL? {
        let fm = FileManager.default
        if let u = Bundle.main.url(forResource: "launch_engine", withExtension: "R") { return u }
        if let res = Bundle.main.resourceURL {
            let p = res.appendingPathComponent("launch_engine.R")
            if fm.fileExists(atPath: p.path) { return p }
        }
        if let exe = Bundle.main.executableURL {
            let p = exe.deletingLastPathComponent()  // Contents/MacOS
                .appendingPathComponent("Resources/launch_engine.R")
            if fm.fileExists(atPath: p.path) { return p }
        }
        return nil
    }

    /// 停引擎：SIGTERM 起步，3 秒内未退则 SIGKILL。在主线程调用时会短暂阻塞 UI，
    /// 仅发生在关窗/退出路径，可接受。
    func stop() {
        guard let p = process else { return }
        userInitiatedStop = true
        if p.isRunning {
            let done = DispatchSemaphore(value: 0)
            p.terminationHandler = { _ in done.signal() }
            // 不用 p.terminate()：isRunning 检查与调用之间进程若恰好退出会抛 ObjC 异常
            kill(p.processIdentifier, SIGTERM)               // SIGTERM
            if done.wait(timeout: .now() + 3) == .timedOut {
                kill(p.processIdentifier, SIGKILL)
                _ = done.wait(timeout: .now() + 2)
            }
            p.terminationHandler = nil
        } else {
            p.terminationHandler = nil
        }
        process = nil
    }
}
