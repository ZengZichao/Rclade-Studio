import Foundation
import AppKit
import Darwin

/// 单实例锁：第二次启动时通知已运行的实例把窗口置前，然后自己退出。
/// 锁文件（含 PID）落在 ~/Library/Application Support/RcladeStudio/；
/// 崩溃遗留的过期锁通过 PID 存活检测自动清理。
enum SingleInstance {

    static let activateNotification = Notification.Name("io.github.zengzichao.rclade-studio.activate")

    private static let dir = NSHomeDirectory() + "/Library/Application Support/RcladeStudio"
    private static var lockPath: String { dir + "/instance.lock" }

    /// 返回 true = 本实例持有锁，可继续启动；false = 已有实例在跑（已通知其置前），调用方应退出。
    static func acquire() -> Bool {
        let fm = FileManager.default
        try? fm.createDirectory(atPath: dir, withIntermediateDirectories: true)

        if let data = fm.contents(atPath: lockPath),
           let pid = Int(String(data: data, encoding: .utf8)?
               .trimmingCharacters(in: .whitespacesAndNewlines) ?? "") {
            // 以 kill 返回值为准；errno 可能残留同线程上一次系统调用的旧值，不可单独依赖
            let rv = kill(Int32(pid), 0)
            if rv == 0 || (rv == -1 && errno == EPERM) {
                // B8：优先用 NSRunningApplication 直接激活已运行实例（不依赖第一实例是否
                // 已注册分布式通知观察者，规避启动早期/快速连点的早发竞态）；通知作为补充。
                NSRunningApplication(processIdentifier: Int32(pid))?.activate(
                    options: [.activateIgnoringOtherApps])
                DistributedNotificationCenter.default().post(name: activateNotification, object: nil)
                return false
            }
            try? fm.removeItem(atPath: lockPath)   // 过期锁：属主进程已不存在
        }
        // 创建失败不拦启动——锁是体验优化，不是硬约束
        _ = fm.createFile(atPath: lockPath, contents: Data("\(ProcessInfo.processInfo.processIdentifier)".utf8))
        return true
    }

    static func release() {
        try? FileManager.default.removeItem(atPath: lockPath)
    }
}
