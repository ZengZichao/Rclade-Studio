import Foundation
import Darwin

/// 定位系统中已安装的 Rscript。GUI 双击启动时 $PATH 极短，绝不能依赖 `which Rscript`，
/// 因此按已知安装位置写死候选（官方框架 / Homebrew / conda·mamba·micromamba 环境）。
/// 多个命中时用 `Rscript --version` 实测版本取最新——不猜测目录名/排序含义。
enum RLocator {

    /// mamba/conda 环境根目录，覆盖 Apple Silicon 与 Intel 常见布局。
    private static let envRoots: [String] = [
        NSHomeDirectory() + "/.local/share/mamba/envs",   // mamba 默认
        NSHomeDirectory() + "/micromamba/envs",
        "/opt/micromamba/envs",
        NSHomeDirectory() + "/opt/anaconda3/envs",
        NSHomeDirectory() + "/opt/miniconda3/envs",
        NSHomeDirectory() + "/anaconda3/envs",
        NSHomeDirectory() + "/miniconda3/envs",
        "/opt/anaconda3/envs",
        "/opt/miniconda3/envs",
    ]

    /// 缓存：上次验证过装有 Rclade 的 Rscript 路径，让日常启动免去逐个探测。
    static var cacheFilePath: String {
        NSHomeDirectory() + "/Library/Application Support/RcladeStudio/rscript.cache"
    }

    static func invalidateCache() {
        try? FileManager.default.removeItem(atPath: cacheFilePath)
    }

    /// "设置"里手动指定的 Rscript：写入缓存即全局生效（下次启动优先使用）。
    static func saveCache(_ path: String) {
        try? path.write(toFile: cacheFilePath, atomically: true, encoding: .utf8)
    }

    /// 后台探测 Rscript，完成后回主线程回调（修复 P0-5/B6：同步探测最坏约 1 分钟会冻结主线程）。
    static func findRscriptAsync(completion: @escaping (String?) -> Void) {
        DispatchQueue.global(qos: .userInitiated).async {
            let path = findRscript()
            DispatchQueue.main.async { completion(path) }
        }
    }

    /// 包内自带的 R 运行时（由 scripts/bundle_runtime.sh 生成）。
    /// 存在即可让 app 脱离用户机器上的 R 独立运行。
    static var bundledRscript: String? {
        guard let res = Bundle.main.resourceURL else { return nil }
        let p = res.appendingPathComponent("R/bin/Rscript").path
        return FileManager.default.isExecutableFile(atPath: p) ? p : nil
    }

    /// 包内 R_HOME 的实际位置（prefix 布局是 R/lib/R，home 布局是 R 本身），
    /// 由打包脚本写进 Resources/R/.rclade-link 的第 2 行。
    static var bundledRuntimeHome: String? {
        guard let res = Bundle.main.resourceURL else { return nil }
        let root = res.appendingPathComponent("R")
        guard let spec = try? String(contentsOfFile: root.appendingPathComponent(".rclade-link").path,
                                     encoding: .utf8) else { return nil }
        let parts = spec.split(separator: "\n", omittingEmptySubsequences: false).map(String.init)
        guard parts.count >= 2 else { return nil }
        let rel = parts[1].trimmingCharacters(in: .whitespacesAndNewlines)
        return rel == "." ? root.path : root.appendingPathComponent(rel).path
    }

    static var bundledLibrary: String? {
        guard let home = bundledRuntimeHome else { return nil }
        let p = home + "/library"
        return FileManager.default.fileExists(atPath: p) ? p : nil
    }

    /// R 把 R_HOME 编译进了二进制，改成了一条固定的"间接路径"。
    /// 每次启动把它重新指向当前 .app 里的运行时——app 被移动或升级后也能自愈。
    @discardableResult
    static func ensureBundledRuntimeLink() -> Bool {
        let fm = FileManager.default
        guard let res = Bundle.main.resourceURL else { return false }
        let specURL = res.appendingPathComponent("R/.rclade-link")
        guard let spec = try? String(contentsOf: specURL, encoding: .utf8) else { return false }
        let parts = spec.split(separator: "\n", omittingEmptySubsequences: false).map(String.init)
        guard parts.count >= 2 else { return false }
        let link = parts[0].trimmingCharacters(in: .whitespacesAndNewlines)
        guard !link.isEmpty, let target = bundledRuntimeHome else { return false }

        if let cur = try? fm.destinationOfSymbolicLink(atPath: link),
           (cur as NSString).standardizingPath == (target as NSString).standardizingPath {
            return true
        }
        try? fm.createDirectory(atPath: (link as NSString).deletingLastPathComponent,
                                withIntermediateDirectories: true)
        try? fm.removeItem(atPath: link)
        return (try? fm.createSymbolicLink(atPath: link, withDestinationPath: target)) != nil
    }

    static func findRscript() -> String? {
        let fm = FileManager.default
        // 快路径：上次验证过带 Rclade 的 Rscript 仍可用
        if let p = try? String(contentsOfFile: cacheFilePath, encoding: .utf8)
            .trimmingCharacters(in: .whitespacesAndNewlines),
           !p.isEmpty, fm.isExecutableFile(atPath: p) {
            return p
        }

        // 包内运行时优先：只有它能保证"双击即可用"。
        // 探测失败（例如被打包机器之外的签名/路径问题）则无缝回落到系统 R。
        if let b = bundledRscript, hasRclade(b) {
            saveCache(b)
            return b
        }

        var seen = Set<String>()
        var found: [String] = []

        // realpath 去重：framework 的 "Current" 符号链与具体版本目录会指向同一文件
        func add(_ raw: String) {
            guard fm.isExecutableFile(atPath: raw),
                  let real = realpathString(raw), !seen.contains(real) else { return }
            seen.insert(real)
            found.append(real)
        }
        func scanDir(_ dir: String, _ leaf: String) {
            guard let names = try? fm.contentsOfDirectory(atPath: dir) else { return }
            for n in names.sorted() { add("\(dir)/\(n)/\(leaf)") }
        }

        scanDir("/Library/Frameworks/R.framework/Versions", "Resources/bin/Rscript")
        add("/Library/Frameworks/R.framework/Resources/bin/Rscript")
        add("/opt/homebrew/bin/Rscript")   // Apple Silicon
        add("/usr/local/bin/Rscript")      // Intel
        for root in envRoots { scanDir(root, "bin/Rscript") }
        // 旧式 conda 直装位置兜底
        add(NSHomeDirectory() + "/opt/anaconda3/bin/Rscript")
        add(NSHomeDirectory() + "/opt/miniconda3/bin/Rscript")
        add("/opt/anaconda3/bin/Rscript")
        add("/opt/miniconda3/bin/Rscript")

        guard !found.isEmpty else { return nil }

        // 按实测版本从新到旧（同版本按路径字典序），优先挑**装有 Rclade** 的那个——
        // 本机常有多个并行 R 环境，"最新的 R"未必是"装了 Rclade 的 R"。
        // 全部探测失败（≤6 个）则回退到最高版本的 R，由引擎侧给安装引导。
        let sorted = found
            .map { ($0, rscriptVersion($0) ?? [0, 0, 0]) }
            .sorted { a, b in
                if a.1 != b.1 { return lexicographicallyGreater(a.1, b.1) }
                return a.0 < b.0
            }
        var fallback: String? = sorted.first?.0
        var probed = 0
        for (path, _) in sorted where probed < 6 {
            probed += 1
            if hasRclade(path) {
                saveCache(path)
                return path
            }
            fallback = fallback ?? path
        }
        return fallback
    }

    /// 探测该 Rscript 是否装了 Rclade（requireNamespace 会拉起 ggtree 等依赖，给 10 秒上限）。
    private static func hasRclade(_ path: String) -> Bool {
        let p = Process()
        p.executableURL = URL(fileURLWithPath: path)
        p.arguments = ["-e", "cat(requireNamespace('Rclade', quietly=TRUE))"]
        let out = Pipe()
        p.standardOutput = out
        p.standardError = FileHandle.nullDevice
        do { try p.run() } catch { return false }
        let deadline = Date().addingTimeInterval(10)
        while p.isRunning && Date() < deadline { usleep(100_000) }
        if p.isRunning { p.terminate(); return false }
        guard let s = String(data: out.fileHandleForReading.readDataToEndOfFile(), encoding: .utf8)
            .map({ $0.trimmingCharacters(in: .whitespacesAndNewlines) }) else { return false }
        return s == "TRUE" && p.terminationStatus == 0
    }

    /// 校验给定路径是否为可用的 Rscript（"设置"面板用）。返回版本号（如 [4,5,3]）。
    static func rscriptVersionPublic(_ path: String) -> [Int]? {
        rscriptVersion(path)
    }

    /// `Rscript --version` 不开 R 会话，毫秒级返回，如 "Rscript (R) version 4.5.3 (…)"。
    private static func rscriptVersion(_ path: String) -> [Int]? {
        let p = Process()
        p.executableURL = URL(fileURLWithPath: path)
        p.arguments = ["--version"]
        let out = Pipe(), err = Pipe()
        p.standardOutput = out
        p.standardError = err
        do { try p.run() } catch { return nil }
        let data = out.fileHandleForReading.readDataToEndOfFile()
            + err.fileHandleForReading.readDataToEndOfFile()
        p.waitUntilExit()
        guard let s = String(data: data, encoding: .utf8),
              let r = s.range(of: "[0-9]+\\.[0-9]+\\.[0-9]+", options: .regularExpression)
        else { return nil }
        return s[r].split(separator: ".").map { Int($0) ?? 0 }
    }

    private static func lexicographicallyGreater(_ a: [Int], _ b: [Int]) -> Bool {
        for i in 0..<max(a.count, b.count) {
            let x = i < a.count ? a[i] : 0
            let y = i < b.count ? b[i] : 0
            if x != y { return x > y }
        }
        return false
    }

    private static func realpathString(_ path: String) -> String? {
        var buf = [CChar](repeating: 0, count: Int(PATH_MAX))
        guard realpath(path, &buf) != nil else { return nil }
        return String(cString: buf)
    }

    /// 分配一个空闲的 127.0.0.1 端口（bind 到 0 让内核选，随即释放交给 R 绑定）。
    /// bind→close→R bind 之间存在极小的竞态；引擎侧通过 handshake 文件回报真实 URL 兜底。
    static func findFreePort() -> UInt16? {
        let fd = socket(AF_INET, SOCK_STREAM, 0)
        guard fd >= 0 else { return nil }
        defer { close(fd) }

        var addr = sockaddr_in()
        addr.sin_family = sa_family_t(AF_INET)
        addr.sin_port = in_port_t(0)
        addr.sin_addr = in_addr(s_addr: inet_addr("127.0.0.1"))

        let bindOK = withUnsafePointer(to: &addr) {
            $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
                bind(fd, $0, socklen_t(MemoryLayout<sockaddr_in>.size)) == 0
            }
        }
        guard bindOK else { return nil }

        var bound = sockaddr_in()
        var len = socklen_t(MemoryLayout<sockaddr_in>.size)
        let ok = withUnsafeMutablePointer(to: &bound) {
            $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
                getsockname(fd, $0, &len) == 0
            }
        }
        guard ok else { return nil }
        return UInt16(bigEndian: bound.sin_port)
    }
}
