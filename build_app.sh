#!/usr/bin/env bash
# 构建 RcladeStudio.app（非破坏式：不 rm，旧的移到 .old-<ts>）
set -euo pipefail
cd "$(dirname "$0")"

APP="RcladeStudio.app"
BIN="RcladeStudio"
DIST="dist"
ARCH="$(uname -m)"                         # arm64 | x86_64
MACOS_MIN="11.0"
SDK="$(xcrun --sdk macosx --show-sdk-path)"

mkdir -p "$DIST"
if [ -d "$DIST/$APP" ]; then
  mv "$DIST/$APP" "$DIST/.${APP}.old-$(date +%s)"
fi

APPDIR="$DIST/$APP"
mkdir -p "$APPDIR/Contents/MacOS" "$APPDIR/Contents/Resources"

echo "› 编译 (${ARCH})"
swiftc -O -sdk "$SDK" -target "${ARCH}-apple-macosx${MACOS_MIN}" \
  Sources/RcladeStudio/*.swift \
  -o "$APPDIR/Contents/MacOS/$BIN"

echo "› 组装 bundle"
cp Info.plist                "$APPDIR/Contents/Info.plist"
cp Resources/launch_engine.R "$APPDIR/Contents/Resources/launch_engine.R"
cp Resources/engine_app.R    "$APPDIR/Contents/Resources/engine_app.R"
printf 'APPL????' > "$APPDIR/Contents/PkgInfo"

# 图标：如放了 Resources/AppIcon.icns 就一并带上（缺失不致命）
if [ -f Resources/AppIcon.icns ]; then
  cp Resources/AppIcon.icns "$APPDIR/Contents/Resources/AppIcon.icns"
fi

echo "› ad-hoc 签名（本地运行用；对外发布再上 Developer ID + notarize）"
codesign --force --deep --sign - "$APPDIR" 2>/dev/null || \
  echo "  （codesign 失败可忽略，本地仍可运行；Gatekeeper 会拦，见 README 去隔离说明）"

cat <<EOF

完成 ✅
  $APPDIR

首次运行若被 Gatekeeper 拦（"已损坏/无法打开"）：
  xattr -dr com.apple.quarantine "$APPDIR"
运行前提：本机已装 R (≥4.1) 且已 install.packages("Rclade")。
EOF
