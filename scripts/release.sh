#!/usr/bin/env bash
# 打包 Release：构建 .app → 与《使用说明》一起压成 dist/RcladeStudio-<版本>-<架构>.zip
set -euo pipefail
cd "$(dirname "$0")/.."

VER="$(/usr/libexec/PlistBuddy -c 'Print :CFBundleShortVersionString' Info.plist)"
ARCH="$(uname -m)"
OUT="dist/RcladeStudio-${VER}-${ARCH}.zip"
STAGE="dist/.release-stage"

./build_app.sh

rm -rf "$STAGE"; mkdir -p "$STAGE"
ditto "dist/RcladeStudio.app" "$STAGE/RcladeStudio.app"
cp "使用说明.md" "$STAGE/使用说明.md"
rm -f "$OUT"
ditto -c -k "$STAGE" "$OUT"
rm -rf "$STAGE"

echo "完成 ✅"
echo "  $OUT"
unzip -l "$OUT" | tail -n +4 | head -6
