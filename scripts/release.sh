#!/usr/bin/env bash
# 打包 Release：
#   dist/RcladeStudio-<版本>-<架构>.zip        完整版（内置 R 运行时，双击即用，约 200MB）
#   dist/RcladeStudio-<版本>-<架构>-slim.zip   精简版（不含运行时，需本机 R + Rclade，约 5MB）
set -euo pipefail
cd "$(dirname "$0")/.."

VER="$(/usr/libexec/PlistBuddy -c 'Print :CFBundleShortVersionString' Info.plist)"
ARCH="$(uname -m)"
OUT="dist/RcladeStudio-${VER}-${ARCH}.zip"
OUT_SLIM="dist/RcladeStudio-${VER}-${ARCH}-slim.zip"
STAGE="${TMPDIR:-/tmp}/rclade-release-$$"   # 放临时目录：不在 dist/ 留几百 MB 副本

./build_app.sh

rm -rf "$STAGE"; mkdir -p "$STAGE"
ditto "dist/RcladeStudio.app" "$STAGE/RcladeStudio.app"
cp "使用说明.md" "$STAGE/使用说明.md"
rm -f "$OUT"
ditto -c -k "$STAGE" "$OUT"
rm -rf "$STAGE"

# 精简版：从同一个 .app 里剔除内置运行时（rsync --exclude，不做删除）
if [ -d Resources/R ]; then
  rm -rf "$STAGE-slim"; mkdir -p "$STAGE-slim"
  rsync -a --exclude 'Contents/Resources/R' "dist/RcladeStudio.app/" "$STAGE-slim/RcladeStudio.app/"
  cp "使用说明.md" "$STAGE-slim/使用说明.md"
  cat > "$STAGE-slim/此版本需要自备 R.txt" <<'NOTE'
精简版不含内置 R 运行时。

运行前提：
  1. 本机已安装 R（>= 4.1）
  2. 该 R 里已安装 Rclade 包：install.packages("Rclade")

若不想装 R，请改用完整版 RcladeStudio-<版本>-arm64.zip（体积约 200MB，双击即用）。
NOTE
  rm -f "$OUT_SLIM"
  ditto -c -k "$STAGE-slim" "$OUT_SLIM"
  rm -rf "$STAGE-slim"
fi

echo "完成 ✅"
echo "  $OUT"
ls -lh "$OUT" | awk '{print "    " $5}'
if [ -f "$OUT_SLIM" ]; then
  echo "  $OUT_SLIM"
  ls -lh "$OUT_SLIM" | awk '{print "    " $5}'
fi
unzip -l "$OUT" | tail -n +4 | head -5
