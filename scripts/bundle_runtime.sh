#!/usr/bin/env bash
# 把一套 R 运行时打进 .app，让 RcladeStudio.app 不依赖用户机器上的 R。
#
# 关键约束（决定了下面为什么要改二进制）：
#   macOS 的 R 把 R_HOME **编译进可执行文件**，并拒绝 R_HOME 环境变量
#   （"WARNING: ignoring environment value of R_HOME"）。所以必须把那条绝对路径
#   原地改写成等长的"间接路径"，再由 app 启动时把该路径 symlink 到包内
#   Resources/R。间接路径写在 Resources/R/.rclade-link 供 app 读取。
#
# 支持的源布局：
#   prefix（conda / micromamba）：<prefix>/lib/R + <prefix>/lib/*.dylib + <prefix>/bin/Rscript
#   home  （Homebrew）          ：<R_HOME>/bin/Rscript + <R_HOME>/lib/*.dylib
#
# 用法：
#   ./scripts/bundle_runtime.sh
#   RCLADE_R_HOME=/path/to/lib/R ./scripts/bundle_runtime.sh
#   RCLADE_BUNDLE_HOME=/Users/Shared/rc/rt/R ./scripts/bundle_runtime.sh
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT="$ROOT/Resources/R"
BUILD="$ROOT/Resources/.R-build"

# ---------------------------------------------------------------- 0) 选源
if [ -z "${RCLADE_R_HOME:-}" ]; then
  CAND=""
  for d in "$HOME"/.local/share/mamba/envs/*/lib/R "$HOME"/micromamba/envs/*/lib/R \
           /opt/homebrew/Cellar/r/*/lib/R /usr/local/Cellar/r/*/lib/R; do
    [ -x "$d/bin/Rscript" ] || continue
    if "$d/bin/Rscript" -e 'q(status=!requireNamespace("Rclade",quietly=TRUE))' >/dev/null 2>&1; then
      CAND="$d"; break
    fi
  done
  [ -n "$CAND" ] || { echo "✗ 没找到装了 Rclade 的 R，请用 RCLADE_R_HOME 指定 R.home()"; exit 1; }
  SRC_HOME="$CAND"
else
  SRC_HOME="$RCLADE_R_HOME"
fi
SRC_HOME="$(cd "$SRC_HOME" && pwd)"

PREFIX=""
if [ -x "$(dirname "$SRC_HOME")/../bin/Rscript" ] && [ "$(basename "$(dirname "$SRC_HOME")")" = "lib" ]; then
  PREFIX="$(cd "$(dirname "$SRC_HOME")/.." && pwd)"
fi
SRC_LIB_DIR="$([ -n "$PREFIX" ] && echo "$PREFIX/lib" || echo "$SRC_HOME/lib")"

OLD_LEN=${#SRC_HOME}
LINK_HOME="${RCLADE_BUNDLE_HOME:-/Users/Shared/rclade/rt/R}"
if [ "${#LINK_HOME}" -gt "$OLD_LEN" ]; then
  echo "✗ 间接路径（${#LINK_HOME}）比原 R_HOME（$OLD_LEN）长，无法等长改写。"
  echo "  换更短的 RCLADE_BUNDLE_HOME，例如 /Users/Shared/rc/R"
  exit 1
fi
PAD=$((OLD_LEN - ${#LINK_HOME}))
PADDED="$LINK_HOME"
for ((i = 0; i < PAD; i++)); do PADDED="$PADDED/"; done

echo "› 源 R_HOME : $SRC_HOME"
echo "› 源布局   : $([ -n "$PREFIX" ] && echo "prefix ($PREFIX)" || echo "home")"
echo "› 间接路径 : $PADDED"

# ---------------------------------------------------------------- 1) 铺结构
# 先构建到 Resources/.R-build，成功后再整体换位，避免中途失败把旧运行时弄坏
echo "› 拷贝运行时…"
if [ -d "$BUILD" ]; then mv "$BUILD" "/tmp/rclade-build-trash-$$" || true; fi
mkdir -p "$BUILD/lib" "$BUILD/bin"
OUT_SAVED="$OUT"; OUT="$BUILD"
if [ -n "$PREFIX" ]; then
  rsync -a "$PREFIX/lib/R/" "$OUT/lib/R/"
  for b in Rscript R Rcmd; do [ -f "$PREFIX/bin/$b" ] && cp "$PREFIX/bin/$b" "$OUT/bin/"; done
else
  rsync -a "$SRC_HOME/" "$OUT/"
fi

# ---------------------------------------------------------------- 2) 改写内嵌 R_HOME
TOUCHED="$ROOT/.bundle-touched"; : > "$TOUCHED"
echo "› 改写内嵌 R_HOME（等长）…"
NB=0; NT=0
while IFS= read -r f; do
  [ -f "$f" ] || continue
  if file -b "$f" | grep -q 'Mach-O'; then
    OLD="$SRC_HOME" NEW="$PADDED" perl -0pi -e 's/\Q$ENV{OLD}\E/$ENV{NEW}/g' "$f"
    echo "$f" >> "$TOUCHED"; NB=$((NB + 1))
  else
    sed -i '' "s|$SRC_HOME|$PADDED|g" "$f"; NT=$((NT + 1))
  fi
done < <(grep -rlaF "$SRC_HOME" "$OUT" 2>/dev/null || true)
echo "  ${NB} 个二进制、${NT} 个脚本"

# ---------------------------------------------------------------- 3) 裁剪 R 包
# 用【源】R 来算依赖闭包（此时包内 R 还不能独立启动），只对拷贝出来的 library 动手
echo "› 裁剪 R 包到 Rclade + shiny 的依赖闭包…"
BEFORE=$(du -sm "$OUT/lib/R/library" | awk '{print $1}')
"$SRC_HOME/bin/Rscript" --vanilla --slave -e '
  lib <- commandArgs(TRUE)[1]
  db  <- installed.packages(lib.loc = lib)
  base <- rownames(db)[!is.na(db[, "Priority"]) & db[, "Priority"] %in% c("base", "recommended")]
  dep <- tryCatch(unlist(tools::package_dependencies(
           c("Rclade", "shiny"), db = db,
           which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)),
         error = function(e) character(0))
  keep <- unique(c(base, dep, "Rclade", "shiny"))
  keep <- keep[keep %in% rownames(db)]
  drop <- setdiff(rownames(db), keep)
  unlink(file.path(lib, drop), recursive = TRUE)
  cat(sprintf("  保留 %d 个包，删除 %d 个\n", length(keep), length(drop)))' \
  "$OUT/lib/R/library"
echo "  包库：${BEFORE}MB → $(du -sm "$OUT/lib/R/library" | awk '{print $1}')MB"

# ---------------------------------------------------------------- 4) 只拷被引用的动态库
# 不做"全拷再删"，直接按引用闭包增量拷贝：既省体积，也不会误删。
echo "› 收集动态库闭包…"
# MAP_SRC 是"源环境里与 $OUT 对应的根"：prefix 布局对应 env 根，home 布局对应 R_HOME。
# 任何位于 MAP_SRC 下的引用都只改写成 @loader_path 相对路径，不保留绝对路径。
MAP_SRC="$([ -n "$PREFIX" ] && echo "$PREFIX" || echo "$SRC_HOME")"
SEEN="$ROOT/.bundle-seen"; : > "$SEEN"
COPIED=0

deps_of() { otool -L "$1" 2>/dev/null | tail -n +2 | sed -E 's/^[[:space:]]+//' | awk '{print $1}'; }

# 引用一律写成「间接路径 + 相对 MAP_SRC 的后缀」。
# 不在这里算 @loader_path 相对路径：包内位置随 .app 移动，而间接路径永远由 app 重建。
walk() {
  local f="$1" dep srcf dst ref name
  while IFS= read -r dep; do
    [ -z "$dep" ] && continue
    case "$dep" in
      /usr/lib/*|/System/*) continue ;;
      @rpath/R/library/*) continue ;;          # 包间互相引用的 .so 已在包里
      "$MAP_SRC"/*) srcf="$dep" ;;
      @rpath/*|@loader_path/*)
        name="$(basename "$dep")"; srcf=""
        for c in "$MAP_SRC/lib/$name" "$MAP_SRC/lib/R/lib/$name"; do
          [ -f "$c" ] && { srcf="$c"; break; }
        done
        ;;
      *) continue ;;
    esac
    [ -n "$srcf" ] && [ -f "$srcf" ] || { echo "  ! 源里找不到：$dep" >&2; continue; }

    dst="$OUT${srcf#"$MAP_SRC"}"
    ref="$PADDED${srcf#"$MAP_SRC"}"
    if [ ! -f "$dst" ]; then
      mkdir -p "$(dirname "$dst")"; cp -L "$srcf" "$dst"; COPIED=$((COPIED + 1))
    fi
    if ! grep -qxF "$dst" "$SEEN" 2>/dev/null; then
      echo "$dst" >> "$SEEN"; walk "$dst"
    fi
    if install_name_tool -change "$dep" "$ref" "$f" 2>/dev/null; then echo "$f" >> "$TOUCHED"; fi
  done < <(deps_of "$f")
}

for f in "$OUT/lib/R/lib/libR.dylib" "$OUT/lib/R/bin/exec/R" "$OUT/bin/Rscript" "$OUT/bin/R"; do
  [ -f "$f" ] || continue
  echo "$f" >> "$SEEN"; walk "$f"
done
while IFS= read -r so; do echo "$so" >> "$SEEN"; walk "$so"; done \
  < <(find "$OUT/lib/R/library" -name '*.so' 2>/dev/null)
echo "  额外拷入 ${COPIED} 个动态库"

# ---------------------------------------------------------------- 5) 重签名
echo "› 重签名 $(wc -l < "$TOUCHED" | tr -d ' ') 个被改动的二进制…"
while IFS= read -r b; do codesign --force --sign - "$b" 2>/dev/null || true; done < "$TOUCHED"
rm -f "$TOUCHED" "$SEEN"

# ---------------------------------------------------------------- 5.5) 换位
OUT="$OUT_SAVED"
if [ -d "$OUT" ]; then
  OLDTRASH="/tmp/rclade-runtime-old-$$"
  mv "$OUT" "$OLDTRASH" || true
  rm -rf "$OLDTRASH" 2>/dev/null || true
fi
mv "$BUILD" "$OUT"

# ---------------------------------------------------------------- 6) 记录间接路径
if [ -n "$PREFIX" ]; then LINK_TARGET="lib/R"; else LINK_TARGET="."; fi
printf '%s\n%s\n' "$LINK_HOME" "$LINK_TARGET" > "$OUT/.rclade-link"

# ---------------------------------------------------------------- 7) 自检
echo "› 自检：建立间接路径并启动包内 R"
mkdir -p "$(dirname "$LINK_HOME")"
rm -f "$LINK_HOME"
ln -s "$OUT/$LINK_TARGET" "$LINK_HOME"
if ! env -i HOME="$HOME" PATH=/usr/bin:/bin \
      "$OUT/bin/Rscript" --vanilla -e 'cat("   R", as.character(getRversion()),
        "| R_HOME =", R.home(), "\n   Rclade:", requireNamespace("Rclade", quietly = TRUE),
        "| ggtree:", requireNamespace("ggtree", quietly = TRUE),
        "| shiny:", requireNamespace("shiny", quietly = TRUE), "\n")'; then
  echo "✗ 包内 R 启动失败"
  exit 1
fi

echo ""
echo "完成 ✅  $OUT  ($(du -sh "$OUT" | awk '{print $1}'), $(ls "$OUT/lib/R/library" | wc -l | tr -d ' ') 个包)"
echo "间接路径： $LINK_HOME  →  $OUT/$LINK_TARGET"
echo "下一步： ./build_app.sh"
