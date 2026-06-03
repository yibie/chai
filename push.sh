#!/bin/bash
# /push — 推送到 yibie/chai 的 main 分支
set -e
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"
echo "🚀 push to yibie/chai main..."
git push chai main "$@"
echo "✅ done"
