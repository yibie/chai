#!/bin/bash
# /push — 推送到 yibie/chai
set -e
cd "$(dirname "$0")"
echo "🚀 push to yibie/chai..."
git push origin main "$@"
echo "✅ done"
