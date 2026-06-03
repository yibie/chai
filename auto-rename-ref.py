#!/usr/bin/env python3
"""
auto-rename-ref.py — 批量重命名 ~/Documents/ref 中的 .org 文件

格式: ID__Author__Title==Keywords--Status-Rating.org

规则:
1. 已有 ID__ 前缀的 → 跳过（已管理）
2. 文件内有 #+TITLE / #+AUTHOR 元数据 → 读取使用
3. 文件名含 __ 分隔符 → 拆分为 Author__Title
4. 均无 → 整个文件名作为 Title，Author 为空
5. ID 使用文件修改时间（保留原始保存日期）
6. 保留已有的 ==Keywords 和 --State-Rating

用法:
  python3 auto-rename-ref.py                    # 实际重命名
  python3 auto-rename-ref.py --dry-run          # 仅预览
  python3 auto-rename-ref.py --dir ~/other/ref  # 指定目录
"""

import os
import re
import sys
import argparse
import hashlib
from datetime import datetime, timezone
from collections import defaultdict


# ── 配置 ────────────────────────────────────────────────────────

# 已管理文件名正则: ID__...
MANAGED_RE = re.compile(r'^\d{8}T\d{6}__')

# 状态后缀: --status-rating
STATE_SUFFIX_RE = re.compile(r'--([a-z]+)(?:-([0-5]))?$')

# 关键词后缀: ==kw1_kw2
KEYWORD_SUFFIX_RE = re.compile(r'==([^=]+)$')

# Org 元数据
TITLE_RE = re.compile(r'^#\+TITLE:\s*(.*)$', re.M | re.I)
AUTHOR_RE = re.compile(r'^#\+AUTHOR:\s*(.*)$', re.M | re.I)
FILETAGS_RE = re.compile(r'^#\+FILETAGS:\s*(.*)$', re.M | re.I)

# 非法字符（在文件名中需要替换）
ILLEGAL_RE = re.compile(r'[/\\:*?"<>|]+')


# ── 工具函数 ────────────────────────────────────────────────────

def sanitize(s, fallback=""):
    """清理字符串，使其安全用于文件名。"""
    if not s:
        return fallback
    s = s.strip()
    # 替换空格
    s = re.sub(r'\s+', '-', s)
    # 替换非法字符
    s = ILLEGAL_RE.sub('-', s)
    # 替换我们的分隔符
    s = s.replace('__', '-').replace('==', '-')
    # 合并连续的 -
    s = re.sub(r'-+', '-', s)
    # 去掉首尾的 -
    s = s.strip('-')
    return s if s else fallback


def read_metadata(filepath):
    """读取 .org 文件的前 4096 字节，提取元数据。返回 dict。"""
    meta = {'title': None, 'author': None, 'keywords': [], 'id': None}
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read(4096)
    except (OSError, UnicodeDecodeError):
        return meta

    # TITLE
    m = TITLE_RE.search(content)
    if m:
        meta['title'] = m.group(1).strip()

    # AUTHOR
    m = AUTHOR_RE.search(content)
    if m:
        meta['author'] = m.group(1).strip()

    # FILETAGS (用 : 或空格分隔)
    m = FILETAGS_RE.search(content)
    if m:
        tags_str = m.group(1).strip()
        # 尝试 : 分隔，失败则用空格
        if ':' in tags_str:
            meta['keywords'] = [t.strip() for t in tags_str.split(':') if t.strip()]
        else:
            meta['keywords'] = tags_str.split()

    return meta


def extract_state_and_keywords(stem):
    """从文件名 stem（不含扩展名）中分离出状态和关键词。
    返回 (core, keywords_str, state_str)
    例如 'Foo--reading-4==tag1_tag2' → ('Foo', 'tag1_tag2', 'reading-4')
    """
    state_str = None
    kw_str = None
    core = stem

    # 先提取 --state (必须在 ==keywords 之前)
    m = STATE_SUFFIX_RE.search(core)
    if m:
        state_str = m.group(0)[2:]  # 去掉 --
        core = core[:m.start()]

    # 再提取 ==keywords
    m = KEYWORD_SUFFIX_RE.search(core)
    if m:
        kw_str = m.group(1)  # 去掉 ==
        core = core[:m.start()]

    return core, kw_str, state_str


def parse_filename_for_author_title(stem):
    """从文件名 stem 尝试解析 Author 和 Title。
    如果含 __ 分隔符，第一段为 Author，其余为 Title。
    否则整个为 Title，Author 为空。
    """
    # 先剥离状态和关键词
    core, kw_str, state_str = extract_state_and_keywords(stem)

    core = core.strip()

    # 检查是否有 __ 分隔符
    if '__' in core:
        parts = core.split('__')
        author = parts[0].strip()
        title = '__'.join(parts[1:]).strip()
    else:
        author = ""
        title = core.strip()

    return author, title, kw_str, state_str


# macOS 文件名最大字节数（UTF-8）
MAX_FILENAME_BYTES = 255


def _utf8_len(s):
    """返回字符串的 UTF-8 字节长度。"""
    return len(s.encode('utf-8'))


def _truncate_to_bytes(s, max_bytes):
    """截断字符串使其 UTF-8 字节长度不超过 max_bytes。"""
    encoded = s.encode('utf-8')
    if len(encoded) <= max_bytes:
        return s
    # 逐字符截断，保证不破坏多字节字符
    result = []
    current_len = 0
    for ch in s:
        ch_len = len(ch.encode('utf-8'))
        if current_len + ch_len > max_bytes:
            break
        result.append(ch)
        current_len += ch_len
    return ''.join(result)


def generate_filename(author, title, keywords_str, state_str, mtime):
    """根据各组件生成标准文件名。超过长度限制时自动截断。"""
    # ID 使用文件 mtime
    id_str = datetime.fromtimestamp(mtime, tz=timezone.utc).strftime('%Y%m%dT%H%M%S')

    safe_author = sanitize(author, "")
    safe_title = sanitize(title, "Untitled")

    # 构建后缀（keywords + state）
    suffix = ""
    if keywords_str:
        safe_kw = sanitize(keywords_str, "")
        if safe_kw:
            safe_kw = safe_kw.replace(' ', '_')
            suffix += f"=={safe_kw}"
    if state_str:
        suffix += f"--{state_str}"

    ext = ".org"

    # 构建文件名
    if safe_author:
        base = f"{id_str}__{safe_author}__{safe_title}"
    else:
        base = f"{id_str}__{safe_title}"

    full = base + suffix + ext

    # 如果超长，优先截断 title，再截断 author
    if _utf8_len(full) > MAX_FILENAME_BYTES:
        # 计算固定部分的字节长度
        fixed = f"{id_str}__"
        author_sep = "__" if safe_author else ""
        fixed += author_sep
        fixed += suffix + ext
        fixed_bytes = _utf8_len(fixed)
        remaining = MAX_FILENAME_BYTES - fixed_bytes

        if safe_author:
            # 给 author 和 title 各分配一部分空间
            author_budget = min(_utf8_len(safe_author), remaining // 3)
            title_budget = remaining - author_budget - _utf8_len("__")
            if title_budget < 20:
                title_budget = max(20, remaining // 2)
                author_budget = remaining - title_budget - _utf8_len("__")

            safe_author = _truncate_to_bytes(safe_author, max(author_budget, 0))
            safe_title = _truncate_to_bytes(safe_title, max(title_budget, 0))
            base = f"{id_str}__{safe_author}__{safe_title}"
        else:
            safe_title = _truncate_to_bytes(safe_title, max(remaining, 0))
            base = f"{id_str}__{safe_title}"

        full = base + suffix + ext

    return full


def generate_unique_path(dirpath, filename):
    """如果文件已存在，添加 (2), (3) 等后缀以保证唯一。"""
    base, ext = os.path.splitext(filename)
    candidate = filename
    counter = 2
    while os.path.exists(os.path.join(dirpath, candidate)):
        candidate = f"{base} ({counter}){ext}"
        counter += 1
    return os.path.join(dirpath, candidate)


# ── 主逻辑 ────────────────────────────────────────────────────

def scan_and_rename(directory, dry_run=True):
    """扫描目录并重命名。"""
    if not os.path.isdir(directory):
        print(f"错误: 目录不存在: {directory}")
        return 1

    # 收集所有 .org 文件
    all_files = sorted([
        f for f in os.listdir(directory)
        if f.endswith('.org') and not f.endswith('.org~') and os.path.isfile(os.path.join(directory, f))
    ])

    stats = {
        'total': len(all_files),
        'managed': 0,       # 已管理，跳过
        'renamed': 0,       # 成功重命名
        'skipped_no_change': 0,  # 重命名后名字没变
        'skipped_no_meta': 0,    # 无法提取任何信息
        'errors': 0,
        'collisions': 0,
    }

    # 用于 ID 碰撞检测: timestamp_str -> count
    id_counter = defaultdict(int)

    for filename in all_files:
        filepath = os.path.join(directory, filename)
        stem = filename[:-4]  # 去掉 .org

        # 1. 已管理 → 跳过
        if MANAGED_RE.match(stem):
            stats['managed'] += 1
            continue

        # 2. 获取文件 mtime
        try:
            mtime = os.path.getmtime(filepath)
        except OSError:
            stats['errors'] += 1
            print(f"  ❌ {filename} — 无法读取文件属性")
            continue

        # 3. 读取元数据
        meta = read_metadata(filepath)

        # 4. 从文件名解析（提取已有的 keywords 和 state）
        fn_author, fn_title, fn_kw_str, fn_state_str = parse_filename_for_author_title(stem)

        # 5. 决定最终的 author, title, keywords, state
        # 优先使用元数据，否则使用文件名解析结果
        author = meta.get('author') or fn_author or ""
        title = meta.get('title') or fn_title or ""

        # keywords: 优先文件名中的，再考虑元数据中的
        if fn_kw_str:
            keywords_str = fn_kw_str
        elif meta.get('keywords'):
            keywords_str = '_'.join(meta['keywords'])
        else:
            keywords_str = None

        state_str = fn_state_str

        # 如果既没有 author 也没有 title，跳过
        if not title.strip():
            stats['skipped_no_meta'] += 1
            if not dry_run:
                print(f"  ⚠️  {filename} — 无 Title，跳过")
            continue

        # 6. ID 碰撞处理：如果同一秒有多个文件，递增
        ts = datetime.fromtimestamp(mtime, tz=timezone.utc).strftime('%Y%m%dT%H%M%S')
        id_counter[ts] += 1
        if id_counter[ts] > 1:
            # 用文件内容的哈希做微调（加 1 秒）
            adjusted_mtime = mtime + (id_counter[ts] - 1)
            new_filename = generate_filename(author, title, keywords_str, state_str, adjusted_mtime)
            stats['collisions'] += 1
        else:
            new_filename = generate_filename(author, title, keywords_str, state_str, mtime)

        # 7. 检查是否需要重命名
        if new_filename == filename:
            stats['skipped_no_change'] += 1
            continue

        # 8. 执行
        if dry_run:
            print(f"  📝 {filename}")
            print(f"      → {new_filename}")
            stats['renamed'] += 1
        else:
            new_path = generate_unique_path(directory, new_filename)
            final_name = os.path.basename(new_path)
            try:
                os.rename(filepath, new_path)
                if final_name != new_filename:
                    print(f"  ✅ {filename}")
                    print(f"      → {final_name} (碰撞重命名)")
                else:
                    print(f"  ✅ {filename}")
                    print(f"      → {new_filename}")
                stats['renamed'] += 1
            except OSError as e:
                stats['errors'] += 1
                print(f"  ❌ {filename} — {e}")

    # 9. 报告
    print()
    print("=" * 60)
    print("统计报告")
    print("=" * 60)
    print(f"  总 .org 文件:     {stats['total']:>5}")
    print(f"  已管理（跳过）:   {stats['managed']:>5}")
    print(f"  {'【预览】' if dry_run else '实际'}重命名:     {stats['renamed']:>5}")
    print(f"  无需重命名:       {stats['skipped_no_change']:>5}")
    print(f"  无元数据（跳过）: {stats['skipped_no_meta']:>5}")
    print(f"  错误:             {stats['errors']:>5}")
    if stats['collisions']:
        print(f"  ID 碰撞处理:      {stats['collisions']:>5}")

    if dry_run:
        print()
        print("💡 这是预览模式。确认无误后运行:")
        print("   python3 auto-rename-ref.py")
        print("   或: python3 auto-rename-ref.py --dir <目标目录>")

    return 0 if stats['errors'] == 0 else 1


# ── 入口 ────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description='批量将 ~/Documents/ref 中的 .org 文件重命名为 chai 管理格式'
    )
    parser.add_argument(
        '--dry-run', action='store_true', default=False,
        help='仅预览，不实际重命名'
    )
    parser.add_argument(
        '--dir', type=str,
        default=os.path.expanduser('~/Documents/ref'),
        help='目标目录（默认: ~/Documents/ref）'
    )
    args = parser.parse_args()

    directory = os.path.expanduser(args.dir)

    print(f"目录: {directory}")
    print(f"模式: {'🔍 预览 (dry-run)' if args.dry_run else '✏️  实际重命名'}")
    print()

    return scan_and_rename(directory, dry_run=args.dry_run)


if __name__ == '__main__':
    sys.exit(main())
