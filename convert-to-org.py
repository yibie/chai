import warnings
warnings.filterwarnings("ignore", message=".*tqdm.*", category=DeprecationWarning)

import os
import sys
import re
import logging
import shutil
import argparse
import subprocess
import platform
import tempfile
from pathlib import Path
from typing import Tuple, List, Optional

# 默认设置
logging.basicConfig(level=logging.WARNING, format='%(levelname)s: %(message)s', stream=sys.stdout)
logger = logging.getLogger(__name__)

def setup_environment():
    script_dir = Path(__file__).parent.absolute()
    root_dir = script_dir.parent
    venv_path = root_dir / '.venv'
    if os.name == 'nt':
        bin_dir = venv_path / 'Scripts'
        python_path = str(bin_dir / 'python.exe')
    else:
        bin_dir = venv_path / 'bin'
        python_path = str(bin_dir / 'python')

    if venv_path.exists() and Path(python_path).exists():
        os.environ['VIRTUAL_ENV'] = str(venv_path)
        os.environ['PATH'] = str(bin_dir) + os.pathsep + os.environ['PATH']
        sys.executable = python_path
    return True

setup_environment()

# 延迟加载重型库
try:
    import fitz  # PyMuPDF
except ImportError:
    fitz = None

IS_MAC = platform.system() == "Darwin"
HAS_OCR_LIBS = False
if IS_MAC:
    try:
        from Vision import VNImageRequestHandler, VNRecognizeTextRequest
        from AppKit import NSBitmapImageRep, NSImage
        from PIL import Image
        HAS_OCR_LIBS = True
    except ImportError:
        HAS_OCR_LIBS = False

def check_dependencies():
    if not shutil.which('pandoc'):
        print("Error: 'pandoc' is not installed.", file=sys.stderr)
        sys.exit(1)

def get_safe_filename(filename: str) -> str:
    filename = re.sub(r'[^\w\s\-\.\(\)\（\）\[\]【】\u4e00-\u9fff]', '_', filename)
    filename = re.sub(r'[\s_]+', '_', filename)
    filename = filename.strip('_').strip()
    return filename or 'unnamed_file'

def is_pdf_scan(pdf_path: Path) -> bool:
    """启发式检测：检查前几页是否有文本"""
    if not fitz: return True
    try:
        with fitz.open(str(pdf_path)) as doc:
            check_pages = min(len(doc), 3)
            total_text = ""
            for i in range(check_pages):
                total_text += doc[i].get_text().strip()
            # 如果平均每页不到 20 个字符，视为扫描版
            return len(total_text) < (check_pages * 20)
    except:
        return True

def apple_vision_ocr(pdf_path: Path) -> Optional[str]:
    """使用 macOS Vision 框架进行 OCR"""
    if not (IS_MAC and HAS_OCR_LIBS and fitz): return None
    
    from Vision import VNImageRequestHandler, VNRecognizeTextRequest
    from AppKit import NSBitmapImageRep, NSImage
    
    results_text = []
    try:
        with fitz.open(str(pdf_path)) as doc:
            for i in range(len(doc)):
                page = doc[i]
                # 渲染页面为高分辨率图像 (300 DPI)
                pix = page.get_pixmap(matrix=fitz.Matrix(2, 2)) 
                img_data = pix.tobytes("png")
                
                # 转换为 Vision 识别所需的格式
                ns_image = NSImage.alloc().initWithData_(img_data)
                bitmap_rep = NSBitmapImageRep.imageRepWithData_(ns_image.TIFFRepresentation())
                cg_image = bitmap_rep.CGImage()
                
                handler = VNImageRequestHandler.alloc().initWithCGImage_options_(cg_image, None)
                request = VNRecognizeTextRequest.alloc().init()
                request.setRecognitionLanguages_(["zh-Hans", "en-US"])
                request.setUsesLanguageCorrection_(True)
                
                if handler.performRequests_error_([request], None):
                    page_text = "\n".join([r.topCandidates_(1)[0].string() for r in request.results()])
                    results_text.append(page_text)
        
        return "\n\n".join(results_text)
    except Exception as e:
        print(f"OCR Error: {str(e)}", file=sys.stderr)
        return None

def convert_pdf_to_org(input_file: Path, output_file: Path) -> bool:
    if not fitz: return False
    
    is_scan = is_pdf_scan(input_file)
    content = None
    
    if is_scan:
        if IS_MAC and HAS_OCR_LIBS:
            print(f"ℹ OCR: Detected scanned PDF, starting Apple Vision OCR...")
            content = apple_vision_ocr(input_file)
        else:
            if IS_MAC:
                print("⚠ Scan detected, but OCR libs missing (pip install pyobjc-framework-Vision Pillow).", file=sys.stderr)
            else:
                print(f"⚠ Scan detected, but OCR is only supported on macOS.", file=sys.stderr)
    
    # 如果 OCR 失败或不是扫描版，尝试普通提取
    if not content:
        try:
            with fitz.open(str(input_file)) as doc:
                content = "".join([page.get_text() for page in doc])
        except Exception as e:
            print(f"PDF Extraction Error: {str(e)}", file=sys.stderr)

    if content and content.strip():
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(content)
        return True
    return False

def convert_generic(input_file: Path, output_file: Path, from_fmt: str) -> bool:
    images_dir = output_file.parent / f"{output_file.stem}_images"
    images_dir.mkdir(parents=True, exist_ok=True)
    cmd = ['pandoc', '--wrap=none', '--standalone', '-t', 'org', 
           '--no-highlight', f'--extract-media={images_dir}', 
           '-f', from_fmt, str(input_file), '-o', str(output_file)]
    try:
        result = subprocess.run(cmd, capture_output=True, text=True)
        if result.returncode == 0:
            return True
    except: pass
    if images_dir.exists() and not any(images_dir.iterdir()):
        images_dir.rmdir()
    return False

def process_file(file_path: Path, reference_dir: Path, archive_dir: Optional[Path]) -> bool:
    if not file_path.exists(): return False
    safe_name = get_safe_filename(file_path.stem)
    output_file = reference_dir / f"{safe_name}.org"
    suffix = file_path.suffix.lower()
    
    success = False
    if suffix == '.md': success = convert_generic(file_path, output_file, 'markdown')
    elif suffix == '.html': success = convert_generic(file_path, output_file, 'html')
    elif suffix == '.epub': success = convert_generic(file_path, output_file, 'epub')
    elif suffix == '.pdf': success = convert_pdf_to_org(file_path, output_file)
    
    if success:
        if archive_dir:
            archive_dir.mkdir(parents=True, exist_ok=True)
            shutil.move(str(file_path), str(archive_dir / file_path.name))
        print(f"✓ Converted: {file_path.name} -> {output_file.name}")
        return True
    return False

def main():
    check_dependencies()
    parser = argparse.ArgumentParser()
    parser.add_argument('--temp')
    parser.add_argument('--file')
    parser.add_argument('--reference', required=True)
    parser.add_argument('--archive')
    args = parser.parse_args()
    
    ref_dir = Path(args.reference)
    arch_dir = Path(args.archive) if args.archive else None
    
    if args.file:
        process_file(Path(args.file), ref_dir, arch_dir)
    elif args.temp:
        temp_dir = Path(args.temp)
        if not temp_dir.exists(): return
        for f in temp_dir.iterdir():
            if f.is_file() and not f.name.startswith('.'):
                process_file(f, ref_dir, arch_dir)

if __name__ == "__main__":
    main()
