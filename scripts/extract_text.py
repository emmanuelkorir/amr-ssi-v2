"""Convert every source document in the review folder to page-level JSON.

Outputs
-------
corpus/json/<doc_id>.json   one record per document with page text + bboxes
corpus/manifest.csv         one row per document with provenance/QC fields

Text is extracted with PyMuPDF first. Documents with little or no embedded
text are flagged `needs_ocr=true`; the `ocr_pages` command can then render
those pages and run Tesseract if a Tesseract binary is available.

Usage
-----
python scripts/extract_text.py build
python scripts/extract_text.py ocr --doc-id <id>
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import json
import re
import shutil
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

import pymupdf
from docx import Document

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "amr-ssis-ecsa-files" / "amr-ssis-ecsa"
OUT = ROOT / "corpus"
JSON_DIR = OUT / "json"
TEXT_DIR = OUT / "text"

MIN_CHARS_PER_PAGE = 200  # below this a page is treated as image-only


def slugify(name: str) -> str:
    stem = Path(name).stem.lower()
    stem = re.sub(r"[^a-z0-9]+", "-", stem).strip("-")
    return stem[:80]


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def pdf_to_record(path: Path) -> dict:
    doc = pymupdf.open(path)
    pages = []
    for i, page in enumerate(doc, start=1):
        text = page.get_text("text")
        blocks = []
        for block in page.get_text("blocks"):
            x0, y0, x1, y1, block_text, block_no, block_type = block
            if block_type != 0:
                continue
            blocks.append(
                {
                    "bbox": [round(x0, 1), round(y0, 1), round(x1, 1), round(y1, 1)],
                    "text": block_text,
                }
            )
        pages.append(
            {
                "page": i,
                "text": text,
                "n_chars": len(text.strip()),
                "n_images": len(page.get_images(full=True)),
                "blocks": blocks,
            }
        )
    meta = doc.metadata or {}
    record = {
        "doc_id": slugify(path.name),
        "source_file": str(path.relative_to(ROOT)),
        "sha256": sha256(path),
        "document_type": "pdf",
        "n_pages": len(pages),
        "pdf_metadata": meta,
        "extracted_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "extraction_method": "pymupdf get_text('text') + blocks",
        "pages": pages,
    }
    doc.close()
    return record


def docx_to_record(path: Path) -> dict:
    doc = Document(path)
    paragraphs = []
    for i, para in enumerate(doc.paragraphs, start=1):
        if para.text.strip():
            paragraphs.append({"para": i, "style": para.style.name, "text": para.text})
    tables = []
    for ti, table in enumerate(doc.tables, start=1):
        rows = []
        for row in table.rows:
            rows.append([cell.text.strip() for cell in row.cells])
        tables.append({"table": ti, "rows": rows})
    text = "\n".join(p["text"] for p in paragraphs)
    record = {
        "doc_id": slugify(path.name),
        "source_file": str(path.relative_to(ROOT)),
        "sha256": sha256(path),
        "document_type": "docx",
        "n_pages": None,
        "extracted_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "extraction_method": "python-docx paragraphs + tables",
        "pages": [
            {
                "page": 1,
                "text": text,
                "n_chars": len(text.strip()),
                "n_images": 0,
                "blocks": [{"bbox": None, "text": p["text"]} for p in paragraphs],
            }
        ],
        "paragraphs": paragraphs,
        "tables": tables,
    }
    return record


def sniff_type(path: Path) -> str:
    """Return 'pdf' or 'docx' based on magic bytes, not the filename."""
    head = path.open("rb").read(8)
    if head.startswith(b"%PDF"):
        return "pdf"
    if head.startswith(b"PK\x03\x04"):
        return "docx"
    return "unknown"


def build() -> None:
    JSON_DIR.mkdir(parents=True, exist_ok=True)
    TEXT_DIR.mkdir(parents=True, exist_ok=True)
    rows = []
    files = sorted(
        [p for p in SOURCE.rglob("*") if p.is_file() and not p.name.startswith(".")],
        key=lambda p: str(p).lower(),
    )
    used_ids: dict[str, Path] = {}
    for path in files:
        doc_type = sniff_type(path)
        if doc_type == "unknown":
            print(f"SKIP (unknown type): {path}")
            continue
        base_id = slugify(path.name)
        doc_id = base_id
        if doc_id in used_ids:
            doc_id = f"{base_id}-{sha256(path)[:8]}"
        used_ids[doc_id] = path
        try:
            record = pdf_to_record(path) if doc_type == "pdf" else docx_to_record(path)
        except Exception as exc:  # keep going, record the failure
            rows.append(
                {
                    "doc_id": doc_id,
                    "source_file": str(path.relative_to(ROOT)),
                    "document_type": doc_type,
                    "n_pages": "",
                    "n_chars": "",
                    "chars_per_page": "",
                    "low_text_pages": "",
                    "needs_ocr": "ERROR",
                    "error": f"{type(exc).__name__}: {exc}",
                    "sha256": sha256(path),
                }
            )
            continue
        record["doc_id"] = doc_id
        out_json = JSON_DIR / f"{record['doc_id']}.json"
        out_json.write_text(json.dumps(record, ensure_ascii=False, indent=1), encoding="utf-8")
        total_chars = sum(p["n_chars"] for p in record["pages"])
        low_pages = [p["page"] for p in record["pages"] if p["n_chars"] < MIN_CHARS_PER_PAGE]
        # Narrative documents (docx) do not have pages; treat as not needing OCR.
        needs_ocr = bool(record["document_type"] == "pdf" and low_pages)
        out_txt = TEXT_DIR / f"{record['doc_id']}.txt"
        with out_txt.open("w", encoding="utf-8") as fh:
            for page in record["pages"]:
                fh.write(f"\n\n===== PAGE {page['page']} =====\n\n")
                fh.write(page["text"])
        rows.append(
            {
                "doc_id": record["doc_id"],
                "source_file": record["source_file"],
                "document_type": record["document_type"],
                "n_pages": record["n_pages"] or "",
                "n_chars": total_chars,
                "chars_per_page": round(total_chars / max(record["n_pages"] or 1, 1), 1),
                "low_text_pages": ",".join(map(str, low_pages[:20])),
                "needs_ocr": needs_ocr,
                "error": "",
                "sha256": record["sha256"],
                "title": (record.get("pdf_metadata") or {}).get("title", ""),
            }
        )
        print(f"{record['doc_id']:<55} pages={record['n_pages']!s:<4} chars={total_chars:>8} low={len(low_pages):>3}")

    manifest = OUT / "manifest.csv"
    fieldnames = [
        "doc_id",
        "source_file",
        "document_type",
        "n_pages",
        "n_chars",
        "chars_per_page",
        "low_text_pages",
        "needs_ocr",
        "error",
        "sha256",
        "title",
    ]
    with manifest.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)
    print(f"\nWrote {len(rows)} records to {OUT}")
    print(f"needs_ocr: {sum(1 for r in rows if r['needs_ocr'] is True)}")


def ocr(doc_id: str, dpi: int = 300) -> None:
    """OCR low-text pages with RapidOCR and replace their text in-place."""
    from rapidocr import RapidOCR

    json_path = JSON_DIR / f"{doc_id}.json"
    record = json.loads(json_path.read_text(encoding="utf-8"))
    pdf_path = ROOT / record["source_file"]
    engine = RapidOCR()
    doc = pymupdf.open(pdf_path)
    img_dir = OUT / "ocr_images"
    img_dir.mkdir(exist_ok=True)
    n_done = 0
    for page_record in record["pages"]:
        if page_record["n_chars"] >= MIN_CHARS_PER_PAGE:
            continue
        page = doc[page_record["page"] - 1]
        pix = page.get_pixmap(dpi=dpi)
        img = img_dir / f"{doc_id}-p{page_record['page']}.png"
        pix.save(img)
        result = engine(str(img))
        lines = []
        if result is not None and result.txts:
            for text, score, box in zip(result.txts, result.scores, result.boxes):
                lines.append(
                    {
                        "text": text,
                        "score": round(float(score), 5),
                        "bbox": [[round(float(x), 1), round(float(y), 1)] for x, y in box],
                    }
                )
        page_record["text_embedded"] = page_record["text"]
        page_record["text"] = "\n".join(line["text"] for line in lines)
        page_record["n_chars"] = len(page_record["text"].strip())
        page_record["ocr"] = f"rapidocr PP-OCRv6 @{dpi}dpi"
        page_record["ocr_lines"] = lines
        n_done += 1
    doc.close()
    record["extraction_method"] += " + RapidOCR for low-text pages"
    json_path.write_text(json.dumps(record, ensure_ascii=False, indent=1), encoding="utf-8")
    with (TEXT_DIR / f"{doc_id}.txt").open("w", encoding="utf-8") as fh:
        for page in record["pages"]:
            fh.write(f"\n\n===== PAGE {page['page']} =====\n\n")
            fh.write(page["text"])
    print(f"OCR complete for {doc_id}: {n_done} pages")


def ocr_all(dpi: int = 300) -> None:
    manifest = list(csv.DictReader((OUT / "manifest.csv").open(encoding="utf-8")))
    for row in manifest:
        if row["needs_ocr"] == "True":
            ocr(row["doc_id"], dpi=dpi)


def main() -> None:
    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers(dest="cmd", required=True)
    sub.add_parser("build")
    ocr_parser = sub.add_parser("ocr")
    ocr_parser.add_argument("--doc-id", action="append", default=[])
    ocr_parser.add_argument("--all", action="store_true")
    ocr_parser.add_argument("--dpi", type=int, default=300)
    args = parser.parse_args()
    if args.cmd == "build":
        build()
    elif args.cmd == "ocr":
        if args.all:
            ocr_all(dpi=args.dpi)
        for doc_id in args.doc_id:
            ocr(doc_id, dpi=args.dpi)


if __name__ == "__main__":
    main()
