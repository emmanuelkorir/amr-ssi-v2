"""Print a compact first-page/abstract overview for screening."""
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
for p in sorted((ROOT / "corpus" / "json").glob("*.json")):
    r = json.loads(p.read_text())
    first = r["pages"][0]["text"] if r["pages"] else ""
    title = (r.get("pdf_metadata") or {}).get("title", "")
    print("=" * 110)
    print(f"{r['doc_id']} | {r['source_file']} | pages={r['n_pages']}")
    print(f"PDF TITLE: {title[:180]}")
    print(" ".join(first.split())[:900])
