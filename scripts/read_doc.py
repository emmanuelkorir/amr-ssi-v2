"""Page-numbered reading helpers for the extracted corpus.

Examples
--------
python scripts/read_doc.py show abayneh-et-al --pages 1-3
python scripts/read_doc.py grep abayneh-et-al "surgical site infection" "262"
python scripts/read_doc.py tables boru-et-al
"""
from __future__ import annotations

import argparse
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
JSON_DIR = ROOT / "corpus" / "json"


def load(doc_id: str) -> dict:
    matches = list(JSON_DIR.glob(f"{doc_id}*.json"))
    if not matches:
        raise SystemExit(f"No doc matching {doc_id}")
    return json.loads(matches[0].read_text(encoding="utf-8"))


def show(doc_id: str, pages: str | None) -> None:
    record = load(doc_id)
    selected = None
    if pages:
        a, _, b = pages.partition("-")
        selected = range(int(a), int(b or a) + 1)
    for page in record["pages"]:
        if selected and page["page"] not in selected:
            continue
        print(f"\n===== PAGE {page['page']} =====\n")
        print(page["text"])


def grep(doc_id: str, terms: list[str], context: int = 2) -> None:
    record = load(doc_id)
    for page in record["pages"]:
        text = page["text"]
        lines = text.splitlines()
        for i, line in enumerate(lines):
            low = line.lower()
            if any(term.lower() in low for term in terms):
                lo = max(0, i - context)
                hi = min(len(lines), i + context + 1)
                snippet = "\n".join(lines[lo:hi])
                print(f"\n--- p{page['page']} line {i + 1} ---\n{snippet}")


def main() -> None:
    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers(dest="cmd", required=True)
    p_show = sub.add_parser("show")
    p_show.add_argument("doc_id")
    p_show.add_argument("--pages")
    p_grep = sub.add_parser("grep")
    p_grep.add_argument("doc_id")
    p_grep.add_argument("terms", nargs="+")
    p_grep.add_argument("--context", type=int, default=2)
    args = parser.parse_args()
    if args.cmd == "show":
        show(args.doc_id, args.pages)
    else:
        grep(args.doc_id, args.terms, args.context)


if __name__ == "__main__":
    main()
