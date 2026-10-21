"""Print page-numbered sentences matching one or more terms.

Usage: python scripts/find_sentence.py <doc_id> <term> [<term> ...]
"""
from __future__ import annotations

import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
JSON_DIR = ROOT / "corpus" / "json"


def load(doc_id: str) -> dict:
    matches = list(JSON_DIR.glob(f"{doc_id}*.json"))
    if not matches:
        raise SystemExit(f"No doc matching {doc_id}")
    return json.loads(matches[0].read_text(encoding="utf-8"))


def main() -> None:
    doc_id = sys.argv[1]
    terms = sys.argv[2:]
    record = load(doc_id)
    for page in record["pages"]:
        text = re.sub(r"\s+", " ", page["text"])
        # split into sentences but keep numeric fragments
        parts = re.split(r"(?<=[.!?])\s+", text)
        for part in parts:
            low = part.lower()
            if any(term.lower() in low for term in terms):
                print(f"p{page['page']}: {part.strip()[:600]}")


if __name__ == "__main__":
    main()
