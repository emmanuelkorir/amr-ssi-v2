"""Harvest candidate methods sentences (design, setting, dates) for a list of docs.

Usage: python scripts/harvest.py doc1 doc2 ...
"""
from __future__ import annotations

import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
JSON_DIR = ROOT / "corpus" / "json"

TERMS = (
    "was conducted",
    "study design",
    "study period",
    "prospective",
    "retrospective",
    "cross-sectional",
    "cross sectional",
    "cohort",
    "randomized",
    "randomised",
    "between ",
    "from ",
)


def load(doc_id: str) -> dict:
    matches = list(JSON_DIR.glob(f"{doc_id}.json"))
    if not matches:
        matches = list(JSON_DIR.glob(f"{doc_id}*.json"))
    if not matches:
        raise SystemExit(f"No doc matching {doc_id}")
    return json.loads(matches[0].read_text(encoding="utf-8"))


def main() -> None:
    for doc_id in sys.argv[1:]:
        record = load(doc_id)
        print(f"\n########## {doc_id}")
        shown = 0
        for page in record["pages"]:
            text = re.sub(r"\s+", " ", page["text"])
            for part in re.split(r"(?<=[.!?])\s+", text):
                low = part.lower()
                if any(t in low for t in TERMS) and re.search(r"(19|20)\d\d", part):
                    print(f"p{page['page']}: {part.strip()[:420]}")
                    shown += 1
                    if shown >= 5:
                        return_block = True
                        break
            if shown >= 5:
                break


if __name__ == "__main__":
    main()
