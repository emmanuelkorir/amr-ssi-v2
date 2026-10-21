"""Resolve verbatim evidence quotes from the page-level corpus.

Quotes are never typed by hand into the dataset.  Each evidence entry
stores two short anchors (``start`` and ``end``) plus the page on which
they occur; the builder slices the normalised page text between the two
anchors.  This guarantees that every quote in the exported dataset is a
literal substring of the source document.
"""
from __future__ import annotations

import json
import re
from functools import lru_cache
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
JSON_DIR = ROOT / "corpus" / "json"


def normalise(text: str) -> str:
    """Collapse all whitespace (including PDF line breaks) to single spaces."""
    return " ".join(text.split())


@lru_cache(maxsize=None)
def load_doc(doc_id: str) -> dict:
    path = JSON_DIR / f"{doc_id}.json"
    if not path.exists():
        raise FileNotFoundError(f"corpus/json/{doc_id}.json not found")
    return json.loads(path.read_text(encoding="utf-8"))


@lru_cache(maxsize=None)
def page_text(doc_id: str, page: int) -> str:
    record = load_doc(doc_id)
    for entry in record["pages"]:
        if entry["page"] == page:
            return normalise(entry["text"])
    raise KeyError(f"{doc_id} has no page {page}")


def resolve_quote(doc_id: str, page: int, start: str, end: str | None = None) -> str:
    """Return the verbatim page slice running from ``start`` through ``end``.

    Matching is case-sensitive on whitespace-normalised text so that the
    returned quote can be checked against the corpus with a simple
    ``quote in page_text`` assertion.
    """
    text = page_text(doc_id, page)
    needle = normalise(start)
    idx = text.find(needle)
    if idx < 0:
        raise ValueError(f"start anchor not found on {doc_id} p{page}: {start!r}")
    if end is None:
        tail = text[idx + len(needle):]
        stop = re.search(r"(?<=[.!?])\s", tail)
        return text[idx: idx + len(needle) + (stop.start() + 1 if stop else len(tail))]
    end_n = normalise(end)
    jdx = text.find(end_n, idx)
    if jdx < 0:
        raise ValueError(f"end anchor not found on {doc_id} p{page}: {end!r}")
    return text[idx: jdx + len(end_n)]


def doc_pages(doc_id: str) -> int:
    return load_doc(doc_id)["n_pages"]
