"""Check that every evidence spec resolves to a verbatim page slice."""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from evidence_lib import page_text, resolve_quote  # noqa: E402
from records import EV_SPECS  # noqa: E402


def main() -> None:
    ok = 0
    failures = []
    for spec in EV_SPECS:
        try:
            quote = resolve_quote(spec["doc_id"], spec["page"], spec["start"], spec["end"])
        except Exception as exc:  # noqa: BLE001
            failures.append((spec, exc))
            continue
        assert quote in page_text(spec["doc_id"], spec["page"])
        if len(quote) > 600:
            print(f"LONG {spec['evidence_id']} ({len(quote)} chars): {quote[:120]}...")
        ok += 1
    print(f"\nresolved {ok}/{len(EV_SPECS)}")
    for spec, exc in failures:
        print(f"FAIL {spec['evidence_id']} [{spec['doc_id']} p{spec['page']}]: {exc}")
    if failures:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
