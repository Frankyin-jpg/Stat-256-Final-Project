#!/usr/bin/env python3
"""Reproduce acquisition of the repository author's supplied data-and-paper ZIP.

This release supplies the SAS data and paper, not the original SAS programs.
Its relationship to the complete Harvard Dataverse deposit is not asserted.
"""
from __future__ import annotations
import argparse
from pathlib import Path
import zipfile

from fetch_dataverse import atomic_json, digest, download_one, safe_extract

URL = "https://github.com/Frankyin-jpg/Stat-256-Final-Project/releases/download/replication-data-v1/data_and_paper.zip"
SHA256 = "a7479a211d034f8fb9209c03435b9be5742f690924c5880cd1a855d5040bc0e4"
SIZE = 91436493
RELATIVE = "originals/github-release/data_and_paper.zip"


def acquire(root: Path, extract: bool = True) -> dict:
    root.mkdir(parents=True, exist_ok=True)
    item = {"id": "github-release", "source_name": "data_and_paper.zip", "path": RELATIVE,
            "size": SIZE, "restricted": False, "url": URL,
            "checksum": {"type": "SHA-256", "value": SHA256},
            "checksum_origin": "pinned_initial_supplied_release_download"}
    downloaded = download_one(item, root)
    archive = root / RELATIVE
    with zipfile.ZipFile(archive) as z:
        contents = [{"path": info.filename, "size": info.file_size, "crc32": f"{info.CRC:08x}"}
                    for info in z.infolist() if not info.is_dir()]
    if not any(info["path"] == "data_and_paper/marcps_w.sas7bdat" for info in contents):
        raise ValueError("Pinned source archive does not contain the expected SAS data")
    catalog = {"source_type": "github_release_mirror", "source_url": URL,
               "release_tag": "replication-data-v1", "archive_contents": contents,
               "referenced_dataset_doi": "doi:10.7910/DVN/WCNKB1",
               "scope": "Repository author's supplied SAS data and paper; original SAS programs are absent; not a complete Dataverse acquisition"}
    atomic_json(root / "catalog.json", catalog)
    if extract:
        destination = root / "extracted/github-release"
        marker = destination / ".archive-sha256"
        if destination.exists():
            if not marker.is_file() or marker.read_text().strip() != SHA256:
                raise ValueError("Existing extraction has no matching provenance marker; inspect it before replacing")
        else:
            safe_extract(archive, destination)
            marker.write_text(SHA256 + "\n")
    manifest = {"schema_version": 1, "source_type": "github_release_mirror", "source_url": URL,
                "doi": "doi:10.7910/DVN/WCNKB1",
                "doi_status": "Referenced by historical repository; equality to complete Dataverse deposit not independently verified",
                "complete": True, "completeness_scope": "Single supplied release archive, not the complete original deposit",
                "original_programs_available": False, "files": [downloaded]}
    atomic_json(root / "manifest.json", manifest)
    return manifest


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=Path(__file__).resolve().parents[1] / "data/raw")
    parser.add_argument("--no-extract", action="store_true")
    args = parser.parse_args()
    manifest = acquire(args.output.resolve(), not args.no_extract)
    print("Verified supplied release SHA-256:", manifest["files"][0]["sha256"])
    print("Saved provenance for SAS data and original-paper PDF; original SAS programs are not supplied.")


if __name__ == "__main__":
    main()
