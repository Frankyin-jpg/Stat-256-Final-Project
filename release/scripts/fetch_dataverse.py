#!/usr/bin/env python3
"""Fetch the public replication archive with metadata and verified checksums.

No credentials are needed for public Dataverse files. TLS and inherited proxy
settings remain enabled. Archives are retained; --extract also produces a safe
working copy without executing any downloaded code.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import shutil
import tarfile
import tempfile
from urllib.parse import urlencode
from urllib.request import urlopen
import zipfile

DOI = "doi:10.7910/DVN/WCNKB1"
SERVER = "https://dataverse.harvard.edu"


def safe_relative(name: str) -> Path:
    if not isinstance(name, str) or not name or "\\" in name or "\x00" in name:
        raise ValueError(f"Unsafe file path: {name!r}")
    p = PurePosixPath(name)
    if p == PurePosixPath(".") or p.is_absolute() or any(part in ("..", "") for part in p.parts) or ":" in p.parts[0]:
        raise ValueError(f"Unsafe file path: {name!r}")
    if p == PurePosixPath("."):
        raise ValueError("An empty archive path is not a file")
    return Path(*p.parts)


def digest(path: Path, algorithm: str = "sha256") -> str:
    h = hashlib.new(algorithm.lower().replace("-", ""))
    with path.open("rb") as f:
        for block in iter(lambda: f.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


def atomic_json(path: Path, value) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile("w", dir=path.parent, delete=False) as f:
        tmp = Path(f.name)
        json.dump(value, f, indent=2, sort_keys=True)
        f.write("\n")
    os.replace(tmp, path)


def catalog_files(catalog: dict) -> list[dict]:
    if catalog.get("status") != "OK":
        raise ValueError("Dataverse did not return an OK catalog response")
    version = catalog["data"]["latestVersion"]
    result = []
    seen = set()
    for entry in version["files"]:
        f = entry["dataFile"]
        ident = int(f["id"])
        directory = entry.get("directoryLabel") or ""
        relative = safe_relative("/".join(x for x in [directory, f["filename"]] if x))
        # File-ID directories prevent collisions between identical source labels.
        local = Path("originals") / str(ident) / relative
        if ident in seen:
            raise ValueError(f"Duplicate Dataverse file ID: {ident}")
        seen.add(ident)
        result.append({"id": ident, "source_name": str(relative), "path": str(local),
                       "size": int(f["filesize"]),
                       "restricted": bool(entry.get("restricted", False)),
                       "checksum": f.get("checksum"),
                       "url": f"{SERVER}/api/access/datafile/{ident}"})
    return result


def verify_download(path: Path, item: dict) -> None:
    if path.stat().st_size != item["size"]:
        raise ValueError(f"Size mismatch for {item['source_name']}")
    checksum = item.get("checksum")
    if checksum:
        if digest(path, checksum["type"]) != checksum["value"].lower():
            raise ValueError(f"Checksum mismatch for {item['source_name']}")


def download_one(item: dict, root: Path, previous: dict | None = None, opener=urlopen) -> dict:
    path = root / safe_relative(item["path"])
    if item["restricted"]:
        raise ValueError(f"Restricted file requires authorization: {item['source_name']}")
    path.parent.mkdir(parents=True, exist_ok=True)
    # Never follow an externally created symlink inside the destination tree.
    if path.is_symlink() or any(p.is_symlink() for p in path.parents if p != root.parent):
        raise ValueError(f"Symlink in download path: {path}")
    cached = False
    if path.is_file():
        try:
            verify_download(path, item)
            cached = bool(item.get("checksum")) or (
                previous is not None and digest(path) == previous.get("sha256"))
        except ValueError:
            cached = False
    if not cached:
        fd, temp_name = tempfile.mkstemp(prefix=".download-", dir=path.parent)
        tmp = Path(temp_name)
        try:
            with os.fdopen(fd, "wb") as dst, opener(item["url"], timeout=120) as src:
                count = 0
                while block := src.read(1024 * 1024):
                    count += len(block)
                    if count > item["size"]:
                        raise ValueError(f"Download exceeds catalog size: {item['source_name']}")
                    dst.write(block)
            verify_download(tmp, item)
            os.replace(tmp, path)
        finally:
            tmp.unlink(missing_ok=True)
    return {**item, "sha256": digest(path), "remote_checksum_verified": bool(item.get("checksum"))}


def safe_extract(archive: Path, destination: Path, max_bytes: int = 8 * 1024**3) -> None:
    """Extract regular files atomically; reject traversal, links and duplicates."""
    if destination.exists():
        raise FileExistsError(f"Extraction directory exists; inspect it before replacing: {destination}")
    destination.parent.mkdir(parents=True, exist_ok=True)
    temp = Path(tempfile.mkdtemp(prefix=".extract-", dir=destination.parent))
    try:
        if zipfile.is_zipfile(archive):
            with zipfile.ZipFile(archive) as z:
                entries = []
                for info in z.infolist():
                    relative = safe_relative(info.filename.rstrip("/"))
                    mode = (info.external_attr >> 16) & 0o170000
                    if mode not in (0, 0o100000, 0o040000):
                        raise ValueError("Archive contains a link or special file")
                    entries.append((relative, info.is_dir(), info.file_size, info))
                _validate_entries(entries, max_bytes)
                for relative, directory, _, info in entries:
                    target = temp / relative
                    if directory:
                        target.mkdir(parents=True, exist_ok=True)
                    else:
                        target.parent.mkdir(parents=True, exist_ok=True)
                        with z.open(info) as src, target.open("xb") as dst:
                            shutil.copyfileobj(src, dst)
        elif tarfile.is_tarfile(archive):
            with tarfile.open(archive) as t:
                entries = []
                for info in t.getmembers():
                    if info.isdir() and info.name.rstrip("/") in ("", "."):
                        continue
                    if not (info.isdir() or info.isfile()):
                        raise ValueError("Archive contains a link or special file")
                    relative = safe_relative(info.name.rstrip("/"))
                    entries.append((relative, info.isdir(), info.size, info))
                _validate_entries(entries, max_bytes)
                for relative, directory, _, info in entries:
                    target = temp / relative
                    if directory:
                        target.mkdir(parents=True, exist_ok=True)
                    else:
                        target.parent.mkdir(parents=True, exist_ok=True)
                        with t.extractfile(info) as src, target.open("xb") as dst:
                            shutil.copyfileobj(src, dst)
        else:
            raise ValueError(f"Unsupported archive format: {archive.name}")
        os.replace(temp, destination)
    finally:
        if temp.exists():
            shutil.rmtree(temp)


def _validate_entries(entries: list, max_bytes: int) -> None:
    if sum(size for _, _, size, _ in entries) > max_bytes:
        raise ValueError("Archive exceeds maximum unpacked size")
    if any(relative == Path(".archive-sha256") for relative, _, _, _ in entries):
        raise ValueError("Archive collides with extraction provenance marker")
    names = [str(relative) for relative, _, _, _ in entries]
    if len(names) != len(set(names)):
        raise ValueError("Archive contains duplicate paths")
    file_names = {str(relative) for relative, directory, _, _ in entries if not directory}
    if any(str(parent) in file_names for relative, _, _, _ in entries for parent in relative.parents):
        raise ValueError("Archive uses a regular file as a directory")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=Path(__file__).resolve().parents[1] / "data/raw")
    parser.add_argument("--extract", action="store_true")
    parser.add_argument("--max-total-bytes", type=int, default=2 * 1024**3)
    args = parser.parse_args()
    root = args.output.resolve()
    root.mkdir(parents=True, exist_ok=True)
    url = SERVER + "/api/datasets/:persistentId/?" + urlencode({"persistentId": DOI})
    with urlopen(url, timeout=60) as response:
        catalog = json.load(response)
    items = catalog_files(catalog)
    atomic_json(root / "catalog.json", catalog)
    if sum(item["size"] for item in items) > args.max_total_bytes:
        raise ValueError("Catalog exceeds --max-total-bytes; review catalog.json before increasing the limit")
    restricted = [item["source_name"] for item in items if item["restricted"]]
    if restricted:
        raise ValueError(f"Catalog contains restricted files: {restricted}. No credentials are requested or guessed.")
    manifest_path = root / "manifest.json"
    previous = json.loads(manifest_path.read_text()) if manifest_path.exists() else {}
    previous_files = {item["id"]: item for item in previous.get("files", [])}
    manifest = {"doi": DOI, "catalog_url": url, "dataset_version": catalog["data"]["latestVersion"].get("versionNumber"),
                "dataset_minor_version": catalog["data"]["latestVersion"].get("versionMinorNumber"), "complete": False, "files": []}
    for item in items:
        result = download_one(item, root, previous_files.get(item["id"]))
        manifest["files"].append(result)
        atomic_json(manifest_path, manifest)
        print(f"Verified {item['source_name']} ({item['size']} bytes)")
        archive = root / item["path"]
        if args.extract and (zipfile.is_zipfile(archive) or tarfile.is_tarfile(archive)):
            dest = root / "extracted" / str(item["id"])
            marker = dest / ".archive-sha256"
            if dest.exists():
                if not marker.is_file() or marker.read_text().strip() != result["sha256"]:
                    raise ValueError(f"Existing extraction differs or is unverified: {dest}")
                print(f"Retained extraction {dest}; remove it explicitly to re-extract")
            else:
                safe_extract(archive, dest)
                marker.write_text(result["sha256"] + "\n")
    manifest["complete"] = True
    atomic_json(manifest_path, manifest)
    print(f"Saved {len(items)} source files and provenance to {root}")


if __name__ == "__main__":
    main()
