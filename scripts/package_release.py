#!/usr/bin/env python3
"""Build a portable, checksum-inventoried release from verified empirical outputs.

Run from any directory. This command neither downloads data nor runs an analysis,
commits, pushes, silently substitutes fixture results, or drops large data files.
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import itertools
import json
import math
import os
from pathlib import Path, PurePosixPath
import shutil
import subprocess
import tempfile
from urllib.parse import urlparse

LFS_THRESHOLD = 95 * 1024**2
MAPPING_STATUS = "publication_and_source_labels_verified"
REPLICATION_STATUSES = {"benchmarks_verified", "reconstruction_discrepancies_reported"}
REQUIRED_RESULTS = (
    "descriptives.csv", "event_study.csv", "extension.csv", "overlap.csv",
    "balance.csv", "sensitivity.csv", "extension_samples.csv", "data-quality.csv",
    "placebo.csv", "distributional.csv", "extension_rich.csv",
    "extended_diagnostics.csv", "bootstrap.csv", "bootstrap_replicates.csv", "bootstrap_status.csv", "replication_benchmarks.csv",
)
DOCUMENTS = {
    "_output/report/paper.pdf": "manuscript/paper.pdf",
    "_output/report/paper.html": "manuscript/paper.html",
    "_output/report/appendix.pdf": "appendix/appendix.pdf",
    "_output/report/appendix.html": "appendix/appendix.html",
}


class ReleaseError(ValueError):
    pass


def digest(path: Path, algorithm: str = "sha256") -> str:
    value = hashlib.new(algorithm.lower().replace("-", ""))
    with path.open("rb") as f:
        for block in iter(lambda: f.read(1024**2), b""):
            value.update(block)
    return value.hexdigest()


def relative_path(value: str) -> Path:
    p = PurePosixPath(value)
    if not value or p == PurePosixPath(".") or p.is_absolute() or ".." in p.parts or "\\" in value or ":" in p.parts[0]:
        raise ReleaseError(f"Expected a safe relative path, got {value!r}")
    return Path(*p.parts)


def checked_file(root: Path, relative: str) -> Path:
    path = root / relative_path(relative)
    if not path.is_file() or path.stat().st_size == 0:
        raise ReleaseError(f"Required file is missing or empty: {relative}")
    if path.is_symlink() or not path.resolve().is_relative_to(root.resolve()):
        raise ReleaseError(f"Release input must be an ordinary file inside the repository: {relative}")
    return path


def read_json(path: Path):
    try:
        value = json.loads(path.read_text())
    except (OSError, ValueError) as e:
        raise ReleaseError(f"Cannot read JSON provenance {path.name}: {e}") from e
    if not isinstance(value, dict):
        raise ReleaseError(f"JSON provenance must be an object: {path.name}")
    return value


def read_rows(root: Path, relative: str, required=(), allow_empty: bool = False) -> list[dict]:
    path = checked_file(root, relative)
    with path.open(newline="") as f:
        reader = csv.DictReader(f)
        if not reader.fieldnames or set(required) - set(reader.fieldnames):
            raise ReleaseError(f"Missing columns in {relative}: {sorted(set(required) - set(reader.fieldnames or []))}")
        rows = list(reader)
    if not rows and not allow_empty:
        raise ReleaseError(f"Required empirical table contains no rows: {relative}")
    return rows


def read_r_evidence(root: Path) -> dict:
    """Read selected metadata only; no execution of deposited analysis programs."""
    code = '''args <- commandArgs(TRUE)
run <- readRDS(args[1])
cps <- readRDS(args[2])
if (!is.list(run) || !is.list(cps) || !is.data.frame(cps$data) || !is.list(cps$metadata)) stop("Unsupported analysis metadata or canonical data schema")
keys <- c("mapping_status", "data_kind", "input_md5", "source_file", "run_utc", "git_commit", "extension_failures", "code_md5", "replication_status", "replication_benchmarks_total", "replication_benchmarks_failed")
jsonlite::write_json(list(run = run[intersect(keys, names(run))], canonical = cps$metadata[intersect(keys, names(cps$metadata))], canonical_rows = nrow(cps$data)), stdout(), auto_unbox = TRUE, null = "null")'''
    try:
        process = subprocess.run(
            ["Rscript", "--vanilla", "-e", code, str(root / "results/run-metadata.rds"), str(root / "data/derived/cps.rds")],
            check=False, text=True, capture_output=True, cwd=root,
        )
    except OSError as e:
        raise ReleaseError("Rscript is required to verify run metadata and canonical CPS data; activate the project R environment") from e
    if process.returncode:
        raise ReleaseError("R metadata validation failed: " + process.stderr.strip())
    try:
        return json.loads(process.stdout)
    except ValueError as e:
        raise ReleaseError("R metadata validation did not return valid JSON") from e


def _verify_evidence(root: Path, evidence: dict, source_manifest: dict) -> dict:
    run, canonical = evidence.get("run", {}), evidence.get("canonical", {})
    for label, item in (("run", run), ("canonical data", canonical)):
        if item.get("mapping_status") != MAPPING_STATUS:
            raise ReleaseError(f"{label}: publication and source-label mappings have not been verified")
        if item.get("data_kind") != "empirical":
            raise ReleaseError(f"{label}: provenance is not marked empirical; generated fixtures cannot be released")
        if item.get("replication_status") not in REPLICATION_STATUSES:
            raise ReleaseError(f"{label}: independent replication benchmarks are unrun or their comparison status is unsupported")
        if item.get("extension_failures") != 0:
            raise ReleaseError(f"{label}: analysis diagnostics are incomplete or failed")
    if evidence.get("canonical_rows", 0) <= 0:
        raise ReleaseError("Canonical CPS data have no rows")
    if run["replication_status"] != canonical["replication_status"]:
        raise ReleaseError("Run and canonical-data replication status disagree")
    for field in ("input_md5", "source_file", "run_utc", "git_commit"):
        if not run.get(field) or run.get(field) != canonical.get(field):
            raise ReleaseError(f"Run and canonical-data provenance disagree or omit {field}")
    input_path = Path(run["source_file"])
    if not input_path.is_absolute():
        input_path = root / input_path
    input_path = input_path.resolve()
    raw_root = (root / "data/raw").resolve()
    if not input_path.is_relative_to(raw_root) or not input_path.is_file():
        raise ReleaseError("The analysis input must be present under data/raw and linked to the deposited source")
    if digest(input_path, "md5") != run["input_md5"]:
        raise ReleaseError("Analysis input differs from the checksum recorded in the executed run")
    raw_relative = input_path.relative_to(raw_root)
    originals = {str(relative_path(item["path"])): item for item in source_manifest["files"]}
    if str(raw_relative) not in originals:
        if len(raw_relative.parts) < 3 or raw_relative.parts[0] != "extracted":
            raise ReleaseError("Analysis input is not an acquired original or a provenance-linked extraction")
        file_id = raw_relative.parts[1]
        source = next((item for item in source_manifest["files"] if str(item["id"]) == file_id), None)
        marker = raw_root / "extracted" / file_id / ".archive-sha256"
        if source is None or not marker.is_file() or marker.read_text().strip() != source["sha256"]:
            raise ReleaseError("Analysis input extraction is not linked to its verified original archive")
    if not isinstance(run.get("code_md5"), dict) or not run["code_md5"] or run["code_md5"] != canonical.get("code_md5"):
        raise ReleaseError("Run metadata omit matching analysis source hashes")
    for relative, expected in run["code_md5"].items():
        if digest(checked_file(root, relative), "md5") != expected:
            raise ReleaseError(f"Analysis source changed after the empirical run: {relative}")
    # Keep portable paths in the release, never the cloud machine's absolute path.
    return {"mapping_status": MAPPING_STATUS, "data_kind": "empirical", "replication_status": run["replication_status"],
            "input_path": str(input_path.relative_to(root.resolve())), "input_md5": run["input_md5"],
            "input_sha256": digest(input_path), "run_utc": run["run_utc"],
            "git_commit": run["git_commit"], "canonical_rows": evidence["canonical_rows"]}



def _verify_source_record(root: Path, source_manifest: dict, catalog: dict,
                          verification: dict, provenance: dict) -> dict:
    """Bind the audit to the acquired mirror without calling it a full deposit."""
    if source_manifest.get("source_type") != "github_release_mirror":
        raise ReleaseError("Unsupported or missing source_type; this release uses the supplied GitHub release mirror")
    source_url = source_manifest.get("source_url", "")
    if not isinstance(source_url, str):
        raise ReleaseError("Source provenance URL must be a string")
    parsed = urlparse(source_url)
    if (parsed.scheme != "https" or parsed.hostname != "github.com" or
            not parsed.path.startswith("/Frankyin-jpg/Stat-256-Final-Project/releases/download/") or
            parsed.username or parsed.password):
        raise ReleaseError("GitHub source provenance has an invalid supplied-release URL")
    if (catalog.get("source_type") != source_manifest["source_type"] or
            catalog.get("source_url") != source_url or
            catalog.get("referenced_dataset_doi") != source_manifest["doi"]):
        raise ReleaseError("Source catalog and acquisition manifest disagree on source_type, URL, or referenced DOI")
    for label, value in (("completeness_scope", source_manifest.get("completeness_scope")),
                         ("doi_status", source_manifest.get("doi_status")),
                         ("catalog scope", catalog.get("scope"))):
        if not isinstance(value, str) or not value.strip():
            raise ReleaseError(f"Source provenance must disclose {label}")
    if (source_manifest.get("original_programs_available") is not False or
            verification.get("original_programs_available") is not False):
        raise ReleaseError("The supplied archive audit must disclose that original SAS programs are unavailable")
    if verification.get("mapping_status") != MAPPING_STATUS or verification.get("data_kind") != "empirical":
        raise ReleaseError("Source-verification record is not an audited empirical publication/source-label reconstruction")
    for key in ("input_md5", "input_sha256", "input_path"):
        if verification.get(key) != provenance[key]:
            raise ReleaseError(f"Source-verification record does not match the executed {key}")
    if verification.get("raw_manifest_sha256") != digest(root / "data/raw/manifest.json"):
        raise ReleaseError("Source-verification record does not match the acquired-source manifest")
    basis = verification.get("verification_basis")
    if not isinstance(basis, dict) or not all(isinstance(basis.get(key), str) and basis[key].strip()
                                            for key in ("source_labels", "publication_timing", "weights")):
        raise ReleaseError("Source-verification record lacks the publication, source-label, or weight audit basis")
    limitations = basis.get("limitations")
    if not isinstance(limitations, list) or not limitations or not all(isinstance(x, str) and x.strip() for x in limitations):
        raise ReleaseError("Source-verification audit must record its limitations")
    if not any("program" in item.lower() and any(term in item.lower() for term in
                ("no original", "unavailable", "absent", "missing", "not available")) for item in limitations):
        raise ReleaseError("Source-verification limitations must explicitly disclose missing original programs")
    return {"source_type": source_manifest["source_type"], "source_url": source_url,
            "referenced_dataset_doi": source_manifest["doi"],
            "doi_status": source_manifest["doi_status"],
            "completeness_scope": source_manifest["completeness_scope"],
            "original_programs_available": False,
            "verification_basis": basis}


def _verify_benchmarks(root: Path, evidence: dict) -> dict:
    rows = read_rows(root, "results/replication_benchmarks.csv",
                     ("original_value", "reproduced_value", "tolerance", "passed"))
    failed = []
    identifiers = set()
    for index, row in enumerate(rows, 1):
        label = (row.get("benchmark_id") or "").strip() or f"row {index}"
        if "benchmark_id" in row:
            if not (row["benchmark_id"] or "").strip() or label in identifiers:
                raise ReleaseError("Replication benchmark identifiers are empty or duplicated")
            identifiers.add(label)
        try:
            original, reproduced, tolerance = (float(row[key]) for key in
                                               ("original_value", "reproduced_value", "tolerance"))
        except (TypeError, ValueError) as e:
            raise ReleaseError(f"Unrun or invalid replication benchmark values: {label}") from e
        if not all(math.isfinite(x) for x in (original, reproduced, tolerance)) or tolerance < 0:
            raise ReleaseError(f"Unrun/nonfinite benchmark or negative tolerance: {label}")
        flag = (row["passed"] or "").strip().lower()
        numerical_pass = abs(reproduced - original) <= tolerance
        if flag not in ("true", "false") or (flag == "true") != numerical_pass:
            raise ReleaseError(f"Replication benchmark passed flag disagrees with its numerical comparison: {label}")
        if not numerical_pass:
            failed.append(label)
    status = "reconstruction_discrepancies_reported" if failed else "benchmarks_verified"
    for label, item in (("run", evidence["run"]), ("canonical data", evidence["canonical"])):
        if item["replication_status"] != status:
            raise ReleaseError(f"{label}: replication status disagrees with {len(failed)} benchmark discrepancies")
        for key, count in (("replication_benchmarks_total", len(rows)),
                           ("replication_benchmarks_failed", len(failed))):
            if key in item and (isinstance(item[key], bool) or item[key] != count):
                raise ReleaseError(f"{label}: {key} disagrees with the saved benchmark comparisons")
    audit = None
    if failed:
        audit = "docs/replication-audit.md"
        checked_file(root, audit)
    return {"status": status, "total": len(rows), "passed": len(rows) - len(failed),
            "discrepancies": len(failed), "unrun": 0, "discrepant_benchmark_ids": failed,
            "audit_path": audit,
            "note": (f"{len(failed)} of {len(rows)} executed comparisons exceed their recorded tolerance. "
                     "These discrepancies are retained and discussed in docs/replication-audit.md; "
                     "this release does not claim a perfect replication."
                     if failed else
                     "All declared executed comparisons meet their recorded tolerances; this does not establish that every historical result was replicated.")}


def _verify_result_grids(root: Path) -> None:
    methods = ("unadjusted_did", "aipw_standardized_did", "outcome_standardized_did", "ipw_standardized_did")
    strata = list(itertools.product(("male", "female"), ("young", "old")))
    outcomes = ("weeks", "worked_any")
    common = ("sex", "age_sample", "outcome", "method")

    def grid(name, columns, expected):
        rows = read_rows(root, "results/" + name, columns + ("estimate",))
        try:
            if not all(math.isfinite(float(row["estimate"])) for row in rows):
                raise ValueError("nonfinite estimate")
        except (TypeError, ValueError) as e:
            raise ReleaseError(f"Required empirical estimates are missing or nonfinite: {name}") from e
        found = [tuple(row[c] for c in columns) for row in rows]
        if len(found) != len(expected) or set(found) != set(expected):
            raise ReleaseError(f"Incomplete or duplicated prespecified result grid: {name}")
        return rows

    base = [(sex, age, outcome, method) for sex, age in strata for outcome in outcomes for method in methods]
    grid("placebo.csv", common, base)
    grid("bootstrap.csv", common, base)
    grid("extension.csv", common + ("window",),
         [x + (window,) for x in base for window in ("main", "later_coverage", "broader_pre")])
    grid("extension_rich.csv", common + ("covariate_set",),
         [x + (covariates,) for x in base for covariates in ("primary_same_sample", "rich")])
    grid("distributional.csv", common + ("threshold",),
         [(sex, age, "cdf_weeks", method, str(threshold)) for sex, age in strata for method in methods for threshold in (0, 13, 26, 39, 51)])
    diagnostics = read_rows(root, "results/extended_diagnostics.csv", ("analysis", "sex", "age_sample", "outcome", "covariate_set", "threshold"))
    keys = ("analysis", "sex", "age_sample", "outcome", "covariate_set", "threshold")
    expected = [("placebo", sex, age, outcome, "primary", "") for sex, age in strata for outcome in outcomes]
    expected += [("rich", sex, age, outcome, covariates, "") for sex, age in strata for outcome in outcomes for covariates in ("primary_same_sample", "rich")]
    expected += [("distribution", sex, age, "cdf_weeks", "primary", str(threshold)) for sex, age in strata for threshold in (0, 13, 26, 39, 51)]
    found = [tuple(row[c] for c in keys) for row in diagnostics]
    if len(found) != 44 or set(found) != set(expected):
        raise ReleaseError("Incomplete or duplicated prespecified extended diagnostics")
    draws = read_rows(root, "results/bootstrap_replicates.csv", common + ("replicate", "success", "estimate"))
    counts = {key: set() for key in base}
    for draw in draws:
        key = tuple(draw[c] for c in common)
        try:
            estimate = float(draw["estimate"])
            replicate = int(draw["replicate"])
        except ValueError as e:
            raise ReleaseError("Invalid bootstrap replicate record") from e
        if key not in counts or draw["success"].lower() != "true" or not math.isfinite(estimate) or replicate < 1 or replicate in counts[key]:
            raise ReleaseError("Failed, invalid, or duplicated bootstrap replicate record")
        counts[key].add(replicate)
    statuses = read_rows(root, "results/bootstrap_status.csv", ("sex", "age_sample", "outcome", "replicate", "success"))
    status_counts = {(sex, age, outcome): set() for sex, age in strata for outcome in outcomes}
    for row in statuses:
        key = (row["sex"], row["age_sample"], row["outcome"])
        try:
            replicate = int(row["replicate"])
        except ValueError as e:
            raise ReleaseError("Invalid bootstrap status replicate index") from e
        if key not in status_counts or row["success"].lower() != "true" or replicate < 1 or replicate in status_counts[key]:
            raise ReleaseError("Failed, invalid, or duplicated bootstrap status record")
        status_counts[key].add(replicate)
    for row in read_rows(root, "results/bootstrap.csv", common + ("n_success",)):
        key = tuple(row[c] for c in common)
        if len(counts[key]) != int(row["n_success"]) or status_counts[key[:3]] != counts[key]:
            raise ReleaseError("Bootstrap summary count does not match saved replicate records")

def build_manifest(root: Path, evidence_loader=read_r_evidence, documents=None) -> dict:
    root = root.resolve()
    documents = DOCUMENTS if documents is None else documents
    source_manifest = read_json(checked_file(root, "data/raw/manifest.json"))
    if source_manifest.get("complete") is not True or not source_manifest.get("files"):
        raise ReleaseError("Raw-source download manifest is incomplete or contains no source files")
    if source_manifest.get("doi") != "doi:10.7910/DVN/WCNKB1":
        raise ReleaseError("Raw-source manifest identifies an unexpected dataset")
    selected = {}

    def add(relative: str, category: str, destination: str | None = None):
        path = checked_file(root, relative)
        destination = str(relative_path(destination or relative))
        if destination in selected and selected[destination]["source"] != relative:
            raise ReleaseError(f"Duplicate release destination: {destination}")
        selected[destination] = {"source": relative, "path": destination, "category": category,
                                 "size": path.stat().st_size, "sha256": digest(path),
                                 "git_lfs_required": path.stat().st_size > LFS_THRESHOLD}

    for item in source_manifest["files"]:
        rel = str(Path("data/raw") / relative_path(item["path"]))
        path = checked_file(root, rel)
        if path.stat().st_size != item["size"] or digest(path) != item.get("sha256"):
            raise ReleaseError(f"Raw original is missing, changed or corrupt: {rel}")
        checksum = item.get("checksum")
        if checksum and digest(path, checksum["type"]) != checksum["value"].lower():
            raise ReleaseError(f"Recorded source checksum mismatch: {rel}")
        add(rel, "acquired_source_files")
    for rel in ("data/raw/catalog.json", "data/raw/manifest.json", "data/source-verification.json"):
        add(rel, "provenance")
    for rel in ("data/derived/cps.rds", "results/run-metadata.rds", "results/session-info.txt"):
        add(rel, "analysis_data_and_run")
    evidence = evidence_loader(root)
    provenance = _verify_evidence(root, evidence, source_manifest)
    verification = read_json(root / "data/source-verification.json")
    source_provenance = _verify_source_record(root, source_manifest,
        read_json(root / "data/raw/catalog.json"), verification, provenance)
    benchmarks = _verify_benchmarks(root, evidence)
    if benchmarks["audit_path"]:
        add(benchmarks["audit_path"], "replication_discrepancy_audit")
    for name in REQUIRED_RESULTS:
        read_rows(root, "results/" + name)
        add("results/" + name, "empirical_tables")
    failures = read_rows(root, "results/extension_failures.csv", allow_empty=True)
    if failures:
        raise ReleaseError("Extension failure table is nonempty; the release is not a complete empirical analysis")
    add("results/extension_failures.csv", "diagnostics")
    read_rows(root, "results/exclusions.csv", allow_empty=True)
    add("results/exclusions.csv", "diagnostics")
    _verify_result_grids(root)
    bootstrap = read_rows(root, "results/bootstrap.csv", ("n_success", "n_requested", "conf_low", "conf_high", "intervals_available"))
    for row in bootstrap:
        try:
            count, low, high = float(row["n_success"]), float(row["conf_low"]), float(row["conf_high"])
        except (TypeError, ValueError) as e:
            raise ReleaseError("Bootstrap table has missing or invalid replication counts/intervals") from e
        if not all(math.isfinite(x) for x in (count, low, high)) or count < 199 or count != int(count) or low > high or row["intervals_available"].lower() != "true" or row["n_requested"] != row["n_success"]:
            raise ReleaseError("Each bootstrap estimate needs at least 199 successful replications and a finite ordered interval")

    # Include curated project source and generated assets, never runtime/tool caches
    # or results/run-* scratch directories. Source archives already include the
    # supplied archive, so extraction copies are not duplicated in the bundle.
    for relative in ("README.md", "LICENSE", "Makefile", "_quarto.yml",
                     "R/prepare_data.R", "R/event_study.R", "R/estimate_did.R",
                     "R/extended_analysis.R", "R/bootstrap_did.R", "scripts/run_analysis.R",
                     "report/paper.qmd", "report/appendix.qmd", "report/references.bib"):
        add(relative, "project")
    patterns = {"R": ("*.R",), "scripts": ("*.R", "*.py", "*.sh"),
                "tests": ("*.R", "*.py"), "env": ("*.yml", "*.yaml", "*.lock"),
                "report": ("*.qmd", "*.bib"), "docs": ("*.md",)}
    for directory, globs in patterns.items():
        for glob in globs:
            for path in sorted((root / directory).rglob(glob)):
                if path.is_file() and "__pycache__" not in path.parts:
                    add(str(path.relative_to(root)), "source_and_documentation")
    for relative in ("data/README.md", "env/environment.yml", "env/conda-linux-64.lock"):
        add(relative, "source_and_documentation")
    table_paths = [p for p in (root / "results/tables").rglob("*") if p.is_file() and p.suffix in (".md", ".tex") and p.stat().st_size]
    if not table_paths:
        raise ReleaseError("Publication tables have not been exported to results/tables")
    for directory in ("results", "data/derived"):
        if (root / directory).is_dir():
            for path in sorted((root / directory).rglob("*")):
                components = path.relative_to(root / directory).parts
                if path.is_file() and not any(part.startswith((".", "run-")) for part in components):
                    add(str(path.relative_to(root)), "derived_assets")
    latest_result = max((root / item["source"]).stat().st_mtime_ns for item in selected.values()
                        if item["source"].startswith(("results/", "data/derived/", "report/")) or item["source"] == "_quarto.yml")
    for source, destination in documents.items():
        path = checked_file(root, source)
        if path.stat().st_mtime_ns < latest_result:
            raise ReleaseError(f"Rendered document predates empirical outputs or report source; render again before packaging: {source}")
        if source.endswith(".pdf"):
            with path.open("rb") as document:
                if document.read(5) != b"%PDF-":
                    raise ReleaseError(f"Expected a rendered PDF: {source}")
        add(source, "rendered_manuscript_or_appendix", destination)
    return {"schema_version": 1, "kind": "empirical_research_release",
            "referenced_dataset_doi": source_manifest["doi"], "source": source_provenance,
            "provenance": provenance, "replication_comparison": benchmarks,
            "files": sorted(selected.values(), key=lambda x: x["path"]),
            "large_file_threshold_bytes": LFS_THRESHOLD,
            "git_lfs_required": sorted(item["path"] for item in selected.values() if item["git_lfs_required"]),
            "note": "Hashes inventory the released artifacts; they do not independently establish the scientific interpretation or prove that a manuscript was derived from those inputs."}


def write_release(root: Path, output: Path, manifest: dict, manifest_only: bool = False) -> None:
    if output.exists():
        raise ReleaseError(f"Release destination already exists; preserve or inspect it and choose a new output directory: {output}")
    output.parent.mkdir(parents=True, exist_ok=True)
    temp = Path(tempfile.mkdtemp(prefix=".release-", dir=output.parent))
    try:
        if not manifest_only:
            for item in manifest["files"]:
                source = checked_file(root, item["source"])
                target = temp / relative_path(item["path"])
                target.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(source, target)
                if digest(target) != item["sha256"]:
                    raise ReleaseError(f"Input changed while assembling release: {item['source']}")
        manifest = {**manifest, "materialized": not manifest_only}
        (temp / "manifest.json").write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n")
        os.replace(temp, output)
    finally:
        if temp.exists():
            shutil.rmtree(temp)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--output", type=Path, help="Destination directory (default: <root>/release)")
    parser.add_argument("--manifest-only", action="store_true", help="Validate and inventory; do not duplicate large data files")
    args = parser.parse_args()
    root = args.root.resolve()
    output = args.output.resolve() if args.output else root / "release"
    try:
        manifest = build_manifest(root)
        write_release(root, output, manifest, args.manifest_only)
    except (ReleaseError, OSError, KeyError, TypeError) as e:
        parser.exit(1, f"Release not created: {e}\n")
    print(f"{'Inventory' if args.manifest_only else 'Release'} written to {output}; {len(manifest['files'])} files")
    if manifest["git_lfs_required"]:
        print("Git LFS is required before committing/pushing these files to GitHub; no files were dropped:")
        for name in manifest["git_lfs_required"]:
            print("  " + name)
    print("No Git commit or push was performed.")


if __name__ == "__main__":
    main()
