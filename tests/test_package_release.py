"""Temporary software fixtures only; never create or label an actual research release."""
import copy
import csv
import hashlib
import importlib.util
import itertools
import json
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("package_release", Path(__file__).resolve().parents[1] / "scripts/package_release.py")
f = importlib.util.module_from_spec(spec)
spec.loader.exec_module(f)


def csv_file(path, rows, fields=None):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields or list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def fixture(root):
    """All bytes and metadata are fake, isolated in TemporaryDirectory."""
    def put(name, content="TEST FIXTURE ONLY\n"):
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        return path
    for name in ("README.md", "LICENSE", "Makefile", "_quarto.yml", "data/README.md",
                 "env/environment.yml", "env/conda-linux-64.lock", "R/prepare_data.R", "R/event_study.R",
                 "R/estimate_did.R", "R/extended_analysis.R", "R/bootstrap_did.R", "scripts/run_analysis.R",
                 "report/paper.qmd", "report/appendix.qmd", "report/references.bib",
                 "data/derived/cps.rds", "results/run-metadata.rds", "results/session-info.txt"):
        put(name)
    source = put("data/raw/originals/42/fixture.sas7bdat")
    item = {"id": 42, "path": "originals/42/fixture.sas7bdat", "size": source.stat().st_size,
            "sha256": f.digest(source), "checksum": {"type": "MD5", "value": f.digest(source, "md5")}}
    source_url = "https://github.com/Frankyin-jpg/Stat-256-Final-Project/releases/download/replication-data-v1/data_and_paper.zip"
    manifest = {"doi": "doi:10.7910/DVN/WCNKB1", "complete": True, "files": [item],
                "source_type": "github_release_mirror", "source_url": source_url,
                "original_programs_available": False,
                "completeness_scope": "Supplied archive only; not a complete original deposit",
                "doi_status": "Historical repository reference; mirror/deposit identity unverified"}
    put("data/raw/manifest.json", json.dumps(manifest))
    put("data/raw/catalog.json", json.dumps({"fixture": True,
        "source_type": "github_release_mirror", "source_url": source_url,
        "referenced_dataset_doi": manifest["doi"], "scope": "Supplied archive without original programs"}))
    put("data/source-verification.json", json.dumps({
        "input_md5": f.digest(source, "md5"), "raw_manifest_sha256": f.digest(root / "data/raw/manifest.json"),
        "input_sha256": f.digest(source), "input_path": str(source.relative_to(root)),
        "mapping_status": f.MAPPING_STATUS, "data_kind": "empirical", "fixture_only": True,
        "original_programs_available": False,
        "verification_basis": {"source_labels": "TEST fixture label audit",
            "publication_timing": "TEST fixture publication audit", "weights": "TEST fixture weight audit",
            "limitations": ["No original SAS programs are present in this test fixture."]}}))
    run = {"mapping_status": f.MAPPING_STATUS, "data_kind": "empirical", "extension_failures": 0,
           "input_md5": f.digest(source, "md5"), "source_file": str(source), "run_utc": "fixture-only",
           "git_commit": "fixture-only", "replication_status": "benchmarks_verified", "code_md5": {"R/prepare_data.R": f.digest(root / "R/prepare_data.R", "md5")}}
    evidence = {"run": run, "canonical": copy.deepcopy(run), "canonical_rows": 10}
    methods = ("unadjusted_did", "aipw_standardized_did", "outcome_standardized_did", "ipw_standardized_did")
    strata = list(itertools.product(("male", "female"), ("young", "old")))
    base = [dict(sex=sex, age_sample=age, outcome=outcome, method=method)
            for sex, age in strata for outcome in ("weeks", "worked_any") for method in methods]
    for name in f.REQUIRED_RESULTS:
        csv_file(root / "results" / name, [{"fixture_only": "TRUE"}])
    csv_file(root / "results/placebo.csv", [{**x, "estimate": 0} for x in base])
    csv_file(root / "results/extension.csv", [{**x, "window": window, "estimate": 0} for x in base for window in ("main", "later_coverage", "broader_pre")])
    csv_file(root / "results/extension_rich.csv", [{**x, "covariate_set": cov, "estimate": 0} for x in base for cov in ("primary_same_sample", "rich")])
    csv_file(root / "results/distributional.csv", [dict(sex=sex, age_sample=age, outcome="cdf_weeks", method=method, threshold=threshold, estimate=0)
        for sex, age in strata for method in methods for threshold in (0, 13, 26, 39, 51)])
    diag = [dict(analysis="placebo", sex=sex, age_sample=age, outcome=outcome, covariate_set="primary", threshold="")
            for sex, age in strata for outcome in ("weeks", "worked_any")]
    diag += [dict(analysis="rich", sex=sex, age_sample=age, outcome=outcome, covariate_set=cov, threshold="")
             for sex, age in strata for outcome in ("weeks", "worked_any") for cov in ("primary_same_sample", "rich")]
    diag += [dict(analysis="distribution", sex=sex, age_sample=age, outcome="cdf_weeks", covariate_set="primary", threshold=threshold)
             for sex, age in strata for threshold in (0, 13, 26, 39, 51)]
    csv_file(root / "results/extended_diagnostics.csv", diag)
    csv_file(root / "results/bootstrap.csv", [{**x, "estimate": 0, "n_success": 199, "n_requested": 199,
        "conf_low": -1, "conf_high": 1, "intervals_available": "TRUE"} for x in base])
    csv_file(root / "results/bootstrap_replicates.csv", [{**x, "replicate": b, "success": "TRUE", "estimate": 0}
        for x in base for b in range(1, 200)])
    csv_file(root / "results/bootstrap_status.csv", [dict(sex=sex, age_sample=age, outcome=outcome, replicate=b, success="TRUE")
        for sex, age in strata for outcome in ("weeks", "worked_any") for b in range(1, 200)])
    csv_file(root / "results/replication_benchmarks.csv", [dict(benchmark_id="test-fixture-only", target=1, original_value=1, reproduced_value=1, tolerance=.001, passed="TRUE")])
    put("results/tables/test-fixture-only.md")
    csv_file(root / "results/extension_failures.csv", [], fields=["reason"])
    csv_file(root / "results/exclusions.csv", [], fields=["reason", "n"])
    for name in f.DOCUMENTS:
        put(name, "%PDF-TEST FIXTURE ONLY" if name.endswith(".pdf") else "<html>TEST FIXTURE ONLY</html>")
    return evidence


class PackageTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.evidence = fixture(self.root)

    def tearDown(self):
        self.temp.cleanup()

    def build(self):
        return f.build_manifest(self.root, evidence_loader=lambda root: self.evidence)

    def refresh_documents(self):
        for name in f.DOCUMENTS:
            (self.root / name).touch()

    def set_replication_status(self, status):
        for key in ("run", "canonical"):
            self.evidence[key]["replication_status"] = status

    def test_complete_fixture_inventory_is_portable(self):
        manifest = self.build()
        self.assertEqual(manifest["provenance"]["input_path"], "data/raw/originals/42/fixture.sas7bdat")
        names = {item["path"] for item in manifest["files"]}
        self.assertIn("manuscript/paper.pdf", names)
        self.assertIn("appendix/appendix.html", names)
        self.assertIn("data/derived/cps.rds", names)
        self.assertIn("results/bootstrap_replicates.csv", names)
        self.assertFalse(any(Path(x).is_absolute() for x in names))
        self.assertEqual(manifest["source"]["source_type"], "github_release_mirror")
        self.assertFalse(manifest["source"]["original_programs_available"])
        self.assertNotIn("dataset_doi", manifest)
        self.assertEqual(manifest["replication_comparison"]["discrepancies"], 0)

    def test_incomplete_acquisition_rejected(self):
        path = self.root / "data/raw/manifest.json"
        value = json.loads(path.read_text()); value["complete"] = False
        path.write_text(json.dumps(value))
        with self.assertRaisesRegex(f.ReleaseError, "incomplete"):
            self.build()

    def test_provisional_or_fixture_metadata_rejected(self):
        self.evidence["run"]["mapping_status"] = "provisional"
        with self.assertRaisesRegex(f.ReleaseError, "not been verified"):
            self.build()
        self.evidence["run"]["mapping_status"] = f.MAPPING_STATUS
        self.evidence["run"]["data_kind"] = "synthetic"
        with self.assertRaisesRegex(f.ReleaseError, "fixtures cannot"):
            self.build()

    def test_changed_original_or_code_rejected(self):
        path = self.root / "data/raw/originals/42/fixture.sas7bdat"
        original = path.read_bytes(); path.write_bytes(b"corrupt")
        with self.assertRaisesRegex(f.ReleaseError, "changed or corrupt"):
            self.build()
        path.write_bytes(original)
        (self.root / "R/prepare_data.R").write_text("changed code")
        with self.assertRaisesRegex(f.ReleaseError, "source changed"):
            self.build()

    def test_missing_results_and_nonempty_failures_rejected(self):
        (self.root / "results/placebo.csv").unlink()
        with self.assertRaisesRegex(f.ReleaseError, "placebo.csv"):
            self.build()
        self.evidence = fixture(self.root)
        csv_file(self.root / "results/extension_failures.csv", [{"reason": "positivity failure"}])
        with self.assertRaisesRegex(f.ReleaseError, "failure table is nonempty"):
            self.build()

    def test_duplicate_prespecified_grid_rejected(self):
        path = self.root / "results/placebo.csv"
        with path.open() as stream:
            rows = list(csv.DictReader(stream))
        rows[-1] = rows[0]
        csv_file(path, rows)
        with self.assertRaisesRegex(f.ReleaseError, "prespecified result grid"):
            self.build()

    def test_unavailable_bootstrap_intervals_rejected(self):
        path = self.root / "results/bootstrap.csv"
        with path.open() as stream:
            rows = list(csv.DictReader(stream))
        rows[0]["intervals_available"] = "FALSE"
        csv_file(path, rows)
        with self.assertRaisesRegex(f.ReleaseError, "successful replications"):
            self.build()

    def test_unverified_benchmarks_rejected(self):
        self.evidence["run"]["replication_status"] = "pending"
        with self.assertRaisesRegex(f.ReleaseError, "benchmarks are unrun"):
            self.build()
        self.evidence["run"]["replication_status"] = "benchmarks_verified"
        csv_file(self.root / "results/replication_benchmarks.csv", [dict(target="fixture", original_value=1, reproduced_value=2, tolerance=.1, passed="TRUE")])
        with self.assertRaisesRegex(f.ReleaseError, "passed flag disagrees"):
            self.build()

    def test_honestly_reported_discrepancy_can_be_packaged(self):
        rows = [dict(benchmark_id="matched", target=1, original_value=1, reproduced_value=1, tolerance=.1, passed="TRUE"),
                dict(benchmark_id="different", target=2, original_value=2, reproduced_value=2.5, tolerance=.1, passed="FALSE")]
        csv_file(self.root / "results/replication_benchmarks.csv", rows)
        self.set_replication_status("reconstruction_discrepancies_reported")
        audit = self.root / "docs/replication-audit.md"
        audit.parent.mkdir()
        audit.write_text("TEST FIXTURE: different differs by 0.5 and is retained as a discrepancy.\n")
        self.refresh_documents()
        manifest = self.build()
        comparison = manifest["replication_comparison"]
        self.assertEqual((comparison["total"], comparison["passed"], comparison["discrepancies"], comparison["unrun"]), (2, 1, 1, 0))
        self.assertEqual(comparison["discrepant_benchmark_ids"], ["different"])
        self.assertIn("does not claim a perfect replication", comparison["note"])
        self.assertIn("docs/replication-audit.md", {x["path"] for x in manifest["files"]})
        f.write_release(self.root, self.root / "discrepancy-bundle", manifest)
        self.assertTrue((self.root / "discrepancy-bundle/docs/replication-audit.md").is_file())

    def test_discrepancy_requires_consistent_status_and_audit(self):
        csv_file(self.root / "results/replication_benchmarks.csv", [dict(original_value=1, reproduced_value=2, tolerance=.1, passed="FALSE")])
        with self.assertRaisesRegex(f.ReleaseError, "status disagrees with 1 benchmark"):
            self.build()
        self.set_replication_status("reconstruction_discrepancies_reported")
        with self.assertRaisesRegex(f.ReleaseError, "docs/replication-audit.md"):
            self.build()

    def test_false_flag_on_passing_comparison_is_rejected(self):
        csv_file(self.root / "results/replication_benchmarks.csv", [dict(original_value=1, reproduced_value=1, tolerance=.1, passed="FALSE")])
        with self.assertRaisesRegex(f.ReleaseError, "passed flag disagrees"):
            self.build()

    def test_unrun_nonfinite_negative_tolerance_and_empty_benchmarks_rejected(self):
        base = dict(original_value=1, reproduced_value=1, tolerance=.1, passed="TRUE")
        for field, value in (("reproduced_value", ""), ("reproduced_value", "NA"),
                             ("original_value", "nan"), ("reproduced_value", "inf"), ("tolerance", -1)):
            with self.subTest(field=field, value=value):
                csv_file(self.root / "results/replication_benchmarks.csv", [{**base, field: value}])
                with self.assertRaisesRegex(f.ReleaseError, "Unrun"):
                    self.build()
        csv_file(self.root / "results/replication_benchmarks.csv", [], fields=list(base))
        with self.assertRaisesRegex(f.ReleaseError, "contains no rows"):
            self.build()

    def test_replication_metadata_counts_must_agree_if_supplied(self):
        self.evidence["run"]["replication_benchmarks_failed"] = 1
        with self.assertRaisesRegex(f.ReleaseError, "replication_benchmarks_failed disagrees"):
            self.build()

    def test_source_catalog_and_audit_contracts(self):
        path = self.root / "data/raw/catalog.json"
        value = json.loads(path.read_text())
        value["source_type"] = "dataverse"
        path.write_text(json.dumps(value))
        with self.assertRaisesRegex(f.ReleaseError, "catalog and acquisition manifest disagree"):
            self.build()
        self.evidence = fixture(self.root)
        path = self.root / "data/source-verification.json"
        value = json.loads(path.read_text())
        value["original_programs_available"] = True
        path.write_text(json.dumps(value))
        with self.assertRaisesRegex(f.ReleaseError, "original SAS programs are unavailable"):
            self.build()
        value["original_programs_available"] = False
        value["verification_basis"]["limitations"] = ["Only sample counts checked."]
        path.write_text(json.dumps(value))
        with self.assertRaisesRegex(f.ReleaseError, "explicitly disclose missing original programs"):
            self.build()

    def test_source_audit_is_bound_to_input_and_manifest(self):
        path = self.root / "data/source-verification.json"
        baseline = json.loads(path.read_text())
        for field in ("input_sha256", "raw_manifest_sha256"):
            with self.subTest(field=field):
                path.write_text(json.dumps({**baseline, field: "incorrect"}))
                with self.assertRaisesRegex(f.ReleaseError, "does not match"):
                    self.build()

    def test_json_provenance_must_be_an_object(self):
        (self.root / "data/raw/catalog.json").write_text("[]")
        with self.assertRaisesRegex(f.ReleaseError, "JSON provenance must be an object"):
            self.build()

    def test_stale_documents_rejected(self):
        path = self.root / "_output/report/paper.pdf"
        os.utime(path, (1, 1))
        with self.assertRaisesRegex(f.ReleaseError, "predates empirical outputs"):
            self.build()

    def test_copy_and_manifest_only_are_distinct(self):
        manifest = self.build()
        f.write_release(self.root, self.root / "inventory", manifest, manifest_only=True)
        self.assertEqual(list((self.root / "inventory").iterdir()), [self.root / "inventory/manifest.json"])
        self.assertFalse(json.loads((self.root / "inventory/manifest.json").read_text())["materialized"])
        f.write_release(self.root, self.root / "bundle", manifest)
        self.assertTrue((self.root / "bundle/manuscript/paper.pdf").is_file())
        self.assertTrue(json.loads((self.root / "bundle/manifest.json").read_text())["materialized"])
        with self.assertRaisesRegex(f.ReleaseError, "already exists"):
            f.write_release(self.root, self.root / "bundle", manifest)

    def test_large_files_reported_not_dropped(self):
        with patch.object(f, "LFS_THRESHOLD", 1):
            manifest = self.build()
        self.assertIn("data/derived/cps.rds", manifest["git_lfs_required"])
        self.assertIn("data/derived/cps.rds", {item["path"] for item in manifest["files"]})


if __name__ == "__main__":
    unittest.main()
