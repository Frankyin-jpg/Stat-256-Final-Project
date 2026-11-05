# Source data and provenance

The supplied data are now available from the repository author's
[`replication-data-v1` release](https://github.com/Frankyin-jpg/Stat-256-Final-Project/releases/tag/replication-data-v1).
It contains `marcps_w.sas7bdat` and a checked proof of the Acemoglu–Angrist paper.
**Original SAS programs are not included.** The historical repository also links
to Harvard Dataverse DOI [`10.7910/DVN/WCNKB1`](https://doi.org/10.7910/DVN/WCNKB1),
but the supplied release has not been established to be a complete copy of that
deposit.

From the repository root, reproduce the supplied archive and its extraction with:

```bash
python3 scripts/fetch_release.py
```

The downloader pins the 91,436,493-byte ZIP to SHA-256
`a7479a211d034f8fb9209c03435b9be5742f690924c5880cd1a855d5040bc0e4`.
It retains the ZIP under `data/raw/originals/github-release/`, extracts a working
copy under `data/raw/extracted/github-release/`, and saves `catalog.json` plus
`manifest.json`. The existing archive is reused only after integrity checks.
Downloads use inherited proxy/CA settings with TLS verification enabled; archive
extraction rejects unsafe paths and links. The `complete` flag covers this one
supplied release, not every file in the original Dataverse collection.

The SAS file is roughly 1.14 GB uncompressed. Keep it unchanged. GitHub regular
file limits require Git LFS for the uncompressed SAS data and potentially derived
RDS files. The 91.4 MB source ZIP remains available as a compact reproducible
archive. The local data folder and any final release must retain actual data,
not replace them with generated test fixtures.

For a later independent acquisition of the complete Dataverse catalog, use a
separate directory so that its provenance does not replace the supplied-release
manifest:

```bash
python3 scripts/fetch_dataverse.py --extract --output data/dataverse
```

That host requires the environment's allowed-domain setting. Its API may reveal
additional data/programs or restrictions; the code does not guess credentials,
skip restricted files silently, or claim those files are already available.

See [the source audit](../docs/source-audit.md) for file hashes, observed codes,
calendar definitions, population exclusions and independent sample-size checks.
`data/source-verification.json` binds the audited field mappings to the actual
input and manifest hashes. Its status is
`publication_and_source_labels_verified`; original-program verification is not
claimed. Numerical replication benchmarks remain separate.

`R/prepare_data.R` exposes `read_cps(path)` and `prepare_cps(raw)`, returning:

- `data`: all survey years 1988–1997, ages 21–58, with positive `FNLWGT2` weights;
- `population_counts`: age/group-eligible counts before weight exclusion, needed
  for the original Table 1's unweighted observation counts;
- `exclusions`: sequential population and estimation-weight exclusions;
- `quality`: outcome/covariate missingness and wage-eligibility counts;
- `metadata`: input descriptions and a deliberately unverified default for
  arbitrary input data, upgraded only by hash-bound empirical source evidence.

`read_cps()` selects only the required source columns. `survey_year` identifies
the March interview, while `outcome_year = survey_year - 1` identifies annual
weeks/earnings timing. Missing wages never remove observations from the
weeks-worked analysis. The three-level education and race classifications follow
the audited historical specification; unsupported codes fail explicitly.

The raw file has 971,058 records. Age eligibility yields 759,771 and the weighted
estimation frame has 709,125; Table 1 observation counts and model sample sizes
must therefore not be conflated. Current work status, SSI receipt and Social
Security receipt are retained when available without inventing a disability-
insurance classification from an unpublished threshold.

Software tests use generated records in temporary directories. They test code
behavior only and are never substituted for the supplied data or empirical
results. Source usage terms should be recorded from the actual original source;
the repository's own code license does not automatically license third-party data.
