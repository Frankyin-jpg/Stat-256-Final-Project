.PHONY: test fetch analyze analyze-full report prepare-release all reproduce
.NOTPARALLEL:

test:
	python3 -m unittest discover -s tests -p 'test_*.py' -v
	Rscript tests/test-data.R
	Rscript tests/test-estimate-did.R
	Rscript tests/test-event-study.R
	Rscript tests/test-replication.R
	Rscript tests/test-extended-analysis.R
	Rscript tests/test-bootstrap-did.R
	Rscript tests/test-pipeline.R

fetch:
	python3 scripts/fetch_release.py

analyze:
	Rscript scripts/run_analysis.R $(if $(RAW_DATA),"$(RAW_DATA)",)

BOOTSTRAP ?= 199
SEED ?= 256
WORKERS ?= 2
analyze-full:
	Rscript scripts/run_analysis.R $(if $(RAW_DATA),"$(RAW_DATA)",) --bootstrap $(BOOTSTRAP) --seed $(SEED) --workers $(WORKERS)

report:
	quarto render --to all

prepare-release:
	python3 scripts/package_release.py

all: fetch analyze report

reproduce: fetch analyze-full report prepare-release
