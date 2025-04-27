.DEFAULT_GOAL := svcomp

svcomp:
	pip install bench/
	dune b && dune install
	rm -rf results/*
	systemd-run --user --scope --slice=benchexec -p Delegate=yes benchexec \
		bench/slplugin.xml --numOfThreads 8

verifit:
	rm -rf results/*
	scp -r verifit:slplugin/results .
	$(MAKE) result

result:
	table-generator results/*.xml.bz2
	python3 -m http.server -b 127.0.0.1 8000 -d /home/tb/projects | \
	firefox 'localhost:8000/slplugin/results'

ALLARGS := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
ARGS_WITH_SUFFIX := $(addsuffix /*.xml.bz2, $(ALLARGS))

result-diff:
	rm -rf results-diff/*
	table-generator $(ARGS_WITH_SUFFIX) -o results-diff
	python3 -m http.server -b 127.0.0.1 8000 -d /home/tb/projects | \
	firefox 'localhost:8000/slplugin/results-diff'

FILE := $(word 2, $(MAKECMDGOALS))
ARGS := $(wordlist 3, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))

run:
	dune b && dune install
	ivette -scf -ulevel=3 $(FILE) -then-replace \
	-sl -sl-msg-key '*' -sl-benchmark-mode -sl-astral-encoding Bitvectors -sl-backend-solver Bitwuzla -sl-edge-deduplication -sl-simple-join $(ARGS)

run-direct:
	dune b && dune install
	ivette -sl -sl-msg-key '*' -sl-benchmark-mode -sl-astral-encoding Bitvectors -sl-backend-solver Bitwuzla -sl-edge-deduplication -sl-simple-join $(ARGS) $(FILE)

%:
	@:

poster:
	typst compile excel_poster/main.typ excel_poster/poster.pdf

EXCEL_ZIP_NAME := xbrabl04_Static_Analysis_of_Heap-Manipulating_Programs_using_Separation_Logic.zip

excel: poster
	rm -rf $(EXCEL_ZIP_NAME)
	zip -j $(EXCEL_ZIP_NAME) excel_poster/poster.pdf excel_abstract/abstrakt.pdf excel_poster/nahled.png

