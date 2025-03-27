ARGS := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))

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

run:
	dune b && dune install
	ivette -ulevel=3 -scf -sl -sl-msg-key '*' -sl-benchmark-mode -sl-astral-encoding Bitvectors -sl-backend-solver Bitwuzla -sl-edge-deduplication -sl-simple-join $(ARGS)

%:
	@:
