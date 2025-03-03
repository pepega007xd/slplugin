svcomp:
	pip install bench/
	dune b && dune install
	rm -rf results/*
	systemd-run --user --scope --slice=benchexec -p Delegate=yes benchexec \
		bench/slplugin.xml --numOfThreads 8

result:
	table-generator results/*.xml.bz2
	python3 -m http.server -b 127.0.0.1 8000 -d /home/tb/projects | \
	firefox 'localhost:8000/slplugin/results'
