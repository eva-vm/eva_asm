.PHONY: build doc tests

all: build doc tests

build:
	@ echo "[\033[32mBuilding the package...\033[0m]"
	dune build -p eva_asm
	@ echo "[\033[32mDone.\033[0m]\n"

doc:
	@ echo "[\033[32mBuilding the Documentation...\033[0m]"
	dune build @doc
	cp -r ./_build/default/_doc/_html/* ./docs/
	@ echo "[\033[32mDone.\033[0m]\n"

tests:
	@ echo "[\033[32mRunning tests...\033[0m]"
	dune runtest
	@ echo "[\033[32mDone.\033[0m]\n"

clean:
	rm examples/*.evo
