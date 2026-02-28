.PHONY: all doc build check install test clean

all: doc build check

doc:
	Rscript -e "roxygen2::roxygenise()"

build: doc
	R CMD build .

check: build
	R CMD check nashR_*.tar.gz

install:
	R CMD INSTALL .

test:
	Rscript -e "library(nashR); testthat::test_dir('tests/testthat')"

clean:
	rm -f src/*.o src/*.so src/*.dll
	rm -f nashR_*.tar.gz
	rm -rf nashR.Rcheck
