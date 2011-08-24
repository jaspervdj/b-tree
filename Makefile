HS_SOURCE_DIRS=-ibenchmarks -isrc -itests

all: benchmarks/BenchmarkSuite tests/TestSuite

benchmarks/BenchmarkSuite:
	ghc --make -O2 ${HS_SOURCE_DIRS} -main-is BenchmarkSuite $@.hs

tests/TestSuite:
	ghc --make -O2 ${HS_SOURCE_DIRS} -main-is TestSuite $@.hs

test.core.hs: test.hs
	ghc --make -O2 ${HS_SOURCE_DIRS} -ddump-simpl test.hs >test.core.hs 2>&1

clean:
	-rm benchmarks/BenchmarkSuite tests/TestSuite
