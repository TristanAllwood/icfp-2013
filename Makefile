SRC=$(wildcard *.hs)

GHC_FLAGS		= -O3 -threaded

all: Competition Train Visualise

%: %.hs $(SRC)
	ghc -main-is $@ $(GHC_FLAGS) --make $< -o $@

.PHONY: all
.SUFFIXES:
