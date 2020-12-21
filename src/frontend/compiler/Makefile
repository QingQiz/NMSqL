.PHONY: clean test purge

test:
	@stack test
	@make -C src/Test/FFI


libCompiler.so: prebuild
	@stack install --local-bin-path . &>/dev/null


prebuild:
	@stack build


clean:
	make -C src/Test/FFI clean


purge: clean
	stack clean --full