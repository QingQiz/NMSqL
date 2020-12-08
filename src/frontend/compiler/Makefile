.PHONY: clean test

test:
	@stack test
	@make -C src/Test/FFI


libCompiler.so: prebuild
	@stack install --local-bin-path . &>/dev/null


prebuild:
	@stack build


clean:
	stack clean --full
	make -C src/Test/FFI clean