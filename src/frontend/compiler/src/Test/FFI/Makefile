.PHONY: clean prebuild test

test: ffi-export.test.out ffi-import.test.out
	@echo -e "compiler> Test test-ffi-import"
	@./ffi-import.test.out || (echo -e "\ncompiler> Test test-ffi-import failed" && exit 1)
	@echo "compiler> Test test-ffi-import pass"
	@echo -e "\ncompiler> Test test-ffi-export"
	@(export LD_LIBRARY_PATH=`pwd` && ./ffi-export.test.out &> /dev/null) || (echo -e "\ncompiler> Test test-ffi-export failed" && exit 1)
	@echo "compiler> Test test-ffi-export pass"


ffi-export.test.out: FFIExport.o FFIImport.o libCompiler.so
	@stack ghc -- -optc "-m64" -dynamic -no-hs-main FFIExport.o FFIImport.o -L. -lCompiler -o $@


libCompiler.so: prebuild
	@stack install --local-bin-path . &>/dev/null

ffi-import.test.out: FFIImport.o Main.hs
	@stack build
	@stack ghc -- $^ -o $@

FFIExport.o: FFIExport.c
	@gcc -c $< -o $@

FFIImport.o: FFIImport.c
	@gcc -c $< -o $@

prebuild:
	@stack build

clean:
	rm -f *.o *.hi *.out ../../../a.out *.so
