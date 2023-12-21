#!/bin/env bash
fstar.exe --cache_checked_modules --include ./src --cache_dir obj Test.fst --dump_module Test --debug Test --debug_level Primops
fstar.exe --silent --include ./src --cmi --odir obj --cache_dir obj Test.fst --codegen krml --extract_module Test --dump_module Test --debug Test --debug_level Extraction --ugly --print_full_names --print_implicits --print_universes --debug_level Primops,NormTop,Unfolding
/home/guido/r/karamel/krml -dast -skip-compilation -tmpdir dist obj/Test.krml
