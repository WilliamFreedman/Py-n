#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Usage: $0 <filename>"
    exit 1
fi

filename="$1"

modified_filename="preprocessed.pyn"

ocamlc -o preprocessor str.cma preprocessor.ml

rm preprocessor.cmo preprocessor.cmi

./preprocessor $filename $modified_filename

ocamlbuild -r -pkgs llvm pyn.native

cat $modified_filename | ./pyn.native -l > $modified_filename.ll
lli $modified_filename.ll
# rm $modified_filename.ll
# rm pyn.native