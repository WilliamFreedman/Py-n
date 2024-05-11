#!/bin/bash

ocamlbuild -r -pkgs llvm pyn.native
$1 > ./pyn.native -l > $1.ll
lli $1.ll