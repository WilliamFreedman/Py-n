# Py+^n

## Final Checkpoint

The link to our video is available here: https://youtu.be/nCTdXWVzS74

To run a Pyn file, inside the src/parsing directory, run

```
./pyn_runner.sh <pyn_file_path>
```

This will run the preprocessor and compilation pipeline and will run the resulting file.

To run the test suite, inside the src/parsing directory, run

```
python3 tester.py
```

## Hello World Front-End Checkpoint Update

So far, we have completed the scanner and the parser, with a functioning AST pretty printer.

The preprocessor works to manage macros and insert INDENT/DEDENT tokens, but there is a bug with 
how those tokens interact with newlines in our grammar which we need to look into. We think this is
due to the order in which newline and DEDENT tokens are added, but we need to spend more time on it to
be sure.

We also have included a few test files that demonstrate important features of our language.

Instructions for use: Move the test file to src/preprocessing/input_file. In the root directory, compile
and run main.ml with the name of the test file as a command line argument. Since the preprocessor doesn't 
correctly handle indentation yet, testing is currently done by building and running the scanner and inputting 
the program into stdin. This also requires manual insertion of INDENT and DEDENT tokens.

We also have included a few test files that demonstrate important features of our language.

In the future, semantics should be implemented along with any back-end functionality that would be required.
