# running the project

the `g` script is intended to launch the process to output audio

# running the scheme code

start guile with the `scm` directory on path to use this project

```
guile -L scm ...
```

for a repl, `guile -L scm`, to run the score generator `guile -L scm mksco.scm`

# running the tests
```
./run-tests
```

this finds all tests under scm/test and runs them, output ends up in the `test-logs` directory
