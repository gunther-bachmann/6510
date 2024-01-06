# test files in current directory (and subdirectories)
test:
    cd {{invocation_directory()}} && raco test -y -t -x -j 8 .

# test all files in the project
test-all:
    raco test -y -t -x -j 8 .

# make all files in the project
make:
    raco make -v -j 8 $(find ${DIRENV_DIR:1}/src -type d -not -name 'compiled' -printf '%p/*.rkt ' | tr '\n' ' ')

# make and test all files in the project
build: make test-all

# clean all build artifacts
clean:
    find "${DIRENV_DIR:1}" \( -name '*.zo' -or -name '*.dep' \) -exec rm {} \;

# complete rebuild of the project
clean-build: clean build

# execute complete tests with coverage
coverage:
    raco cover $(find ${DIRENV_DIR:1} -name "*.rkt" -not \( -wholename '*/example*' -or -wholename '*/compiled*' -or -wholename '*/.direnv*' -or -wholename '*/compiled*' -or -wholename '*/auto*' -or -wholename '*/coverage*' \) -exec dirname {} \; | sort | uniq | tail --lines=+2 | xargs realpath | sed -e 's/$/\/\*\.rkt/' | tr '\n' ' ')

# show collected coverage
show-coverage:
    chromium ${DIRENV_DIR:1}/coverage/index.html &>/dev/null

# collect coverage and show
collect-and-show-coverage: coverage show-coverage

[private]
alias t := test
[private]
alias m := make
[private]
alias tt := test-all
[private]
alias c := clean
[private]
alias b := build
[private]
alias bb := clean-build
[private]
alias cov := coverage
[private]
alias sc := show-coverage
[private]
alias csc := collect-and-show-coverage
