# About


To run tests:

    make test

To run a test file interactively:

    rm -rf sandbox/
    ./makem.sh --sandbox=sandbox --install-deps interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-creation)"


# Known issues
- Within a workspace project directory names need to be unique. This is by
  design, to prevent longer path-based naming.
