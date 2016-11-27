## Using opam

    opam install stone

## Manual installation

    ./configure  # optionally: --bin-dir bin_dir
    make
    make install # if --bin-dir was passed to ./configure

This produces a `stone` binary, and installs it to `bin_dir` if
`--bin-dir bin_dir` was passed to `./configure`.
