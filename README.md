[![Build Status](https://travis-ci.org/yav/graphmod.svg?branch=master)](https://travis-ci.org/yav/graphmod)

# graphmod

Generate a graph of the module dependencies in the "dot" format, suitable as input to the graphviz tools.

Similar to `ghc-pkg dot`, but on modules instead of packages.

See the [wiki](https://github.com/yav/graphmod/wiki) for more documentation and examples.

## Simple quickstart for stack users

    $ stack build --copy-compiler-tool graphmod
    $ stack exec graphmod -- --help
    $ stack exec graphmod | tred | dot -Tpdf > modules.pdf
