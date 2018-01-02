* Version 1.4.1
  - Support for Cabal 2
  - Updates to build infrastructure
  - Fixes memory leak

* Version 1.3
  - Corrects collapsing logic.
  - Change node coloring in clusters:
    * Clusters are displayed with various shades of gray
    * Nodes in a cluster are all the same color
  - Change to clustering logic:  by default, a module that has the same
    name as a cluster will be rendered inside the cluster.  One can tell
    that the module is different because it will still have the color of
    modules from the cluster "above".  Also, the module has a border to
    empahsize the difference.
    This behavior may be disabled using `--no-module-in-cluster`

* Version 1.2.9
  - Support for Cabal: if we find a cabal file, we add all modules in it
  - Render `{-# SOURCE #-}` imports specially.

* Version 1.2.7
  Correct the prunning logic.

* Version 1.2.6

  Add support for parsing GHC's `.import` files.  These may be produced
  by running GHC with `-ddump-minimal-imports`

* Version 1.2.5

  Add support for pruning the dependecy graph.

* Version 1.2.3

  [Collapse Modules]
  The flag `--collapse-module` (`-C` for short) adds a new mode of collapsing
  multiples nodes into a single one.  This is similar to `--collapse` except
  that the parameter can refer either to a module name, or to a module prefix.
  So, for example, `--collapse-module=A.B` will use a single node for the
  module A.B (if there is one), as well as for any module that starts with
  the prefix A.B (e.g., A.B.C).

  "Collapsed" nodes are represented with a box.

  Collapsed nodes corresponding to modules have a border, while ones which
  correspond to just a prefix do not have a border.


  [Color Schemes]
  The flag `--colors` (`-s` for short) enables users to choose from
  a set of predefined color schemes.


* Version 1.2.2

  [Show Version]
  The flag `--version` (`-v` for short) shows graphmod's version.

