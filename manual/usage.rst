Synopsis
^^^^^^^^

| glucose [file]
| glucose -t {ll|js} [file]

Description
^^^^^^^^^^^

**glucose** compiles a single glucose source file to LLVM IR or JavaScript source.

If a source file path is specified, the output is written to the same path with an extension based on
the output type ('.ll' or '.js').

If no path is specified, **glucose** compiles standard input to standard output.

Options
^^^^^^^

.. program:: glucose

.. option:: -t --output-type={ll|js}

   Produce the type of output associated with the given extension: LLVM IR (default) or JavaScript source.

.. option:: -? --help

   Displays a summary of the command and exits.

.. option:: -V --version

   Displays version information and exits.

.. option:: --numeric-version

   Displays a machine-readable numeric version.
