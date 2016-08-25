Synopsis
^^^^^^^^

| glucose [file]
| glucose -?
| glucose -V
| glucose --numeric-version

Description
^^^^^^^^^^^

**glucose** compiles a single glucose source file to LLVM IR.

If a source file path is specified, the output is written to the same path with a '.ll' extension.

If no path is specified, **glucose** compiles standard input to standard output.

Options
^^^^^^^

.. program:: glucose

.. option:: -? --help

   Displays a summary of the command and exits.

.. option:: -V --version

   Displays version information and exits.

.. option:: --numeric-version

   Displays a machine-readable numeric version.
