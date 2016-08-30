Language Reference
==================

The **glucose** language is presently trivial and only supports the global
declaration of 32-bit integer and 64-bit floating-point constants.

A **glucose** source file consists of zero or more definitions, each starting
at the beginning of a line, optionally seperated by white-space.

Source files are UTF-8 encoded and it is the intention that any
alphanumeric character in any script may appear in variable names. Operators
consist of sequences of the charactes ``~!@#$%^&*-+=<>.?:`` and this list is
likely to expand.

All newline characters as well as paragraph separators and form feeds are
treated as newlines and all Unicode white-space characters are permitted
and treated as such.

Identifiers
-----------

Identifiers consist of alphanumeric characters, marks, and punctuation
considered connectors such as underscores, but may not begin with an
ASCII digit.

Identifiers are mangled to support LLVM IR with unsupported characters
along with dollar signs being replaced with their Unicode code-point in
hexadecimal surrounded by dollar signs e.g. hebrew aleph becomes $5d0$.

Definitions
----------

Definitions bind a numeric constant to an identifier as either a 32-bit
integer or a 64-bit floating-point value.

A definition must be positioned at the beginning of a line and consist of
an identifier, equals character and a numeric literal e.g.
``identifier = 1.23e4``

Integer literals may be prefixed with redundent zeros and may have positive
integer exponents (themselves potentially prefixed with zeros) e.g. ``012e03``
(12000).

Floating-point literals are distinguished from integer literals by the presence
of a single decimal point followed by one or more digits that preceed any
exponent and may have negative exponents e.g. ``01.2e-03`` (0.0012).
