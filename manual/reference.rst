Language Reference
==================

The **glucose** language is presently trivial and only supports the global
declaration of 32-bit integer and 64-bit floating-point constants,
enumeration data types and non-recursive, single-argument functions.

A **glucose** source file consists of zero or more definitions, each starting
at the beginning of a line, optionally seperated by white-space. The order
of definitions is not significant.

Source files are UTF-8 encoded and it is the intention that any
alphanumeric character in any script may appear in variable names. Operators
consist of sequences of the charactes ``~!@#$%^&|*-+=<>.?:`` and this list is
likely to expand.

All newline characters as well as paragraph separators and form feeds are
treated as newlines and all Unicode white-space characters are permitted
and treated as such.

Identifiers
-----------

Identifiers consist of alphanumeric characters, marks, and punctuation
considered connectors such as underscores, but may not begin with an
ASCII digit.

The LLVM code-generator mangles identifiers, with unsupported characters
along with dollar signs being replaced with their Unicode code-point in
hexadecimal surrounded by dollar signs e.g. Hebrew aleph becomes $5d0$.

Variable Definitions
--------------------

Variable definitions bind a numeric constant to an identifier as either a
32-bit integer or a 64-bit floating-point value, either directly or based
on the value of another constant.

A variable definition must be positioned at the beginning of a line and
consist of an identifier, equals character and either a numeric literal
(e.g. ``a = 1.23e4``) or identifier (e.g. ``a = b``).

Integer literals may be prefixed with redundent zeros and may have positive
integer exponents (themselves potentially prefixed with zeros) e.g. ``012e03``
(12000).

Floating-point literals are distinguished from integer literals by the presence
of a single decimal point followed by one or more digits that preceed any
exponent and may have negative exponents e.g. ``01.2e-03`` (0.0012).

Enum Types
----------

An enumeration type definition defines a set of distinct constructors that
share a user-defined type.

A type definition must be positioned at the beginning of a line and consist
of the keyword ``type``, a type identifier, an equals character, and one or
more constructor identifiers separated by vertical bars e.g.
``type Colour = Red | Green | Blue``.

Constructors and variables share a single namespace and cannot have the same
name. Type names occupy their own namespace.

Function Definitions
--------------------

Function definitions bind a single-argument lambda expression to an identifier.

A function definition must be positioned at the beginning of a line and
consist of an identifier, equals character and a single-argument lambda
expression consisting of a ``\``, an argument identifier, a ``->`` and either
a literal, a reference (to the argument, a global variable or an enum type
constructor), or function application.

Function application consists of an identifier referencing a function
(an argument or function definition) and a value separated by white-space,
where the value can be either a literal, a reference or enum type constructor.

Function application can be chained and is left-associative i.e. ``f a b``
applies the argument ``a`` to ``f``, then applies the argument ``b`` to the
result.

Example function definitions include:
 - ``three = \a -> 3``
 - ``id = \a -> a``
 - ``applyRed = \f -> f Red``
