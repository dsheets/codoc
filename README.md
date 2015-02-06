# codoc

**codoc** is a new OCaml documentation tool. It does not impose any
  specific package manager on you or assume a specific
  workflow. **codoc** requires **OCaml 4.02.1+doc** switch from the
  [OCaml Platform development repository](https://github.com/ocaml/platform-dev).

If you'd like to generate cross-referenced documentation for all
packages on an opam switch, see the [**opam doc**
Script](https://github.com/dsheets/codoc/tree/release-0.2.0#an-opam-doc-script)
section of this README.

## Quick Start

To generate HTML documentation from a build directory, simply run:

```sh
codoc doc [CMTI_DIR] -o [OUTPUT_DIR]
```

**codoc** will recursively search for *.cmti* files in *CMTI_DIR* and
  use their relative paths to construct a parallel directory hierarchy
  in *OUTPUT_DIR*. Of course, you can also run **codoc** on individual
  files and *OUTPUT_DIR* does not need to exist.

**codoc** will refuse to overwrite already extracted or rendered
  documentation without the **-f** flag.

**codoc** understands package hierarchies. To output documentation for a
  specific package, simply pass **--package** followed by a
  *slash/separated/package/path*. The *OUTPUT_DIR* will be the base of
  the hierarchy and the documentation inside will cross-reference.

**codoc doc** has some other options that are described below in the
  section about the **html** subcommand.

## Scriptable Command-line

The **doc** subcommand of **codoc** is actually a composition of 3
different subcommands: **extract**, **link**, and **html**. Each of
these commands can take the **-f** and **--index** arguments.

Documentation indexes can be generated with the **--index**
flag. **--index** can optionally take an argument but this is a bad idea
and [will be removed](https://github.com/dsheets/codoc/issues/43) in the
future. You should always put **--index** last on the command line.

The **html** subcommand takes extra options, **--css**, **--scheme**,
and **--share**. It does not yet support **--package**, though, [support
is planned](https://github.com/dsheets/codoc/issues/42). Of these,
**--scheme file** is the most useful for browsing documentation in a web
browser directly off of the disk.

## An **opam doc** Script

Before using **opam doc**:

1. Switch into an empty **4.02.01+doc** switch with **opam switch
4.02.1+doc** or **opam switch 4.02.1+doc -A doc**.
2. **export OPAMKEEPBUILDIR=1**
3. **export OCAMLPARAM=_,doc=1,w=-50,bin-annot=1**
4. Install your package set.

**codoc** supplies an **opam doc** script as well as the stand-alone
  **codoc** tool. This script uses **codoc**'s scriptable CLI and and
  also offers an easy-to-use **--serve** option for immediately starting
  a web server for your opam installation's documentation. Using **opam doc**
  is as easy as:

```sh
opam doc my-opam-docs/ --serve
```

## Interfaces

The toolchain that **codoc** realizes was designed to have many
interfaces to subcomponents.

[doc-ock-lib](https://github.com/lpw25/doc-ock-lib) parses OCaml
interfaces, exposes types for signatures and identifiers, and defines
resolution procedures.

[doc-ock-xml](https://github.com/lpw25/doc-ock-xml/) defines XML
serialization and parsing for the types in **doc-ock-lib**. This XML
format will eventually provide mechanism for extensible documentation
content.

**codoc** defines a documentation index XML format for tracking package
  hierarchies, documentation issues, and localized
  configuration. **codoc** also defines a scriptable command-line
  interface.

Most of these interfaces are not stable. The basics of **codoc**'s CLI,
though, are likely to remain consistent.

## Contributions

Contributions are very welcome! Please feel free to report issues,
ideas, comments, or concerns to the [codoc issue
tracker](https://github.com/dsheets/codoc/issues).

## Documentation Feature Tests

See
[dsheets/ocamlary-test-library](https://github.com/dsheets/ocamlary-test-library)
for some documentation feature tests.
