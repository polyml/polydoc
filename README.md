

# PolyDoc

PolyDoc is a tool to create documentation from ML source files.  Output files are XML documents with a style sheet so they can be viewed with a web browser.

PolyDoc is used to create the documentation for the Poly/ML extensions to the basis library.  For an example see [Thread.xml](https://www.polyml.org/documentation/Reference/Thread.xml).

PolyDoc parses ML source files, which must end in `.ML,` `.sml` or `.sig`, and extracts signatures, structures and functors from the source along with specially formatted comments.  These are normal ML comments that begin with `(*!` or begin with `(**` and end with` **)`.  

The output for each source file is a corresponding XML file with the signatures, structures and functors displayed along with the documentation comments.

The parser in PolyDoc was extracted from the Poly/ML compiler but has no error recovery.  It should therefore only be run on files that already compile correctly.
