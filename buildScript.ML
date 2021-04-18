
PolyDoc.processFiles{
    sourceFiles=[
        "../polyml/basis/ASN1.sml", "../polyml/basis/Signal.sml", "../polyml/basis/SingleAssignment.sml",
        "../polyml/basis/Thread.sml", "../polyml/basis/Universal.ML", "../polyml/basis/UniversalArray.ML",
        "../polyml/basis/Weak.sml"], outputFileOrDir="outputs", parent=NONE,
        styleSheet=SOME "xslstylesheet.xsl"};


PolyDoc.processFiles{
    sourceFiles=[
        "../polyml/basis"], outputFileOrDir="outputs", parent=NONE};
