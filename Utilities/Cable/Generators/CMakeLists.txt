LIBRARY(CableGenerators)

SOURCE_FILES(
genCxxGenerator
)

INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Parsers
)

LINK_LIBRARIES(
Expat
CableParsers
CableGenerators
)

EXECUTABLES(generateWrappers)

