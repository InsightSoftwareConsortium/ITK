LIBRARY(CableGenerators)

SOURCE_FILES(
genCxxGenerator
genGeneratorBase
genTclGenerator
)

INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Parsers
)
