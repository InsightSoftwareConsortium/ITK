SOURCE_FILES(CableGenerators_SRCS
genCxxGenerator
genGeneratorBase
genTclGenerator
)

INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Parsers
)

ADD_LIBRARY(CableGenerators CableGenerators_SRCS)

