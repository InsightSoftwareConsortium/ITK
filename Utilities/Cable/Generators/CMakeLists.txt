SOURCE_FILES(CableGenerators_SRCS
genCxxGenerator
genGeneratorBase
genTclGenerator
genCvTypeGenerator
)

INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Parsers
)

ADD_LIBRARY(CableGenerators CableGenerators_SRCS)

