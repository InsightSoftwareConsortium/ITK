INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Parsers
)

ADD_LIBRARY(CableGenerators
genGeneratorBase
genTclGenerator
genCvTypeGenerator
)

