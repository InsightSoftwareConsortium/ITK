INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Parsers
)

ADD_LIBRARY(CableGenerators STATIC
genGeneratorBase
genTclGenerator
genCvTypeGenerator
)

LINK_LIBRARIES(CableParsers CxxTypes Expat)
