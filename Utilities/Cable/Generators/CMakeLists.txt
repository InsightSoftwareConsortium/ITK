INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/CxxTypes
${CABLE_SOURCE_DIR}/Parsers
)

ADD_LIBRARY(CableGenerators STATIC
genGeneratorBase
genTclGenerator
genCvTypeGenerator
)

ADD_DEFINITIONS(-D_cxx_STATIC)

