INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/CxxTypes
)

ADD_LIBRARY(CableParsers STATIC
cableConfigurationRepresentation
cableConfigurationParser
cableSourceParser
cableSourceRepresentation
cableXmlAttributes
)

ADD_DEFINITIONS(-D_cxx_STATIC)

