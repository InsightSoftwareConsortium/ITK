SOURCE_FILES(CableParsers_SRCS
cableConfigurationRepresentation
cableConfigurationParser
cableSourceParser
cableSourceRepresentation
cableXmlAttributes
)

ADD_LIBRARY(CableParsers CableParsers_SRCS)

IF (WIN32)
  ADD_DEFINITIONS(-DPARSERS_DLL)
ENDIF (WIN32)
