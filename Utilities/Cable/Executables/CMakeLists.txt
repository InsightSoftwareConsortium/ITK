INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Parsers
${CABLE_SOURCE_DIR}/Generators
)

LINK_LIBRARIES(
CxxTypes
Expat
CableParsers
CableGenerators
)

EXECUTABLES(cable)
