INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Parsers
${CABLE_SOURCE_DIR}/Generators
)

LINK_LIBRARIES(
Expat
CableParsers
CableGenerators
)

EXECUTABLES(cable)
