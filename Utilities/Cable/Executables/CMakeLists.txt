INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Parsers
${CABLE_SOURCE_DIR}/Generators
)

LINK_DIRECTORIES (
${CABLE_BINARY_DIR}/CxxTypes/Static
${CABLE_BINARY_DIR}/Expat
${CABLE_BINARY_DIR}/Parsers
${CABLE_BINARY_DIR}/Generators
)

LINK_LIBRARIES(
CxxTypesStatic
Expat
CableParsers
CableGenerators
)

ADD_EXECUTABLE(cable cable)
