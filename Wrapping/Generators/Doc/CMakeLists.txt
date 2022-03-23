find_package(Doxygen REQUIRED OPTIONAL_COMPONENTS dot)

###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_DOC_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "doc source dir")
set(ITK_WRAP_DOC_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "doc binary dir")
