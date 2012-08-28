find_package(TCL)
# Hide useless settings provided by FindTCL.
foreach(entry TCL_LIBRARY_DEBUG
              TK_LIBRARY_DEBUG
              TCL_STUB_LIBRARY
              TCL_STUB_LIBRARY_DEBUG
              TK_STUB_LIBRARY
              TK_STUB_LIBRARY_DEBUG
              TK_WISH)
  set(${entry} "${${entry}}" CACHE INTERNAL "This value is not used by ITK.")
endforeach()

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
