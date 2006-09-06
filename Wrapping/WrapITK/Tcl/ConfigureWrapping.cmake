FIND_PACKAGE(TCL)
# Hide useless settings provided by FindTCL.
FOREACH(entry TCL_LIBRARY_DEBUG
              TK_LIBRARY_DEBUG
              TCL_STUB_LIBRARY
              TCL_STUB_LIBRARY_DEBUG
              TK_STUB_LIBRARY
              TK_STUB_LIBRARY_DEBUG
              TK_WISH)
  SET(${entry} "${${entry}}" CACHE INTERNAL "This value is not used by ITK.")
ENDFOREACH(entry)

INCLUDE_DIRECTORIES(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
