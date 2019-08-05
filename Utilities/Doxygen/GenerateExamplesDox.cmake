#
# Usage: cmake -D "PROJECT_SOURCE_DIR:PATH=${PROJECT_SOURCE_DIR}"
#              -D "OUTPUT_FILE:PATH=${PROJECT_BINARY_DIR}/Examples.dox"
#              -P GenerateExamplesDox.cmake

# This cmake script gets a lists of examples for the project and
# generates a file suitable for doxygen. The file is a list of
# "\examples filename", where doxygen will search for fileanme in it's
# example path. Doxygen should then generate an examples page with the
# referenced files.

# glob the examples for each language
file( GLOB_RECURSE examples_list_fullpath
  FOLLOW_SYMLINKS
  "${PROJECT_SOURCE_DIR}/Examples/*.cxx"
  "${PROJECT_SOURCE_DIR}/Examples/*.py"
  "${PROJECT_SOURCE_DIR}/Examples/*.java"
)

file( GLOB_RECURSE sphinx_examples_list_fullpath
  "${PROJECT_SOURCE_DIR}/Modules/Remote/SphinxExamples/src/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/SphinxExamples/src/*.py"
)

# Use relative paths
set( examples_list )
string( LENGTH "${PROJECT_SOURCE_DIR}/" root_length )
foreach( _example ${examples_list_fullpath} )
  string( SUBSTRING ${_example} ${root_length} -1 example_relative )
  list( APPEND examples_list ${example_relative} )
endforeach()
string( LENGTH "${PROJECT_SOURCE_DIR}/Modules/Remote/" root_length )
foreach( _example ${sphinx_examples_list_fullpath} )
  string( SUBSTRING ${_example} ${root_length} -1 example_relative )
  list( APPEND examples_list ${example_relative} )
endforeach()

# remove the file before we begin appending
file( REMOVE ${OUTPUT_FILE} )

# begin comment
file( APPEND ${OUTPUT_FILE} "/**\n")

# After updating to Doxygen 1.8.4, this new line is required for any
# of the \example to be utilized
file( APPEND ${OUTPUT_FILE} " \n")

foreach( f IN LISTS examples_list )
  file( APPEND ${OUTPUT_FILE}  "\\example ${f}\n" )
endforeach()

# end comment
file( APPEND ${OUTPUT_FILE}  "*/\n" )
