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
file( GLOB_RECURSE wiki_examples_list_fullpath
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Conversions/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Curves/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Developer/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/DICOM/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/EdgesAndGradients/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Functions/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/ImageProcessing/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Images/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/ImageSegmentation/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Inspection/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Instructions/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/IO/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Iterators/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Math/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Meshes/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Metrics/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Morphology/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Operators/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/PointSet/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Registration/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Segmentation/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/SimpleOperations/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Smoothing/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/SpatialObjects/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/SpectralAnalysis/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Statistics/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/VectorImages/*.cxx"
  "${PROJECT_SOURCE_DIR}/Modules/Remote/WikiExamples/Visualization/*.cxx"
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
foreach( _example ${wiki_examples_list_fullpath} ${sphinx_examples_list_fullpath} )
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
