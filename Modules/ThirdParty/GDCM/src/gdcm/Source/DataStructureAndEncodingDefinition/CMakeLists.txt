# Define the srcs for Data Structure and Encoding Definitions
# DSED

# dual compilation (namespace gdcm_ns), all the following either directly
# reference code using GDCM_SUPPORT_BROKEN_IMPLEMENTATION or indirectly
set(DSED2_SRCS
  gdcmByteValue.cxx
  gdcmDataElement.cxx
  gdcmDataSet.cxx
  gdcmExplicitDataElement.cxx
  gdcmFile.cxx # FileMeta is class member
  gdcmFileMetaInformation.cxx # subclass of DataSet
  gdcmFragment.cxx
  gdcmImplicitDataElement.cxx
  gdcmItem.cxx
  gdcmMediaStorage.cxx # SetFromModality takes a DataSet
  gdcmPrivateTag.cxx
  gdcmReader.cxx
  gdcmSequenceOfFragments.cxx
  gdcmSequenceOfItems.cxx
  gdcmValue.cxx # friend decl
  )
# need to prepare duplicate files to help cmake handle setting compile
# definitions (cmake cannot handle duplicate source file in same target).
# this trick allows us to avoid a static compilation which may not be portable
foreach(src ${DSED2_SRCS})
  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/${src}
    ${CMAKE_CURRENT_BINARY_DIR}/strict_${src}
    COPYONLY
    )
  list(APPEND DSED3_SRCS
    ${CMAKE_CURRENT_BINARY_DIR}/strict_${src}
    )
  set_property(
    SOURCE ${CMAKE_CURRENT_BINARY_DIR}/strict_${src}
    PROPERTY COMPILE_DEFINITIONS "GDCM_OVERRIDE_BROKEN_IMPLEMENTATION" "gdcm_ns=gdcmstrict"
    )
endforeach()

# the following source code do not need a duplicate compilation to handle
# GDCM_SUPPORT_BROKEN_IMPLEMENTATION
set(DSED_SRCS
  ${DSED2_SRCS}
  gdcmReader.strict.cxx # hook to call gdcmstrict API
  gdcmTag.cxx
  gdcmTagToVR.cxx
  gdcmCodeString.cxx
  gdcmFileSet.cxx
  gdcmByteSwapFilter.cxx
  gdcmUNExplicitImplicitDataElement.cxx
  gdcmWriter.cxx
  #gdcmParser.cxx
  gdcmCSAHeader.cxx
  gdcmMrProtocol.cxx
  gdcmPDBHeader.cxx
  gdcmTransferSyntax.cxx
  gdcmVM.cxx
  gdcmVR.cxx
  gdcmPreamble.cxx
  gdcmParseException.cxx
  gdcmUNExplicitDataElement.cxx
  gdcmCP246ExplicitDataElement.cxx
  gdcmExplicitImplicitDataElement.cxx
  gdcmVR16ExplicitDataElement.cxx
  )

# Add the include paths
include_directories(
  # Bin:
  "${GDCM_BINARY_DIR}/Source/Common"

  # src:
  "${GDCM_SOURCE_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/DataDictionary"
  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition/"

  "${GDCM_SOURCE_DIR}/Utilities"
 )

if(NOT GDCM_USE_SYSTEM_ZLIB)
  include_directories(
    "${GDCM_BINARY_DIR}/Utilities/gdcmzlib"
    )
endif()

add_library(gdcmDSED ${DSED_SRCS} ${DSED3_SRCS})
target_link_libraries(gdcmDSED LINK_PUBLIC gdcmCommon)
# zlib stuff are actually included (template) so we need to link them here.
target_link_libraries(gdcmDSED LINK_PRIVATE ${GDCM_ZLIB_LIBRARIES})
set_target_properties(gdcmDSED PROPERTIES ${GDCM_LIBRARY_PROPERTIES})
if(BUILD_SHARED_LIBS)
  #set(_ill "gdcmCommon")
  #set_target_properties(gdcmDSED PROPERTIES INTERFACE_LINK_LIBRARIES "${_ill}" LINK_INTERFACE_LIBRARIES "${_ill}")
endif()

# libs
install_library(gdcmDSED)
# PDB
install_pdb(gdcmDSED)
# include files
install_includes("*.h" "*.txx")
