PROJECT(DICOMParser)

SET(DICOMParserLibSrcFiles DICOMFile.cxx DICOMParser.cxx DICOMAppHelper.cxx)

ADD_LIBRARY(DICOMParser ${DICOMParserLibSrcFiles})

SET(LIBRARY_OUTPUT_PATH ${DICOMParser_BINARY_DIR}/bin CACHE INTERAL "single output directory for all libraries")
SET(EXECUTABLE_OUTPUT_PATH ${DICOMParser_BINARY_DIR}/bin CACHE INTERAL "single output directory for all executables")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
