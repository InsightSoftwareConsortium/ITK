ADD_LIBRARY(ITKDICOMParser DICOMFile.cxx DICOMParser.cxx DICOMAppHelper.cxx)
INSTALL_TARGETS(/lib/InsightToolkit ITKDICOMParser)
INSTALL_FILES(/include/InsightToolkit/Utilities/DICOMParser "(\\.h|\\.txx)$")
