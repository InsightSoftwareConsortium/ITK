ADD_LIBRARY(ITKBasicFilters itkVTKImageExportBase.cxx)
TARGET_LINK_LIBRARIES(ITKBasicFilters ITKCommon)
INSTALL_TARGETS(/lib/InsightToolkit ITKBasicFilters)
INSTALL_FILES(/include/InsightToolkit/BasicFilters "(\\.h|\\.txx)$")
