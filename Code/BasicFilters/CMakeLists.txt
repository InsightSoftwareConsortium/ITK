SET(ITKBasicFilters_SRCS
itkVTKImageExportBase.cxx
)

ADD_LIBRARY(ITKBasicFilters ${ITKBasicFilters_SRCS})
TARGET_LINK_LIBRARIES(ITKBasicFilters ITKCommon VXLNumerics)
INSTALL_TARGETS(/lib/InsightToolkit ITKBasicFilters)
