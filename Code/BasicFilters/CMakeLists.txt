LINK_LIBRARIES (
ITKCommon
VXLNumerics
)

INCLUDE_DIRECTORIES(
${ITK_SOURCE_DIR}/Code/BasicFilters
)

SOURCE_FILES(ITKBasicFilters_SRCS
itkWriter
)

ADD_LIBRARY(ITKBasicFilters ITKBasicFilters_SRCS)
