LINK_LIBRARIES (
ITKCommon
)

INCLUDE_DIRECTORIES(
${ITK_SOURCE_DIR}/Code/BasicFilters
)

SOURCE_FILES(ITKBasicFilters_SRCS
itkWriter
itkRawImageIO
)

ADD_LIBRARY(ITKBasicFilters ITKBasicFilters_SRCS)
