SUBDIRS(vxl Statistics FEM)
# Sources of non-templated classes.
SOURCE_FILES(ITKNumerics_SRCS
itkCacheableScalarFunction
itkCompositeValleyFunction
itkMultivariateLegendrePolynomial
)

LINK_LIBRARIES (
VXLNumerics
)

IF (WIN32)
  SOURCE_FILES(ITKNumerics_SRCS itkWin32OutputWindow)
ENDIF (WIN32)

ADD_LIBRARY(ITKNumerics ITKNumerics_SRCS)

