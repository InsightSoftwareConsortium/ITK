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

ADD_LIBRARY(ITKNumerics ITKNumerics_SRCS)

