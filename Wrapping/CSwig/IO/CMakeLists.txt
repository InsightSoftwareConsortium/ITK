# create the ITKIOTcl libraries
SET(WRAP_SOURCES
    wrap_IOBase
    wrap_itkImageFileReader
    wrap_itkImageFileWriter
    wrap_itkImageSeriesReader
    wrap_itkImageSeriesWriter
)
SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/Common/ITKCommon.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFilters/ITKBasicFilters.mdx"
                       "${WrapITK_BINARY_DIR}/IO/ITKIO.mdx"
)
SET(ITK_EXTRA_TCL_SOURCES itkTkImageViewer2D.cxx)
SET(ITK_EXTRA_TCL_WRAP wrap_itkTkImageViewer2D)
ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKIO IO "_ITKNumericsPython;_ITKCommonPython" "" "")
IF(ITK_CSWIG_TCL)
  TARGET_LINK_LIBRARIES(ITKIOTcl ${TK_LIBRARY})
ENDIF(ITK_CSWIG_TCL)
