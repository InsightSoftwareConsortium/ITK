# create the ITKIOTcl libraries
SET(WRAP_SOURCES
    wrap_IOBase
    wrap_itkImageFileReader_2D
    wrap_itkImageFileReader_3D
    wrap_itkImageFileWriter_2D
    wrap_itkImageFileWriter_3D
    wrap_itkImageSeriesReader
    wrap_itkImageSeriesWriter
)
SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/CommonA/ITKCommonA.mdx"
                       "${WrapITK_BINARY_DIR}/CommonB/ITKCommonB.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFiltersA/ITKBasicFiltersA.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFiltersB/ITKBasicFiltersB.mdx"
                       "${WrapITK_BINARY_DIR}/IO/ITKIO.mdx"
)
SET(ITK_EXTRA_TCL_SOURCES itkTkImageViewer2D.cxx)
SET(ITK_EXTRA_TCL_WRAP wrap_itkTkImageViewer2D)
ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKIO IO "ITKNumerics;ITKCommonA;ITKCommonB" "" "")
IF(ITK_CSWIG_TCL)
  TARGET_LINK_LIBRARIES(ITKIOTcl ${TK_LIBRARY})
ENDIF(ITK_CSWIG_TCL)
