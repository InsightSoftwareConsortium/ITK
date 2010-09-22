# create the ITKIOTcl libraries
set(WRAP_SOURCES
    wrap_IOBase
    wrap_itkImageFileReader_2D
    wrap_itkImageFileReader_3D
    wrap_itkImageFileWriter_2D
    wrap_itkImageFileWriter_3D
    wrap_itkImageSeriesReader
    wrap_itkImageSeriesWriter
)
set(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/CommonA/ITKCommonA.mdx"
                       "${WrapITK_BINARY_DIR}/CommonB/ITKCommonB.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFiltersA/ITKBasicFiltersA.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFiltersB/ITKBasicFiltersB.mdx"
                       "${WrapITK_BINARY_DIR}/IO/ITKIO.mdx"
)
set(ITK_EXTRA_TCL_SOURCES itkTkImageViewer2D.cxx)
set(ITK_EXTRA_TCL_WRAP wrap_itkTkImageViewer2D)
ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKIO IO "ITKNumerics;ITKCommonA;ITKCommonB" "" "")
if(ITK_CSWIG_TCL)
  target_link_libraries(ITKIOTcl ${TK_LIBRARY})
endif(ITK_CSWIG_TCL)
