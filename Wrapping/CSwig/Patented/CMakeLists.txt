# create the ITKPatentedTcl libraries
SET(WRAP_SOURCES
    wrap_itkSimpleFuzzyConnectednessImageFilterBase
    wrap_itkSimpleFuzzyConnectednessScalarImageFilter

)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/CommonA/ITKCommonA.mdx"
                       "${WrapITK_BINARY_DIR}/CommonB/ITKCommonB.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFiltersA/ITKBasicFiltersA.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFiltersB/ITKBasicFiltersB.mdx"
                       "${WrapITK_BINARY_DIR}/Algorithms/ITKAlgorithms.mdx"
                       "${WrapITK_BINARY_DIR}/Patented/ITKPatented.mdx"
)

ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKPatented Patented 
  "ITKNumerics;ITKCommonA;ITKCommonB;ITKBasicFiltersA;ITKBasicFiltersB" "" "")
