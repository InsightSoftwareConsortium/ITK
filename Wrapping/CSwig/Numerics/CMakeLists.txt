# create the ITKNumericsTcl libraries
set(WRAP_SOURCES
  wrap_ITKCostFunctions
  wrap_ITKOptimizers
)


set(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/CommonA/ITKCommonA.mdx"
                       "${WrapITK_BINARY_DIR}/CommonB/ITKCommonB.mdx"
)
ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKNumerics Numerics "ITKCommonA;ITKCommonB" "" "")

