INCLUDE_DIRECTORIES(
${ITK_SOURCE_DIR}/Code/Common
${ITK_SOURCE_DIR}/Code/BasicFilters
${ITK_SOURCE_DIR}/Code/Numerics/Statistics
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection
${ITK_SOURCE_DIR}/Validation/StatisticalClustering
)

LINK_DIRECTORIES(
${ITK_BINARY_DIR}/bin
)


LINK_LIBRARIES (
ITKCommon
ITKBasicFilters
ITKNumerics
ITKStatistics
ITKIO
)

SET(IBSRClassStatisticsApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/IBSRClassStatisticsApp.cxx
)

SET(BrainWebClassStatisticsApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/BrainWebClassStatisticsApp.cxx
)

SET(KdTreeBasedKmeansIBSRValidationApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/ParameterTable.cxx
Code/ClusteringOutputEvaluator.cxx
Code/KdTreeBasedKmeansIBSRValidationApp.cxx
)

SET(KdTreeBasedKmeansBrainWebValidationApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/ParameterTable.cxx
Code/ClusteringOutputEvaluator.cxx
Code/KdTreeBasedKmeansBrainWebValidationApp.cxx
)

SET(ExpectationMaximizationIBSRValidationApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/ParameterTable.cxx
Code/ClusteringOutputEvaluator.cxx
Code/ExpectationMaximizationIBSRValidationApp.cxx
)

SET(ExpectationMaximizationBrainWebValidationApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/ParameterTable.cxx
Code/ClusteringOutputEvaluator.cxx
Code/ExpectationMaximizationBrainWebValidationApp.cxx
)

IF(UNIX)
  LINK_LIBRARIES (
  -lm
  )
ENDIF(UNIX)

ADD_EXECUTABLE(IBSRClassStatisticsApp ${IBSRClassStatisticsApp_SRCS})
ADD_EXECUTABLE(BrainWebClassStatisticsApp ${BrainWebClassStatisticsApp_SRCS})
ADD_EXECUTABLE(KdTreeBasedKmeansIBSRValidationApp 
                 ${KdTreeBasedKmeansIBSRValidationApp_SRCS})
ADD_EXECUTABLE(KdTreeBasedKmeansBrainWebValidationApp 
                 ${KdTreeBasedKmeansBrainWebValidationApp_SRCS})
ADD_EXECUTABLE(ExpectationMaximizationIBSRValidationApp 
                 ${ExpectationMaximizationIBSRValidationApp_SRCS})
ADD_EXECUTABLE(ExpectationMaximizationBrainWebValidationApp 
                 ${ExpectationMaximizationBrainWebValidationApp_SRCS})
