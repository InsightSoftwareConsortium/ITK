include_directories(
${ITK_SOURCE_DIR}/Code/Common
${ITK_SOURCE_DIR}/Code/BasicFilters
${ITK_SOURCE_DIR}/Code/Numerics/Statistics
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection
${ITK_SOURCE_DIR}/Validation/StatisticalClustering
)

link_directories(
${ITK_BINARY_DIR}/bin
)


link_libraries(
ITKCommon
ITKBasicFilters
ITKNumerics
ITKStatistics
ITKIO
)

set(IBSRClassStatisticsApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/IBSRClassStatisticsApp.cxx
)

set(BrainWebClassStatisticsApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/BrainWebClassStatisticsApp.cxx
)

set(KdTreeBasedKmeansIBSRValidationApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/ParameterTable.cxx
Code/ClusteringOutputEvaluator.cxx
Code/KdTreeBasedKmeansIBSRValidationApp.cxx
)

set(KdTreeBasedKmeansBrainWebValidationApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/ParameterTable.cxx
Code/ClusteringOutputEvaluator.cxx
Code/KdTreeBasedKmeansBrainWebValidationApp.cxx
)

set(ExpectationMaximizationIBSRValidationApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/ParameterTable.cxx
Code/ClusteringOutputEvaluator.cxx
Code/ExpectationMaximizationIBSRValidationApp.cxx
)

set(ExpectationMaximizationBrainWebValidationApp_SRCS
${ITK_SOURCE_DIR}/Applications/MRIBiasCorrection/OptionList.cxx
Code/ParameterTable.cxx
Code/ClusteringOutputEvaluator.cxx
Code/ExpectationMaximizationBrainWebValidationApp.cxx
)

if(UNIX)
  link_libraries(
  -lm
  )
endif(UNIX)

add_executable(IBSRClassStatisticsApp ${IBSRClassStatisticsApp_SRCS})
add_executable(BrainWebClassStatisticsApp ${BrainWebClassStatisticsApp_SRCS})
add_executable(KdTreeBasedKmeansIBSRValidationApp
                 ${KdTreeBasedKmeansIBSRValidationApp_SRCS})
add_executable(KdTreeBasedKmeansBrainWebValidationApp
                 ${KdTreeBasedKmeansBrainWebValidationApp_SRCS})
add_executable(ExpectationMaximizationIBSRValidationApp
                 ${ExpectationMaximizationIBSRValidationApp_SRCS})
add_executable(ExpectationMaximizationBrainWebValidationApp
                 ${ExpectationMaximizationBrainWebValidationApp_SRCS})
