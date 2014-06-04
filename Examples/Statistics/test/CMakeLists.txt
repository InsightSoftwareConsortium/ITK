set(BASELINE ${ITK_DATA_ROOT}/Baseline/Statistics)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

itk_add_test(NAME  ScalarImageKmeansClassifierTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ScalarImageKmeansClassifierOutput.png}
            ${TEMP}/ScalarImageKmeansClassifierOutput.png
  $<TARGET_FILE:ScalarImageKmeansClassifier>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
        ${TEMP}/ScalarImageKmeansClassifierOutput.png
        1 3 14.8 91.6 134.9
)

itk_add_test(NAME  ScalarImageMarkovRandomField1Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ScalarImageMarkovRandomField1Output.png}
            ${TEMP}/ScalarImageMarkovRandomField1Output.png
  $<TARGET_FILE:ScalarImageMarkovRandomField1>
            ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
            DATA{${ITK_DATA_ROOT}/Input/Statistics/BrainT1Slice_AfterKMeansClassification.png}
            ${TEMP}/ScalarImageMarkovRandomField1Output.png
            50 3 3 14.8 91.6 134.9
)

itk_add_test(NAME  BayesianPluginClassifierTest COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:BayesianPluginClassifier>)

itk_add_test(NAME  BayesianClassifierInitializerTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/BayesianClassifierInitializerClass2Output.png}
            ${TEMP}/BayesianClassifierInitializerClass2Output.png
  $<TARGET_FILE:BayesianClassifierInitializer>
            ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
            ${TEMP}/BayesianClassifierInitializerMemberships.mhd
            4 2 ${TEMP}/BayesianClassifierInitializerClass2Output.png
)

# This test uses inputs from BayesianClassifierInitializer.
# If that output changes, a new file should be placed in Examples/Data.
itk_add_test(NAME  BayesianClassifierTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/BayesianClassifierLabelMap.png}
            ${TEMP}/BayesianClassifierLabelMap.png
  $<TARGET_FILE:BayesianClassifier>
            ${ITK_SOURCE_DIR}/Examples/Data/BayesianClassifierInitializerMemberships.mhd
            ${TEMP}/BayesianClassifierLabelMap.png 2
)

itk_add_test(NAME  EuclideanDistanceTest COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:EuclideanDistanceMetric>)

itk_add_test(NAME  GaussianDensityFunctionTest COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:GaussianMembershipFunction>)

itk_add_test(NAME  MinimumDecisionRuleTest COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:MinimumDecisionRule>)

itk_add_test(NAME  MaximumDecisionRuleTest COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:MaximumDecisionRule>)

itk_add_test(NAME  MaximumRatioDecisionRuleTest COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:MaximumRatioDecisionRule>)

itk_add_test(NAME  KdTreeTest COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:KdTree>)
