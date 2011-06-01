set(ITK-Statistics_SRC
itkMaximumDecisionRule.cxx
itkDenseFrequencyContainer2.cxx
itkChiSquareDistribution.cxx
itkDecisionRuleBase.cxx
itkTDistribution.cxx
itkMersenneTwisterRandomVariateGenerator.cxx
itkGaussianDistribution.cxx
itkNormalVariateGenerator.cxx
itkMaximumRatioDecisionRule2.cxx
itkDecisionRule.cxx
itkMinimumDecisionRule2.cxx
itkSparseFrequencyContainer2.cxx
itkMaximumDecisionRule2.cxx
itkMaximumRatioDecisionRule.cxx
itkMinimumDecisionRule.cxx
)

add_library(ITK-Statistics ${ITK-Statistics_SRC})
target_link_libraries(ITK-Statistics  ${ITK-Common_LIBRARIES} ${ITK-Netlib_LIBRARIES})
itk_module_target(ITK-Statistics)
