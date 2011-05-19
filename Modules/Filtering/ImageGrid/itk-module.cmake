itk_module(ITK-ImageGrid DEPENDS ITK-ImageFunction ITK-ImageFilterBase TEST_DEPENDS ITK-TestKernel ITK-RegistrationCommon)
#extra test dependency on ITK-RegistrationCommon is introduced by itkShrinkImagePreserveObjectPhysicalLocations
