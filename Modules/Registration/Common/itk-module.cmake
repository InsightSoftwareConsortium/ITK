itk_module(ITK-RegistrationCommon DEPENDS ITK-Optimizers ITK-ImageIntensity ITK-ImageFunction ITK-ImageGrid ITK-SpatialObjects ITK-Smoothing ITK-ImageGradient ITK-ImageFeature ITK-FiniteDifference TEST_DEPENDS ITK-TestKernel ITK-DistanceMap)
#extra test dependency on ITK-DistanceMap is introduced by itkPointSetToPointSetRegistrationTest
