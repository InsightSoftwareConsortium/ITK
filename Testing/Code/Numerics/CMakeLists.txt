SUBDIRS(vxl)

TESTS(
itkConjugateGradientOptimizerTest
itkLevenbergMarquardtOptimizerTest
itkNumericsHeaderTest
itkNumericsTest
)

LINK_LIBRARIES (
ITKCommon
)


