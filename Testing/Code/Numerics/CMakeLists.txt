SUBDIRS(vxl)

TESTS(
itkConjugateGradientOptimizerTest
itkGradientDescentOptimizerTest
itkLevenbergMarquardtOptimizerTest
itkNumericsHeaderTest
itkNumericsTest
)

LINK_LIBRARIES (
ITKCommon
)


