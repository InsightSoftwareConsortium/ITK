SUBDIRS(vxl)

TESTS(
itkAmoebaOptimizerTest
itkConjugateGradientOptimizerTest
itkGradientDescentOptimizerTest
itkLevenbergMarquardtOptimizerTest
itkLBFGSOptimizerTest
itkNumericsHeaderTest
itkNumericsTest
)

LINK_LIBRARIES (
ITKCommon
)



