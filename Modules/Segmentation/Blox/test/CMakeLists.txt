itk_module_test()
set(ITK-BloxTests
itkBloxCoreAtomTest.cxx
itkBloxBoundaryPointImageTest.cxx
itkBinaryMedialNodeMetricTest.cxx
itkBloxHeaderTest.cxx
itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest.cxx
itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest.cxx
itkMedialNodeCorrespondencesTest.cxx
)

CreateTestDriver(ITK-Blox  "${ITK-Blox-Test_LIBRARIES}" "${ITK-BloxTests}")

itk_add_test(NAME itkBloxHeaderTest
      COMMAND ITK-BloxTestDriver itkBloxHeaderTest)
itk_add_test(NAME itkBloxCoreAtomTest
      COMMAND ITK-BloxTestDriver itkBloxCoreAtomTest)
itk_add_test(NAME itkBloxBoundaryPointImageTest
      COMMAND ITK-BloxTestDriver itkBloxBoundaryPointImageTest)
itk_add_test(NAME itkBinaryMedialNodeMetricTest
      COMMAND ITK-BloxTestDriver itkBinaryMedialNodeMetricTest)
itk_add_test(NAME itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest
      COMMAND ITK-BloxTestDriver itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest)
itk_add_test(NAME itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest
      COMMAND ITK-BloxTestDriver itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest)
itk_add_test(NAME itkMedialNodeCorrespondencesTest
      COMMAND ITK-BloxTestDriver itkMedialNodeCorrespondencesTest)
