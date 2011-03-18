itk_module_test()
set(ITK-BloxTests
itkBloxBoundaryPointImageTest.cxx
itkBinaryMedialNodeMetricTest.cxx
itkBloxHeaderTest.cxx
itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest.cxx
itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest.cxx
)

CreateTestDriver(ITK-Blox  "${ITK-Blox-Test_LIBRARIES}" "${ITK-BloxTests}")

add_test(NAME itkBloxHeaderTest
      COMMAND ITK-BloxTestDriver itkBloxHeaderTest)
add_test(NAME itkBloxBoundaryPointImageTest
      COMMAND ITK-BloxTestDriver itkBloxBoundaryPointImageTest)
add_test(NAME itkBinaryMedialNodeMetricTest
      COMMAND ITK-BloxTestDriver itkBinaryMedialNodeMetricTest)
add_test(NAME itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest
      COMMAND ITK-BloxTestDriver itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest)
add_test(NAME itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest
      COMMAND ITK-BloxTestDriver itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest)
