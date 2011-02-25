itk_module_test()
set(ITK-DeprecatedTests
itk2DDeformableTest.cxx
#itkAnalyzeImageIODirectionsTest.cxx
#itkAnalyzeImageIOTest.cxx
itkDeformableTest.cxx
#itkDICOMImageIO2Test.cxx
#itkDicomImageIOTest.cxx
#itkDICOMImageSeriesTest.cxx
#itkDICOMImageSeriesTest2.cxx
#itkDICOMSeriesFileNamesTest.cxx
#itkFEMLandmarkLoadImplementationTest.cxx
#itkFFTTests.cxx
#itkImageReadDICOMSeriesWriteTest.cxx
itkNonThreadedShrinkImageTest.cxx
itkNonUniformBSplineTest.cxx
#itkQuaternionOrientationAdapterTest.cxx
itkReflectImageFilterTest.cxx
# itkScalarTest.cxx (itkScalar.h is missing)
itkShiftScaleInPlaceImageFilterTest.cxx
itkTwoOutputExampleImageFilterTest.cxx
# itkVOLImageIOTest.cxx (itkVOLImageIOFactory.h is missing)
)


CreateTestDriver(ITK-Deprecated  "${ITK-Deprecated-Test_LIBRARIES}" "${ITK-DeprecatedTests}")

add_test(NAME itk2DDeformableTest
      COMMAND ITK-DeprecatedTestDriver itk2DDeformableTest)
add_test(NAME itkDeformableTest
      COMMAND ITK-DeprecatedTestDriver itkDeformableTest)
add_test(NAME itkNonThreadedShrinkImageTest
      COMMAND ITK-DeprecatedTestDriver itkNonThreadedShrinkImageTest)
add_test(NAME itkNonUniformBSplineTest
      COMMAND ITK-DeprecatedTestDriver itkNonUniformBSplineTest)
add_test(NAME itkReflectImageFilterTest
      COMMAND ITK-DeprecatedTestDriver itkReflectImageFilterTest)
add_test(NAME itkShiftScaleInPlaceImageFilterTest
      COMMAND ITK-DeprecatedTestDriver itkShiftScaleInPlaceImageFilterTest)
add_test(NAME itkTwoOutputExampleImageFilterTest
      COMMAND ITK-DeprecatedTestDriver itkTwoOutputExampleImageFilterTest)
