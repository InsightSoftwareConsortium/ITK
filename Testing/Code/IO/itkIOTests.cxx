// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(itkConvertBufferTest);
  REGISTER_TEST(itkDicomImageIOTest);
  REGISTER_TEST(itkDICOMImageIO2Test);
  REGISTER_TEST(itkAnalyzeImageIOTest);
  REGISTER_TEST(itkGiplImageIOTest);
  REGISTER_TEST(itkIOPrintTest);
  REGISTER_TEST(itkMetaImageIOTest);
  REGISTER_TEST(itkPNGImageIOTest);
  REGISTER_TEST(itkVOLImageIOTest);
  REGISTER_TEST(itkVTKImageIOTest);
  REGISTER_TEST(itkRawImageIOTest);
  REGISTER_TEST(itkRawImageIOTest2);
  REGISTER_TEST(itkRawImageIOTest3);
  REGISTER_TEST(itkRawImageIOTest4);
  REGISTER_TEST(itkImageSeriesIOTest);
  REGISTER_TEST(itkGEImageIOTest);
  REGISTER_TEST(itkAnalyzeImageIOTest2);
  REGISTER_TEST(itkImageSeriesIOTest);
  REGISTER_TEST(testMetaUtils);
  REGISTER_TEST(testMetaBlob);
  REGISTER_TEST(testMetaImage);
  REGISTER_TEST(testMetaLine);
  REGISTER_TEST(testMetaObject);
  REGISTER_TEST(testMetaScene);
  REGISTER_TEST(testMetaSurface);
  REGISTER_TEST(testMetaTube);
}
