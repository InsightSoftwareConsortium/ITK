/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h"


void RegisterTests()
{
  REGISTER_TEST(itkConvertBufferTest);
  REGISTER_TEST(itkConvertBufferTest2);
  REGISTER_TEST(itkBMPImageIOTest);
  REGISTER_TEST(itkBMPImageIOTest2);
  REGISTER_TEST(itkBioRadImageIOTest);
  REGISTER_TEST(itkLSMImageIOTest);
  REGISTER_TEST(itkDICOMImageIO2Test);
  REGISTER_TEST(itkDicomImageIOTest);
  REGISTER_TEST(itkDicomImageIODirection2DTest);
  REGISTER_TEST(itkDICOMSeriesFileNamesTest);
  REGISTER_TEST(itkDICOMImageSeriesTest);
  REGISTER_TEST(itkDICOMImageSeriesTest2);
  REGISTER_TEST(itkAnalyzeImageIOTest);
  REGISTER_TEST(itkAnalyzeImageIOTest2);
  REGISTER_TEST(itkAnalyzeImageIOBadHeader);
  REGISTER_TEST(itkAnalyzeImageIODirectionsTest);
  REGISTER_TEST(itkAnalyzeImageIORGBImageTest);
  REGISTER_TEST(itkGiplImageIOTest);
  REGISTER_TEST(itkImageFileReaderStreamingTest);
  REGISTER_TEST(itkImageFileReaderStreamingTest2);
  REGISTER_TEST(itkImageFileReaderTest1);
  REGISTER_TEST(itkImageFileReaderDimensionsTest);
  REGISTER_TEST(itkImageFileWriterTest);
  REGISTER_TEST(itkImageFileWriterTest2);
  REGISTER_TEST(itkImageFileWriterPastingTest1);
  REGISTER_TEST(itkImageFileWriterPastingTest2);
  REGISTER_TEST(itkImageFileWriterStreamingTest1);
  REGISTER_TEST(itkImageFileWriterStreamingTest2);
  REGISTER_TEST(itkImageFileWriterStreamingPastingCompressingTest1);
  REGISTER_TEST(itkImageSeriesReaderDimensionsTest);
  REGISTER_TEST(itkImageSeriesWriterTest);
  REGISTER_TEST(itkImageReadDICOMSeriesWriteTest);
  REGISTER_TEST(itkImageIOBaseTest);
  REGISTER_TEST(itkImageIOFileNameExtensionsTests);
  REGISTER_TEST(itkIOCommonTest);
  REGISTER_TEST(itkIOPrintTest);
  REGISTER_TEST(itkIOPluginTest);
  REGISTER_TEST(itkJPEGImageIOTest);
  REGISTER_TEST(itkMatrixImageWriteReadTest);
  REGISTER_TEST(itkMeshSpatialObjectIOTest);
  REGISTER_TEST(itkMetaImageIOTest);
  REGISTER_TEST(itkMetaImageStreamingIOTest);
  REGISTER_TEST(itkMetaImageStreamingWriterIOTest);
  REGISTER_TEST(itkNrrdImageIOTest);
  REGISTER_TEST(itkNrrdImageReadWriteTest);
  REGISTER_TEST(itkNrrdRGBImageReadWriteTest);
  REGISTER_TEST(itkNrrdRGBAImageReadWriteTest);
  REGISTER_TEST(itkNrrdDiffusionTensor3DImageReadTest);
  REGISTER_TEST(itkNrrdDiffusionTensor3DImageReadWriteTest);
  REGISTER_TEST(itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest);
  REGISTER_TEST(itkNrrdComplexImageReadTest);
  REGISTER_TEST(itkNrrdComplexImageReadWriteTest);
  REGISTER_TEST(itkNrrdVectorImageReadTest);
  REGISTER_TEST(itkNrrdVectorImageReadWriteTest);
  REGISTER_TEST(itkNrrdCovariantVectorImageReadTest);
  REGISTER_TEST(itkNrrdCovariantVectorImageReadWriteTest);
  REGISTER_TEST(itkNumericSeriesFileNamesTest);
  REGISTER_TEST(itkPolygonGroupSpatialObjectXMLFileTest);
  REGISTER_TEST(itkPNGImageIOTest);
  REGISTER_TEST(itkPNGRGBAIOTest);
  REGISTER_TEST(itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1);
  REGISTER_TEST(itkVTKImageIOTest);
  REGISTER_TEST(itkVTKImageIOTest2);
  REGISTER_TEST(itkVTKPolyDataIOQuadEdgeMeshTest);
  REGISTER_TEST(itkVTKPolyDataReaderQuadEdgeMeshTest);
  REGISTER_TEST(itkVTKPolyDataReaderTest);
  REGISTER_TEST(itkVTKPolyDataWriterTest01);
  REGISTER_TEST(itkVTKPolyDataWriterTest02);
  REGISTER_TEST(itkRawImageIOTest);
  REGISTER_TEST(itkRawImageIOTest2);
  REGISTER_TEST(itkRawImageIOTest3);
  REGISTER_TEST(itkRawImageIOTest4);
  REGISTER_TEST(itkRawImageIOTest5);
  REGISTER_TEST(itkVectorImageReadWriteTest);
  REGISTER_TEST(itkReadWriteImageWithDictionaryTest);
  REGISTER_TEST(itkReadWriteSpatialObjectTest);
  REGISTER_TEST(itkRegularExpressionSeriesFileNamesTest);
  REGISTER_TEST(itkArchetypeSeriesFileNamesTest);
  REGISTER_TEST(itkSymmetricSecondRankTensorImageReadTest);
  REGISTER_TEST(itkSymmetricSecondRankTensorImageWriteReadTest);
  REGISTER_TEST(itkStimulateImageIOTest);
  REGISTER_TEST(itkStimulateImageIOTest2);
  REGISTER_TEST(itkGEImageIOTest);
  REGISTER_TEST(testMetaUtils);
  REGISTER_TEST(testMetaBlob);
  REGISTER_TEST(testMetaImage);
  REGISTER_TEST(testMetaLine);
  REGISTER_TEST(testMetaLandmark);
  REGISTER_TEST(testMetaObject);
  REGISTER_TEST(testMetaScene);
  REGISTER_TEST(testMetaSurface);
  REGISTER_TEST(testMetaTube);
  REGISTER_TEST(testMetaGroup);
  REGISTER_TEST(testMetaMesh);
  REGISTER_TEST(testMetaArray);
  REGISTER_TEST(testMetaCommand);
  REGISTER_TEST(itkGEImageIOFactoryTest);
  REGISTER_TEST(itkTIFFImageIOTest);
  REGISTER_TEST(itkTransformIOTest);
  REGISTER_TEST(itkImageIODirection2DTest);
  REGISTER_TEST(itkImageIODirection3DTest);
  REGISTER_TEST(itkLargeImageWriteReadTest);
  REGISTER_TEST(itkLargeImageWriteConvertReadTest);
  REGISTER_TEST(itkNiftiImageIOTest);
  REGISTER_TEST(itkNiftiImageIOTest2);
  REGISTER_TEST(itkNiftiImageIOTest3);
  REGISTER_TEST(itkNiftiImageIOTest4);
  REGISTER_TEST(itkNiftiImageIOTest5);
  REGISTER_TEST(itkNiftiImageIOTest6);
  REGISTER_TEST(itkNiftiImageIOTest7);
  REGISTER_TEST(itkNiftiImageIOTest8);
  REGISTER_TEST(itkNiftiImageIOTest9);
  REGISTER_TEST(itkNiftiImageIOTest10);
  REGISTER_TEST(itkNiftiImageIOTest11);
  REGISTER_TEST(itkImageFileWriterUpdateLargestPossibleRegionTest);
}
