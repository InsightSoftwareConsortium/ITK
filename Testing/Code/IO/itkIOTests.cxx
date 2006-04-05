/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIOTests.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
  REGISTER_TEST(itkBioRadImageIOTest);
  REGISTER_TEST(itkLSMImageIOTest);
  REGISTER_TEST(itkDicomImageIOTest);
  REGISTER_TEST(itkDICOMImageIO2Test);
  REGISTER_TEST(itkDICOMSeriesFileNamesTest);
  REGISTER_TEST(itkDICOMImageSeriesTest);
  REGISTER_TEST(itkAnalyzeImageIOTest);
  REGISTER_TEST(itkGiplImageIOTest);
  REGISTER_TEST(itkImageSeriesWriterTest);
  REGISTER_TEST(itkIOPrintTest);
  REGISTER_TEST(itkJPEGImageIOTest);
  REGISTER_TEST(itkMeshSpatialObjectIOTest);
  REGISTER_TEST(itkMetaImageIOTest);
  REGISTER_TEST(itkNrrdImageIOTest);
  REGISTER_TEST(itkNrrdImageReadWriteTest);
  REGISTER_TEST(itkNrrdRGBImageReadWriteTest);
  REGISTER_TEST(itkNrrdRGBAImageReadWriteTest);
  REGISTER_TEST(itkNrrdDiffusionTensor3DImageReadTest);
  REGISTER_TEST(itkNrrdDiffusionTensor3DImageReadWriteTest);
  REGISTER_TEST(itkNrrdComplexImageReadTest);
  REGISTER_TEST(itkNrrdComplexImageReadWriteTest);
  REGISTER_TEST(itkNrrdVectorImageReadTest);
  REGISTER_TEST(itkNrrdVectorImageReadWriteTest);
  REGISTER_TEST(itkNrrdCovariantVectorImageReadTest);
  REGISTER_TEST(itkNrrdCovariantVectorImageReadWriteTest);
  REGISTER_TEST(itkNumericSeriesFileNamesTest);
  REGISTER_TEST(itkPolygonGroupSpatialObjectXMLFileTest);
  REGISTER_TEST(itkPNGImageIOTest);
  REGISTER_TEST(itkVTKImageIOTest);
  REGISTER_TEST(itkRawImageIOTest);
  REGISTER_TEST(itkRawImageIOTest2);
  REGISTER_TEST(itkRawImageIOTest3);
  REGISTER_TEST(itkRawImageIOTest4);
  REGISTER_TEST(itkRawImageIOTest5);
  REGISTER_TEST(itkVectorImageReadWriteTest);
  REGISTER_TEST(itkReadWriteSpatialObjectTest);
  REGISTER_TEST(itkRegularExpressionSeriesFileNamesTest);
  REGISTER_TEST(itkArchetypeSeriesFileNamesTest);
  REGISTER_TEST(itkStimulateImageIOTest);
  REGISTER_TEST(itkStimulateImageIOTest2);
  REGISTER_TEST(itkGEImageIOTest);
  REGISTER_TEST(itkAnalyzeImageIOTest2);
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
  REGISTER_TEST(itkBrains2MaskTest);
  REGISTER_TEST(itkGEImageIOFactoryTest);
  REGISTER_TEST(itkTIFFImageIOTest);
  REGISTER_TEST(itkTransformIOTest);
  REGISTER_TEST(itkImageFileWriterTest);
  REGISTER_TEST(itkNiftiImageIOTest);
  REGISTER_TEST(itkNiftiImageIOTest2);
  REGISTER_TEST(itkNiftiImageIOTest3);
}
