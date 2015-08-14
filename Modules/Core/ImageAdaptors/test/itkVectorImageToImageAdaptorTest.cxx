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

#include <iostream>
#include "itkVectorImageToImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

// This test tests the basic functionality of
// VectorImageToImageAdaptor, espeically the Set/GetPixel() methods.

int itkVectorImageToImageAdaptorTest( int, char* [] )
{

  //image type typedefs
  const unsigned int Dimension    = 3;
  const unsigned int VectorLength = 4;
  const unsigned int componentToExtract = 3;
  typedef float PixelType;

  typedef itk::VectorImage< PixelType, Dimension >   VectorImageType;

  typedef itk::VectorImageToImageAdaptor< PixelType, Dimension > VectorImageToImageAdaptorType;

  //initialize a vector image
  VectorImageType::Pointer vectorImage = VectorImageType::New();
  VectorImageType::IndexType start;
  itk::VariableLengthVector< PixelType > f( VectorLength );
  VectorImageType::SizeType  size;
  for( unsigned int i = 0; i < VectorLength; i++ )
    {
    f[i] = PixelType(i);
    }
  start.Fill(0);
  size.Fill(50);

  VectorImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( start );
  vectorImage->SetVectorLength( VectorLength );
  vectorImage->SetRegions( region );
  vectorImage->Allocate();
  vectorImage->FillBuffer( f );


   //run the adaptor
  VectorImageToImageAdaptorType::Pointer vectorImageToImageAdaptor = VectorImageToImageAdaptorType::New();
  vectorImageToImageAdaptor->SetExtractComponentIndex( componentToExtract );

  vectorImageToImageAdaptor->SetImage(vectorImage);
  vectorImageToImageAdaptor->Update();

  //test adaptor with const iterator
  itk::ImageRegionConstIteratorWithIndex< VectorImageToImageAdaptorType> adaptIt(vectorImageToImageAdaptor, region);
  adaptIt.GoToBegin();
  while (!adaptIt.IsAtEnd())
    {
    PixelType pixelV = adaptIt.Get();
    if (itk::Math::NotAlmostEquals( pixelV, PixelType(componentToExtract) ))
      {
      std::cout << "Wrong Pixel Value: adaptIt(" << adaptIt.GetIndex() << ") = " << adaptIt.Get()
                 << std::endl;

      return EXIT_FAILURE;
      }
    ++adaptIt;
    }

  //test Get/SetPixel() methods
  VectorImageToImageAdaptorType::IndexType index;
  index.Fill(10);
  TEST_EXPECT_EQUAL(PixelType(componentToExtract), vectorImageToImageAdaptor->GetPixel(index));

  PixelType v = 4.4f;
  vectorImageToImageAdaptor->SetPixel(index, v);
  TEST_EXPECT_EQUAL(v, vectorImageToImageAdaptor->GetPixel(index));

  return EXIT_SUCCESS;
}
