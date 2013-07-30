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
/**
 *
 *  This program illustrates the use of Adaptors and
 *  NthElementPixelAccessor
 *
 *  The example shows how an Adaptor can be used to
 *  get acces only to the Nth component of a vector image
 *  giving the appearance of being just a 'float' image
 *
 *  That will allow to pass the Nth component of this
 *  image as input or output to any filter that expects
 *  a float image
 *
 */

#include "itkAdaptImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkNthElementPixelAccessor.h"
#include "itkTestingMacros.h"
#include "itkVariableLengthVector.h"
#include "itkVectorImage.h"

//-------------------------
//
//   Main code
//
//-------------------------
int itkNthElementPixelAccessorTest2(int, char* []) {

  // Typedefs for convenience
  const unsigned int Dimension = 2;
  const unsigned int VectorLength = 3;

  typedef float PrecisionType;

  typedef itk::VectorImage< PrecisionType, Dimension >                            VectorImageType;
  typedef itk::VariableLengthVector< PrecisionType >                              PixelType;
  typedef itk::Image< PrecisionType, Dimension >                                  ScalarImageType;
  typedef itk::NthElementPixelAccessor< PrecisionType, PixelType >                AccessorType;
  typedef itk::AdaptImageFilter< VectorImageType, ScalarImageType, AccessorType > AdaptorType;

  // Test on variable length vector image
  VectorImageType::SizeType size;
  size[0] = 1;
  size[1] = 1;

  VectorImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  VectorImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize(  size  );

  VectorImageType::Pointer vectorImage = VectorImageType::New();

  vectorImage->SetLargestPossibleRegion( region );
  vectorImage->SetBufferedRegion( region );
  vectorImage->SetRequestedRegion( region );
  vectorImage->SetNumberOfComponentsPerPixel( VectorLength );
  vectorImage->Allocate();
  PixelType pixel;
  pixel.SetSize( VectorLength );
  pixel.Fill(0);
  vectorImage->FillBuffer(pixel);

  PixelType referencePixel;
  referencePixel.SetSize( VectorLength );

  // Value to initialize the pixels
  referencePixel[0] = 1.0f;
  referencePixel[1] = 0.5f;
  referencePixel[2] = 1.5f;

  vectorImage->SetPixel( index, referencePixel );

  // Print values to verify content
  pixel = vectorImage->GetPixel( index );
  std::cout << pixel[0] << "  ";
  std::cout << pixel[1] << "  ";
  std::cout << pixel[2] << std::endl;

  // Access values using adaptor
  AccessorType accessor;
  accessor.SetElementNumber(0);

  AdaptorType::Pointer adaptor = AdaptorType::New();
  adaptor->SetInput( vectorImage );
  adaptor->SetAccessor( accessor );
  adaptor->Update();

  ScalarImageType::Pointer scalarImage = adaptor->GetOutput();

  TEST_EXPECT_EQUAL( adaptor->GetOutput()->GetPixel(index), referencePixel[0] );

  // Test operator
  AccessorType accessor1;
  accessor1.SetElementNumber(1);
  if (accessor1 != accessor)
    {
    accessor = accessor1;
    adaptor->SetAccessor( accessor);
    adaptor->Update();
    scalarImage = adaptor->GetOutput();
    TEST_EXPECT_EQUAL( adaptor->GetOutput()->GetPixel(index), referencePixel[1] );
    }

  return EXIT_SUCCESS;
}
