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
#include "itkImage.h"
#include "itkTernaryOperatorImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"

int itkTernaryOperatorImageFilterTest( int, char* [] )
{

  //
  // Test the functor
  //

  itk::Functor::TernaryOperator< bool, short, short, short > func;

  if( ( true ? 12 : 37 ) != func( true, 12, 37 ) )
    {
    std::cerr << "Incorrect value returned: true ? 12 : 37." << std::endl;
    return EXIT_FAILURE;
    }

  if( ( false ? 12 : 37 ) != func( false, 12, 37 ) )
    {
    std::cerr << "Incorrect value returned: false ? 12 : 37." << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Check the image filter
  //

  //
  // Overview of the test:
  // Mask is a checkerboard, with "even" indices set to true and
  // "odd" indices set to false. Input2 and Input3 are set to constant
  // values that differ from one another. After the ternary operator
  // is applied, the output image should have the value of Input2 at
  // even indices and the value of Input3 at odd indices.
  //

  // Define the dimension of the images
  const unsigned int ImageDimension = 2;

  // Declare the pixel types of the images
  typedef bool                MaskPixelType;
  typedef short               GrayPixelType;

  typedef itk::Image< MaskPixelType, ImageDimension > MaskImageType;
  typedef itk::Image< GrayPixelType, ImageDimension > GrayImageType;

  MaskImageType::IndexType origin;
  origin.Fill( 0 );
  MaskImageType::SizeType size;
  size.Fill( 20 );
  MaskImageType::RegionType region(origin, size);

  MaskImageType::Pointer mask = MaskImageType::New();
  mask->SetRegions( region );
  mask->Allocate();
  mask->FillBuffer( false );

  // Checkerboard the mask
  typedef itk::ImageRegionIteratorWithIndex< MaskImageType > MaskItType;
  MaskItType maskIt( mask, mask->GetLargestPossibleRegion() );
  for (maskIt.GoToBegin(); !maskIt.IsAtEnd(); ++maskIt)
    {
    if (maskIt.GetIndex()[0] + maskIt.GetIndex()[1] % 2 == 0)
      maskIt.Set( true );
    }

  const short val1 = 25;
  GrayImageType::Pointer image1 = GrayImageType::New();
  image1->SetRegions( region );
  image1->Allocate();
  image1->FillBuffer( val1 );

  const short val2 = 123;
  GrayImageType::Pointer image2 = GrayImageType::New();
  image2->SetRegions( region );
  image2->Allocate();
  image2->FillBuffer( val2 );

  typedef itk::TernaryOperatorImageFilter< MaskImageType, GrayImageType >
    TernaryType;
  TernaryType::Pointer tern = TernaryType::New();

  EXERCISE_BASIC_OBJECT_METHODS( tern, TernaryOperatorImageFilter,
    TernaryFunctorImageFilter );

  tern->SetInput1( mask );
  tern->SetInput2( image1 );
  tern->SetInput3( image2 );
  tern->Update();

  GrayImageType::Pointer output = GrayImageType::New();
  output->Graft( tern->GetOutput() );

  // Even indices should be equal to val1 (the value of the second input)
  // Odd indices should be equal to val2 (the value of the third input)
  typedef itk::ImageRegionIteratorWithIndex< GrayImageType > GrayItType;
  GrayItType outIt( output, output->GetLargestPossibleRegion() );
  for( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
    {
    if (outIt.GetIndex()[0] + outIt.GetIndex()[1] % 2 == 0 && outIt.Get() != val1)
      {
      std::cerr << "Error: Value should be " << val1 << " but was " << outIt.Get() << std::endl;
      return EXIT_FAILURE;
      }
    if (outIt.GetIndex()[0] + outIt.GetIndex()[1] % 2 == 1 && outIt.Get() != val2)
      {
      std::cerr << "Error: Value should be " << val2 << " but was " << outIt.Get() << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
