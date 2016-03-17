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

#include "itkImage.h"
#include "itkTernaryOperatorImageFilter.h"
#include <iostream>
#include "itkImageRegionIteratorWithIndex.h"

int itkTernaryOperatorImageFilterTest(int argc, char *argv [])
{

  if (argc != 1)
    {
    std::cerr << "Usage: " << argv[0] << std::endl;
    return EXIT_FAILURE;
    }

  /////////////////////////////////////
  // First, test the functor itself. //
  /////////////////////////////////////

  itk::Functor::TernaryOperator< bool, short, short, short > func;

  if ((true ? 12 : 37) != func(true,12,37))
    {
    std::cerr << "Incorrect value returned: true ? 12 : 37." << std::endl;
    return EXIT_FAILURE;
    }

  if ((false ? 12 : 37) != func(false,12,37))
    {
    std::cerr << "Incorrect value returned: false ? 12 : 37." << std::endl;
    return EXIT_FAILURE;
    }

  //////////////////////////////////
  // Now, check the image filter. //
  //////////////////////////////////

  //
  // Overview of the test:
  // Mask is a checkerboard, with "even" indices set to true and
  // "odd" indices set to false.  Input2 and Input3 are set to constant
  // values that differ from one another.  After the ternary operator
  // is applied, the output image should have the value of Input2 at
  // even indices and the value of Input3 at odd indices.
  //

  typedef itk::Image< bool, 2 >  MaskType;
  typedef itk::Image< short, 2 > GrayType;

  MaskType::IndexType origin;
  origin.Fill( 0 );
  MaskType::SizeType size;
  size.Fill( 20 );
  MaskType::RegionType region(origin, size);

  MaskType::Pointer mask = MaskType::New();
  mask->SetRegions( region );
  mask->Allocate();
  mask->FillBuffer( false );

  // Checkerboard the mask.
  typedef itk::ImageRegionIteratorWithIndex< MaskType > MaskItType;
  MaskItType maskIt( mask, mask->GetLargestPossibleRegion() );
  for (maskIt.GoToBegin(); !maskIt.IsAtEnd(); ++maskIt)
    {
    if (maskIt.GetIndex()[0] + maskIt.GetIndex()[1] % 2 == 0)
      maskIt.Set( true );
    }

  const short val1 = 25;
  GrayType::Pointer image1 = GrayType::New();
  image1->SetRegions( region );
  image1->Allocate();
  image1->FillBuffer( val1 );

  const short val2 = 123;
  GrayType::Pointer image2 = GrayType::New();
  image2->SetRegions( region );
  image2->Allocate();
  image2->FillBuffer( val2 );

  typedef itk::TernaryOperatorImageFilter< MaskType, GrayType > TernaryType;
  TernaryType::Pointer tern = TernaryType::New();
  tern->SetInput1( mask );
  tern->SetInput2( image1 );
  tern->SetInput3( image2 );
  tern->Update();

  GrayType::Pointer output = GrayType::New();
  output->Graft( tern->GetOutput() );

  // Even indices should be equal to val1 (the value of the second input)
  // Odd indices should be equal to val2 (the value of the third input)
  typedef itk::ImageRegionIteratorWithIndex< GrayType > GrayItType;
  GrayItType outIt( output, output->GetLargestPossibleRegion() );
  for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
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
