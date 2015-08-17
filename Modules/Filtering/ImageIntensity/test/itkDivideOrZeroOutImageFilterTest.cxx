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

#include "itkDivideOrZeroOutImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

int itkDivideOrZeroOutImageFilterTest(int, char* [])
{
  const unsigned int                                   Dimension = 3;
  typedef float                                        PixelType;
  typedef itk::Image< PixelType, Dimension >           ImageType;
  typedef itk::DivideOrZeroOutImageFilter< ImageType > DivideFilterType;

  ImageType::SizeType size = {{10, 12, 14}};

  ImageType::Pointer numeratorImage = ImageType::New();
  numeratorImage->SetRegions( size );
  numeratorImage->Allocate();
  numeratorImage->FillBuffer( 1.0f );

  ImageType::Pointer denominatorImage = ImageType::New();
  denominatorImage->SetRegions( size );
  denominatorImage->Allocate();

  // Set input test values
  denominatorImage->FillBuffer( 1.0f );
  ImageType::IndexType zeroIndex = {{4, 5, 6}};
  denominatorImage->SetPixel( zeroIndex, 0.0f );

  // Instantiate and run the filter
  DivideFilterType::Pointer divider = DivideFilterType::New();
  divider->SetInput1( numeratorImage );
  divider->SetInput2( denominatorImage );
  divider->InPlaceOn();
  divider->UpdateLargestPossibleRegion();

  ImageType::RegionType region = divider->GetOutput()->GetLargestPossibleRegion();
  itk::ImageRegionConstIteratorWithIndex< ImageType > iter( divider->GetOutput(), region );
  for (; !iter.IsAtEnd(); ++iter)
    {
    ImageType::IndexType index = iter.GetIndex();
    if ( index != zeroIndex )
      {
      if ( iter.Get() != 1.0f )
        {
        std::cerr << "Incorrect value. Got " << iter.Get() << ", expected 1.0"
                  << std::endl;
        return EXIT_FAILURE;
        }
      }
    else
      {
      if ( iter.Get() != 0.0f )
        {
        std::cerr << "Incorrect value when denominator is below threshold. "
                  << "Got " << iter.Get() << ", expected 0.0" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  divider->Print( std::cout );

  divider->SetThreshold( 2.0f );
  if ( divider->GetThreshold() != 2.0f )
    {
    std::cerr << "Threshold not set correctly. Got " << divider->GetThreshold()
              << ", expected 2.0." << std::endl;
    return EXIT_FAILURE;
    }

  divider->SetConstant( -2.3f );
  if ( itk::Math::NotExactlyEquals(divider->GetConstant(), -2.3f) )
    {
    std::cerr << "Constant not set correctly. Got " << divider->GetConstant()
              << ", expected -2.3 ." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
