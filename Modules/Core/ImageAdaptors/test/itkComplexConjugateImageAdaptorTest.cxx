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

#include "itkComplexConjugateImageAdaptor.h"
#include "itkImageRegionIterator.h"


typedef std::complex< float >                          PixelType;
typedef itk::Image< PixelType, 2 >                     ImageType;
typedef itk::ComplexConjugateImageAdaptor< ImageType > AdaptorType;

int itkComplexConjugateImageAdaptorTest(int, char* [])
{
  // Create a test image.
  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size = {{10, 10}};
  image->SetRegions( size );
  image->Allocate();
  ImageType::RegionType region = image->GetLargestPossibleRegion();

  srand( 50L );

  itk::ImageRegionIterator< ImageType> iter( image, region );
  for ( iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
    float randMax = static_cast< float >( RAND_MAX );
    PixelType pixel( static_cast< float >( rand() ) / randMax,
                     static_cast< float >( rand() ) / randMax );
    iter.Set( pixel );
    }

  image->FillBuffer( PixelType( 2.0, -3.7 ) );

  // Create adaptor.
  AdaptorType::Pointer adaptor = AdaptorType::New();
  adaptor->SetImage( image );

  // Iterate over the pixels and check the functioning of the adaptor.
  itk::ImageRegionIterator< ImageType >   iterA( image, region );
  itk::ImageRegionIterator< AdaptorType > iterB( adaptor, region );

  for ( iterA.GoToBegin(), iterB.GoToBegin();
        !iterA.IsAtEnd(); ++iterA, ++iterB )
    {
    PixelType imageValue = iterA.Get();
    PixelType adaptedValue = iterB.Get();

    // Check that the adapted value is as expected.
    if ( imageValue != std::conj( adaptedValue ) )
      {
      std::cerr << "Unexpected adapted pixel value." << std::endl;
      std::cerr << "Got " << adaptedValue << ", expected " << imageValue
                << std::endl;
      return EXIT_FAILURE;
      }

    // Try setting the value and reading it again.
    iterB.Set( adaptedValue );
    PixelType newValue = iterB.Get();
    if ( adaptedValue != newValue )
      {
      std::cerr << "Setting adapted pixel value failed." << std::endl;
      std::cerr << "Got " << newValue << ", expected " << adaptedValue
                << std::endl;
      return EXIT_FAILURE;
      }
    }

  // To improve code coverage
  adaptor->Print( std::cout );

  return EXIT_SUCCESS;
}
