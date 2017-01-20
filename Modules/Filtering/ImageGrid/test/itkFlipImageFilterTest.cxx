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

#include "itkFlipImageFilter.h"
#include "itkTextOutput.h"
#include "itkFilterWatcher.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"


int itkFlipImageFilterTest( int argc, char* argv[] )
{
  if( argc != 2 )
    {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0] << " FlipAboutOrigin" << std::endl;
    return EXIT_FAILURE;
    }

  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  const unsigned int ImageDimension = 3;
  typedef unsigned char                           PixelType;
  typedef itk::Image< PixelType, ImageDimension > ImageType;
  typedef itk::FlipImageFilter< ImageType >       FlipperType;

  // Define a small input image
  ImageType::IndexType index = {{ 10, 20, 30 }};
  ImageType::SizeType size = {{ 5, 4, 3 }};
  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  ImageType::SpacingType spacing;
  spacing[0] = 1.1;
  spacing[1] = 1.2;
  spacing[2] = 1.3;
  ImageType::PointType origin;
  origin[0] = 0.5;
  origin[1] = 0.4;
  origin[2] = 0.3;

  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->Allocate();

  inputImage->SetSpacing( spacing );
  inputImage->SetOrigin( origin );

  typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;
  IteratorType inputIter( inputImage, inputImage->GetBufferedRegion() );

  PixelType counter = 0;
  while( !inputIter.IsAtEnd() )
    {
    inputIter.Set( counter );
    ++counter;
    ++inputIter;
    }


  // Flip the image
  FlipperType::Pointer flipper = FlipperType::New();

  EXERCISE_BASIC_OBJECT_METHODS( flipper, FlipImageFilter, ImageToImageFilter );

  FilterWatcher watcher( flipper, "FlipImageFilter" );

  bool bArray[ImageDimension] = { true, false, true };
  FlipperType::FlipAxesArrayType flipAxes( bArray );

  flipper->SetFlipAxes( flipAxes );
  TEST_SET_GET_VALUE( flipAxes, flipper->GetFlipAxes() );

  bool flipAboutOrigin = static_cast< bool >( atoi( argv[1] ) );
  TEST_SET_GET_BOOLEAN( flipper, FlipAboutOrigin, flipAboutOrigin );

  flipper->SetInput( inputImage );

  flipper->Update();

  // Check the output
  ImageType::Pointer outputImage = flipper->GetOutput();

  const ImageType::SpacingType& inputSpacing  = inputImage->GetSpacing();
  const ImageType::PointType&   inputOrigin   = inputImage->GetOrigin();
  const ImageType::SpacingType& outputSpacing = outputImage->GetSpacing();
  const ImageType::PointType&   outputOrigin  = outputImage->GetOrigin();

  typedef ImageType::IndexType      IndexType;
  typedef IndexType::IndexValueType IndexValueType;

  inputIter.GoToBegin();
  bool passed = true;
  while( !inputIter.IsAtEnd() )
    {
    IndexType inputIndex = inputIter.GetIndex();
    IndexType outputIndex;

    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      if( flipAxes[j] )
        {
        int sign = flipAboutOrigin ? -1 : 1;

        ImageType::PointType::ValueType temp = sign * (
          static_cast<double>( inputIndex[j] ) * inputSpacing[j] + inputOrigin[j] );

        ImageType::PointType::ValueType outputPoint = flipAboutOrigin ?
          temp - outputOrigin[j] : outputOrigin[j] - temp;

        outputIndex[j] = itk::Math::Round< IndexValueType >( outputPoint / outputSpacing[j] );
        }
      else
        {
        outputIndex[j] = inputIndex[j];
        }
      }

    if( inputIter.Get() != outputImage->GetPixel( outputIndex ) )
      {
      passed = false;
      std::cout << "Mismatch at index: in: " << inputIndex;
      std::cout << "; out: " << outputIndex << std::endl;
      std::cout << "Expected pixel value: "
        << itk::NumericTraits< IteratorType::PixelType >::PrintType( inputIter.Get() )
        << ", but got: "
        << itk::NumericTraits< IteratorType::PixelType >::PrintType(
        outputImage->GetPixel( outputIndex ) ) << std::endl;
      }

    ++inputIter;
    }

  if( !passed )
    {
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
