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

int itkFlipImageFilterTest(int, char* [] )
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  typedef uint8_t PixelType;
  enum { ImageDimension = 3 };
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::FlipImageFilter<ImageType> FlipperType;


  // define a small input test
  ImageType::IndexType index = {{ 10, 20, 30 }};
  ImageType::SizeType size = {{5,4,3}};
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

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator inputIter( inputImage, inputImage->GetBufferedRegion() );

  PixelType counter = 0;
  while ( !inputIter.IsAtEnd() )
    {
    inputIter.Set( counter );
    ++counter;
    ++inputIter;
    }


  // permute the image
  FlipperType::Pointer flipper = FlipperType::New();
  FilterWatcher watcher(flipper);

  bool bArray[ImageDimension] = { true, false, true };
  FlipperType::FlipAxesArrayType flipAxes( bArray );

  flipper->SetFlipAxes( flipAxes );
  std::cout << "FlipAxes: " << flipper->GetFlipAxes() << std::endl;
  flipper->SetInput( inputImage );
  flipper->Update();

  flipper->GetOutput()->Print( std::cout );
  flipper->Print( std::cout );

  // check the output
  ImageType::Pointer outputImage = flipper->GetOutput();

  const ImageType::SpacingType& inputSpacing  = inputImage->GetSpacing();
  const ImageType::PointType&   inputOrigin   = inputImage->GetOrigin();
  const ImageType::SpacingType& outputSpacing = outputImage->GetSpacing();
  const ImageType::PointType&   outputOrigin  = outputImage->GetOrigin();

  inputImage->Print( std::cout );
  outputImage->Print( std::cout );

  typedef ImageType::IndexType IndexType;
  typedef IndexType::IndexValueType IndexValueType;

  inputIter.GoToBegin();
  bool passed = true;
  while ( !inputIter.IsAtEnd() )
    {

    IndexType inputIndex = inputIter.GetIndex();
    IndexType outputIndex;

    for ( int j = 0; j < ImageDimension; j++ )
      {
      if ( flipAxes[j] )
        {
        double temp = - 1 * ( static_cast<double>( inputIndex[j] ) *
           inputSpacing[j] + inputOrigin[j]);
        outputIndex[j] = itk::Math::Round<IndexValueType>(( temp - outputOrigin[j] ) /
           outputSpacing[j] );
        }
      else
        {
        outputIndex[j] = inputIndex[j];
        }
      }

    if ( inputIter.Get() != outputImage->GetPixel( outputIndex ) )
      {
      passed = false;
      std::cout << "Mismatch at index: " << inputIndex;
      std::cout << " " << outputIndex << std::endl;
      }

    ++inputIter;

    }

  if ( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
