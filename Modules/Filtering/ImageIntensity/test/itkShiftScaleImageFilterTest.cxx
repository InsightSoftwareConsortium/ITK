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


#include "itkShiftScaleImageFilter.h"
#include "itkRandomImageSource.h"

#include "itkFilterWatcher.h"
int itkShiftScaleImageFilterTest(int, char* [] )
{
  std::cout << "itkShiftScaleImageFilterTest Start" << std::endl;

  typedef itk::Image<char,3>                 TestInputImage;
  typedef itk::Image<unsigned char,3>        TestOutputImage;
  typedef itk::NumericTraits<char>::RealType RealType;

  TestInputImage::Pointer    inputImage  = TestInputImage::New();
  TestInputImage::RegionType region;
  TestInputImage::SizeType   size; size.Fill(64);
  TestInputImage::IndexType  index; index.Fill(0);

  region.SetIndex (index);
  region.SetSize (size);

  // first try a constant image
  double fillValue = -100.0;
  inputImage->SetRegions( region );
  inputImage->Allocate();
  inputImage->FillBuffer( static_cast< TestInputImage::PixelType >( fillValue ) );

  typedef itk::ShiftScaleImageFilter<TestInputImage,TestOutputImage> FilterType;
  FilterType::Pointer filter = FilterType::New();

  // Set up Start, End and Progress callbacks
  FilterWatcher filterWatch(filter);

  // Filter the image
  filter->SetInput (inputImage);
  filter->UpdateLargestPossibleRegion();

  // Now generate a real image

  typedef itk::RandomImageSource<TestInputImage> SourceType;
  SourceType::Pointer source = SourceType::New();
  TestInputImage::SizeValueType randomSize[3] = {17, 8, 20};

  // Set up Start, End and Progress callbacks
  FilterWatcher sourceWatch(source);

  // Set up source
  source->SetSize( randomSize );
  double minValue = -128.0;
  double maxValue = 127.0;

  source->SetMin( static_cast< TestInputImage::PixelType >( minValue ) );
  source->SetMax( static_cast< TestInputImage::PixelType >( maxValue ) );
  std::cout << source;


  // Test GetMacros
  RealType getShift = filter->GetShift();
  std::cout << "filter->GetShift(): " << getShift << std::endl;
  RealType getScale = filter->GetScale();
  std::cout << "filter->GetScale(): " << getScale << std::endl;
  long underflowCount = filter->GetUnderflowCount();
  std::cout << "filter->GetUnderflowCount(): " << underflowCount << std::endl;
  long overflowCount = filter->GetOverflowCount();
  std::cout << "filter->GetOverflowCount(): " << overflowCount << std::endl;


  filter->SetInput(source->GetOutput());
  filter->SetScale(4.0);
  try
    {
    filter->UpdateLargestPossibleRegion();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e;
    return -1;
    }

  return EXIT_SUCCESS;
}
