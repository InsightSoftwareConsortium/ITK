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

#include "itkRescaleIntensityImageFilter.h"
#include "itkRandomImageSource.h"

int itkRescaleIntensityImageFilterTest(int, char* [] )
{
  std::cout << "itkRescaleIntensityImageFilterTest Start" << std::endl;

  typedef itk::Image<float,3> TestInputImage;
  typedef itk::Image<float,3> TestOutputImage;

  TestInputImage::RegionType region;
  TestInputImage::SizeType   size; size.Fill(64);
  TestInputImage::IndexType  index; index.Fill(0);

  region.SetIndex (index);
  region.SetSize (size);


  typedef itk::RescaleIntensityImageFilter<TestInputImage,TestOutputImage> FilterType;
  FilterType::Pointer filter = FilterType::New();

  // Now generate a real image

  typedef itk::RandomImageSource<TestInputImage> SourceType;
  SourceType::Pointer source = SourceType::New();
  TestInputImage::SizeValueType  randomSize[3] = {17, 8, 20};


  // Set up source
  source->SetSize(randomSize);
  double minValue = -128.0;
  double maxValue = 127.0;

  source->SetMin( static_cast< TestInputImage::PixelType >( minValue ) );
  source->SetMax( static_cast< TestInputImage::PixelType >( maxValue ) );

  filter->SetFunctor(filter->GetFunctor());
  filter->SetInput(source->GetOutput());

  const double desiredMinimum = -1.0;
  const double desiredMaximum =  1.0;

  filter->SetOutputMinimum( desiredMinimum );
  filter->SetOutputMaximum( desiredMaximum );
  try
    {
    filter->UpdateLargestPossibleRegion();
    filter->SetFunctor(filter->GetFunctor());
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e;
    return -1;
    }

  typedef itk::MinimumMaximumImageCalculator< TestOutputImage > CalculatorType;
  CalculatorType::Pointer calculator  =  CalculatorType::New();

  calculator->SetImage( filter->GetOutput() );

  calculator->Compute();

  const double tolerance = 1e-7;

  const double obtainedMinimum = calculator->GetMinimum();
  const double obtainedMaximum = calculator->GetMaximum();

  if( vnl_math_abs( obtainedMinimum - desiredMinimum ) > tolerance )
    {
    std::cerr << "Error in minimum" << std::endl;
    std::cerr << "Expected minimum = " << desiredMinimum  << std::endl;
    std::cerr << "Obtained minimum = " << obtainedMinimum << std::endl;
    return EXIT_FAILURE;
    }

  if( vnl_math_abs( obtainedMaximum - desiredMaximum ) > tolerance )
    {
    std::cerr << "Error in minimum" << std::endl;
    std::cerr << "Expected minimum = " << desiredMaximum  << std::endl;
    std::cerr << "Obtained minimum = " << obtainedMaximum << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}
