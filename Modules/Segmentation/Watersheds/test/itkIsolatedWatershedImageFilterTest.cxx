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

#include <fstream>
#include "itkIsolatedWatershedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int itkIsolatedWatershedImageFilterTest(int ac, char* av[] )
{
  if(ac < 7)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage seed1_x seed1_y seed2_x seed2_y\n";
    return EXIT_FAILURE;
    }

  typedef unsigned char            PixelType;
  typedef itk::Image<PixelType, 2> myImage;
  itk::ImageFileReader<myImage>::Pointer input
    = itk::ImageFileReader<myImage>::New();
  input->SetFileName(av[1]);

  // Create a filter
  typedef itk::IsolatedWatershedImageFilter<myImage,myImage> FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(input->GetOutput());

  FilterType::IndexType seed1;

  seed1[0] = atoi(av[3]); seed1[1] = atoi(av[4]);
  filter->SetSeed1(seed1);

  seed1[0] = atoi(av[5]); seed1[1] = atoi(av[6]);
  filter->SetSeed2(seed1);

  filter->SetThreshold(0.001);
  filter->SetReplaceValue1(255);
  filter->SetReplaceValue2(127);
  filter->SetUpperValueLimit(1);

  // Test SetMacro
  filter->SetIsolatedValueTolerance(.0001);

  // Test GetMacros
  double threshold = filter->GetThreshold();
  std::cout << "filter->GetThreshold(): "
            << threshold
            << std::endl;
  double isolatedValueTolerance = filter->GetIsolatedValueTolerance();
  std::cout << "filter->GetIsolatedValueTolerance(): "
            << isolatedValueTolerance
            << std::endl;
  double upperValueLimit = filter->GetUpperValueLimit();
  std::cout << "filter->GetUpperValueLimit(): "
            << upperValueLimit
            << std::endl;
  PixelType replaceValue1 = filter->GetReplaceValue1();
  std::cout << "filter->GetReplaceValue1(): "
            << static_cast<itk::NumericTraits<PixelType>::PrintType>(replaceValue1)
            << std::endl;
  PixelType replaceValue2 = filter->GetReplaceValue2();
  std::cout << "filter->GetReplaceValue2(): "
            << static_cast<itk::NumericTraits<PixelType>::PrintType>(replaceValue2)
            << std::endl;


  try
    {
    input->Update();
    filter->Update();
    double isolatedValue = filter->GetIsolatedValue();
    std::cout << "filter->GetIsolatedValue(): "
              << isolatedValue
              << std::endl;
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return EXIT_FAILURE;
    }

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( av[2] );
  writer->Update();

  //
  // Test when lower and upper thresholds are "close", expect the
  // filter not to crash, and complete
  //

  filter = FilterType::New();

  filter->SetInput(input->GetOutput());

  seed1[0] = atoi(av[3]); seed1[1] = atoi(av[4]);
  filter->SetSeed1(seed1);

  seed1[0] = atoi(av[5]); seed1[1] = atoi(av[6]);
  filter->SetSeed2(seed1);

  filter->SetThreshold(.1);
  filter->SetIsolatedValueTolerance(1.0);
  filter->SetReplaceValue1(255);
  filter->SetReplaceValue2(127);
  filter->SetUpperValueLimit(1);

  try
    {
    filter->Update();
    double isolatedValue = filter->GetIsolatedValue();
    std::cout << "filter->GetIsolatedValue(): "
              << isolatedValue
              << std::endl;
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;
}
