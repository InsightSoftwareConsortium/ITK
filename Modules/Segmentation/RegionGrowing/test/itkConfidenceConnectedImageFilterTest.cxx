/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <fstream>
#include "itkConfidenceConnectedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkConfidenceConnectedImageFilterTest(int argc, char * argv[])
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if (argc < 5)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage BaselineImage seed_x seed_y\n";
    return EXIT_FAILURE;
  }

  using PixelType = unsigned char;
  using myImage = itk::Image<PixelType, 2>;

  itk::ImageFileReader<myImage>::Pointer input = itk::ImageFileReader<myImage>::New();
  input->SetFileName(argv[1]);

  // Create a filter
  using FilterType = itk::ConfidenceConnectedImageFilter<myImage, myImage>;

  auto                     filter = FilterType::New();
  itk::SimpleFilterWatcher filterWatch(filter);

  filter->SetInput(input->GetOutput());
  filter->SetInitialNeighborhoodRadius(3); // measured in pixels

  FilterType::IndexType seed;
  seed[0] = std::stoi(argv[3]);
  seed[1] = std::stoi(argv[4]);
  //  FilterType::IndexType seed; seed[0] = 56; seed[1] = 90;
  //  FilterType::IndexType seed; seed[0] = 96; seed[1] = 214;
  filter->SetSeed(seed);
  filter->SetMultiplier(2.5);
  filter->SetReplaceValue(255);
  filter->SetNumberOfIterations(10);

  std::cout << "Filter Seeds";
  for (const auto & oneSeed : filter->GetSeeds())
  {
    std::cout << " " << oneSeed;
  }
  std::cout << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(input->Update());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Test the GetMacros
  double doubleMultiplier = filter->GetMultiplier();
  std::cout << "filter->GetMultiplier(): " << doubleMultiplier << std::endl;

  unsigned int uintNumberOfIterations = filter->GetNumberOfIterations();
  std::cout << "filter->GetNumberOfIterations(): " << uintNumberOfIterations << std::endl;

  PixelType pixelReplaceValue = filter->GetReplaceValue();
  std::cout << "filter->GetReplaceValue(): " << static_cast<itk::NumericTraits<PixelType>::PrintType>(pixelReplaceValue)
            << std::endl;

  const unsigned int cuintInitialNeighborhoodRadius = filter->GetInitialNeighborhoodRadius();
  std::cout << "filter->GetInitialNeighborhoodRadius(): " << cuintInitialNeighborhoodRadius << std::endl;

  const double mean = filter->GetMean();
  std::cout << "filter->GetMean(): " << mean << std::endl;

  const double variance = filter->GetVariance();
  std::cout << "filter->GetVariance(): " << variance << std::endl;

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  // Exercise AddSeed() method
  filter->AddSeed(seed);


  return EXIT_SUCCESS;
}
