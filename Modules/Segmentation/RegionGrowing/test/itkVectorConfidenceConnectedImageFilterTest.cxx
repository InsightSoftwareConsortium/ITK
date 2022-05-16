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
#include "itkVectorConfidenceConnectedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkVectorConfidenceConnectedImageFilterTest(int argc, char * argv[])
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if (argc < 9)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " InputImage BaselineImage seed1X seed1Y seed2X seed2Y multiplier iterations\n";
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelComponentType = unsigned char;
  using PixelType = itk::RGBPixel<PixelComponentType>;

  using OutputPixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;

  auto input = ReaderType::New();
  input->SetFileName(argv[1]);

  // Create a filter
  using FilterType = itk::VectorConfidenceConnectedImageFilter<ImageType, OutputImageType>;

  auto                     filter = FilterType::New();
  itk::SimpleFilterWatcher filterWatch(filter);

  filter->SetInput(input->GetOutput());
  filter->SetInitialNeighborhoodRadius(3); // measured in pixels

  FilterType::IndexType seed1;
  FilterType::IndexType seed2;

  seed1[0] = std::stoi(argv[3]);
  seed1[1] = std::stoi(argv[4]);

  seed2[0] = std::stoi(argv[5]);
  seed2[1] = std::stoi(argv[6]);

  filter->AddSeed(seed1);
  filter->AddSeed(seed2);

  filter->SetReplaceValue(255);
  filter->SetMultiplier(std::stod(argv[7]));
  filter->SetNumberOfIterations(std::stoi(argv[8]));

  ITK_TRY_EXPECT_NO_EXCEPTION(input->Update());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Test the GetMacros
  double doubleMultiplier = filter->GetMultiplier();
  std::cout << "filter->GetMultiplier(): " << doubleMultiplier << std::endl;

  unsigned int uintNumberOfIterations = filter->GetNumberOfIterations();
  std::cout << "filter->GetNumberOfIterations(): " << uintNumberOfIterations << std::endl;

  OutputPixelType pixelReplaceValue = filter->GetReplaceValue();
  std::cout << "filter->GetReplaceValue(): "
            << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(pixelReplaceValue) << std::endl;

  const unsigned int cuintInitialNeighborhoodRadius = filter->GetInitialNeighborhoodRadius();
  std::cout << "filter->GetInitialNeighborhoodRadius(): " << cuintInitialNeighborhoodRadius << std::endl;

  // Generate test image
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();

  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  // Exercise SetSeed() method
  filter->SetSeed(seed1);


  using VectorImageType = itk::VectorImage<PixelComponentType, Dimension>;

  using VectorReaderType = itk::ImageFileReader<VectorImageType>;
  auto vinput = VectorReaderType::New();
  vinput->SetFileName(argv[1]);

  using VectorFilterType = itk::VectorConfidenceConnectedImageFilter<VectorImageType, OutputImageType>;
  auto vFilter = VectorFilterType::New();

  vFilter->SetInput(vinput->GetOutput());
  vFilter->SetInitialNeighborhoodRadius(3); // measured in pixels
  vFilter->AddSeed(seed1);
  vFilter->AddSeed(seed2);
  vFilter->SetReplaceValue(255);
  vFilter->SetMultiplier(std::stod(argv[7]));
  vFilter->SetNumberOfIterations(std::stoi(argv[8]));
  vFilter->Update();


  itk::ImageRegionConstIterator<OutputImageType> iter(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
  itk::ImageRegionConstIterator<OutputImageType> viter(vFilter->GetOutput(), vFilter->GetOutput()->GetBufferedRegion());

  // check the at
  bool diff = false;
  while (!iter.IsAtEnd())
  {
    if (iter.Get() != viter.Get())
    {
      diff = true;
    }

    ++viter;
    ++iter;
  }

  if (diff)
  {
    std::cerr << "VectorImage output does not match covarient!" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
