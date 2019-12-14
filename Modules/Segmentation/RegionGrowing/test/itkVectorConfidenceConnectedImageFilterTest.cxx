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
#include "itkVectorConfidenceConnectedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkSimpleFilterWatcher.h"

int
itkVectorConfidenceConnectedImageFilterTest(int ac, char * av[])
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if (ac < 9)
  {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage seed1X seed1Y seed2X seed2Y multiplier iterations\n";
    return -1;
  }

  constexpr unsigned int Dimension = 2;

  using PixelComponentType = unsigned char;
  using PixelType = itk::RGBPixel<PixelComponentType>;

  using OutputPixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;

  ReaderType::Pointer input = ReaderType::New();
  input->SetFileName(av[1]);

  // Create a filter
  using FilterType = itk::VectorConfidenceConnectedImageFilter<ImageType, OutputImageType>;

  FilterType::Pointer      filter = FilterType::New();
  itk::SimpleFilterWatcher filterWatch(filter);

  filter->SetInput(input->GetOutput());
  filter->SetInitialNeighborhoodRadius(3); // measured in pixels

  FilterType::IndexType seed1;
  FilterType::IndexType seed2;

  seed1[0] = std::stoi(av[3]);
  seed1[1] = std::stoi(av[4]);

  seed2[0] = std::stoi(av[5]);
  seed2[1] = std::stoi(av[6]);

  filter->AddSeed(seed1);
  filter->AddSeed(seed2);

  filter->SetReplaceValue(255);
  filter->SetMultiplier(std::stod(av[7]));
  filter->SetNumberOfIterations(std::stoi(av[8]));

  try
  {
    input->Update();
    filter->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected: " << e.GetDescription();
    return -1;
  }

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
  WriterType::Pointer writer = WriterType::New();

  writer->SetInput(filter->GetOutput());
  writer->SetFileName(av[2]);
  writer->Update();

  // Exercise SetSeed() method
  filter->SetSeed(seed1);


  using VectorImageType = itk::VectorImage<PixelComponentType, Dimension>;

  using VectorReaderType = itk::ImageFileReader<VectorImageType>;
  VectorReaderType::Pointer vinput = VectorReaderType::New();
  vinput->SetFileName(av[1]);

  using VectorFilterType = itk::VectorConfidenceConnectedImageFilter<VectorImageType, OutputImageType>;
  VectorFilterType::Pointer vFilter = VectorFilterType::New();

  vFilter->SetInput(vinput->GetOutput());
  vFilter->SetInitialNeighborhoodRadius(3); // measured in pixels
  vFilter->AddSeed(seed1);
  vFilter->AddSeed(seed2);
  vFilter->SetReplaceValue(255);
  vFilter->SetMultiplier(std::stod(av[7]));
  vFilter->SetNumberOfIterations(std::stoi(av[8]));
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
