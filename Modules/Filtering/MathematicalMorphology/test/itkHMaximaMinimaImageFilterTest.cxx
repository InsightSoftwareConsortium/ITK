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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkHMaximaImageFilter.h"
#include "itkHMinimaImageFilter.h"
#include "itkTestingMacros.h"

int
itkHMaximaMinimaImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImageFile";
    std::cerr << " outputImageFile height" << std::endl;
    return EXIT_FAILURE;
  }

  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned short;
  using InternalPixelType = short;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using InternalImageType = itk::Image<InternalPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  // Readers/writers
  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  // Define the itk::HMaximaImageFilter filter type
  using HMaximaFilterType = itk::HMaximaImageFilter<InputImageType, InternalImageType>;
  // Define the itk::HMinimaImageFilter filter type
  using HMinimaFilterType = itk::HMinimaImageFilter<InternalImageType, OutputImageType>;


  // Creation of reader and writer filters
  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  // Create the filters
  auto hMaximaFilter = HMaximaFilterType::New();
  auto hMinimaFilter = HMinimaFilterType::New();

  itk::SimpleFilterWatcher watchHmaxima(hMaximaFilter, "HMaximaImageFilter");
  itk::SimpleFilterWatcher watchHminima(hMinimaFilter, "HMinimaImageFilter");

  // Set up the input and output files
  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Set up the filters
  hMaximaFilter->SetInput(reader->GetOutput());
  hMaximaFilter->SetHeight(static_cast<InputPixelType>(std::stod(argv[3])));

  hMinimaFilter->SetInput(hMaximaFilter->GetOutput());
  hMinimaFilter->SetHeight(static_cast<InputPixelType>(std::stod(argv[3])));

  // Run the filter
  writer->SetInput(hMinimaFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
