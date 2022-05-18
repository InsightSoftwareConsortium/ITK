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
#include "itkRescaleIntensityImageFilter.h"

#include "itkHConvexImageFilter.h"
#include "itkHConcaveImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkHConvexConcaveImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
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

  using InputPixelType = float;
  using OutputPixelType = float;
  using WritePixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using WriteImageType = itk::Image<WritePixelType, Dimension>;

  // Readers/writers
  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<WriteImageType>;
  using RescaleType = itk::RescaleIntensityImageFilter<OutputImageType, WriteImageType>;

  // Define the itk::HConvexImageFilter filter type
  using HConvexFilterType = itk::HConvexImageFilter<InputImageType, InputImageType>;
  // Define the itk::HConcaveImageFilter filter type
  using HConcaveFilterType = itk::HConcaveImageFilter<InputImageType, InputImageType>;


  // Creation of reader and writer filters
  auto reader = ReaderType::New();
  auto writer = WriterType::New();
  auto rescaler = RescaleType::New();

  // Create the filters
  auto hConvexFilter = HConvexFilterType::New();
  auto hConcaveFilter = HConcaveFilterType::New();

  itk::SimpleFilterWatcher watchConvex(hConvexFilter, "HConvexImageFilter");
  itk::SimpleFilterWatcher watchConcave(hConcaveFilter, "HConcaveImageFilter");

  // Set up the input and output files
  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Set up the filters
  hConvexFilter->SetInput(reader->GetOutput());
  hConvexFilter->SetHeight(std::stod(argv[3]));

  hConcaveFilter->SetInput(reader->GetOutput());
  hConcaveFilter->SetHeight(std::stod(argv[3]));

  // Create a filter to add the two images
  using AddFilterType = itk::AddImageFilter<InputImageType, InputImageType, OutputImageType>;

  auto add = AddFilterType::New();
  add->SetInput1(hConvexFilter->GetOutput());
  add->SetInput2(hConcaveFilter->GetOutput());

  // Run the filters
  rescaler->SetInput(add->GetOutput());
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);

  writer->SetInput(rescaler->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
