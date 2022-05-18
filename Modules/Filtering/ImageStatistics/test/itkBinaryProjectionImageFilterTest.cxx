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
#include "itkThresholdLabelerImageFilter.h"
#include "itkChangeLabelImageFilter.h"

#include "itkBinaryProjectionImageFilter.h"
#include "itkTestingMacros.h"

int
itkBinaryProjectionImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " InputImage OutputImage Foreground Background" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int dim = 3;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, dim>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // produce an image with 3 labels: 0 (background), 100 and 200

  using LabelerType = itk::ThresholdLabelerImageFilter<ImageType, ImageType>;
  auto labeler = LabelerType::New();
  labeler->SetInput(reader->GetOutput());
  LabelerType::RealThresholdVector thresholds;
  thresholds.push_back(100);
  thresholds.push_back(200);
  labeler->SetRealThresholds(thresholds);

  using ChangeType = itk::ChangeLabelImageFilter<ImageType, ImageType>;
  auto change = ChangeType::New();
  change->SetInput(labeler->GetOutput());
  change->SetChange(1, 100);
  change->SetChange(2, 200);

  using FilterType = itk::BinaryProjectionImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();
  filter->SetInput(change->GetOutput());

  // Exercise Set/Get methods for Foreground Value
  filter->SetForegroundValue(255);

  if (filter->GetForegroundValue() != 255)
  {
    std::cerr << "Set/Get Foreground value problem: " << filter->GetForegroundValue() << std::endl;
    return EXIT_FAILURE;
  }

  filter->SetForegroundValue(std::stoi(argv[3]));

  // Exercise Set/Get methods for Background Value
  filter->SetBackgroundValue(0);

  if (filter->GetBackgroundValue() != 0)
  {
    std::cerr << "Set/Get Background value problem" << std::endl;
    return EXIT_FAILURE;
  }

  filter->SetBackgroundValue(std::stoi(argv[4]));

  itk::SimpleFilterWatcher watcher(filter, "filter");

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
