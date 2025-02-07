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
#include "itkMetaImageIO.h"
#include "itkSimpleFilterWatcher.h"

#include "itkVectorImage.h"
#include "itkMaximumProjectionImageFilter.h"
#include "itkTestingMacros.h"

int
itkMaximumProjectionImageFilterTest4(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << "Dimension Inputimage Outputimage " << std::endl;
    return EXIT_FAILURE;
  }

  // Legacy compat with older MetaImages
  itk::MetaImageIO::SetDefaultDoublePrecision(6);
  const int dim = std::stoi(argv[1]);

  using PixelType = unsigned char;

  using ImageType = itk::VectorImage<PixelType, 3>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[2]);

  using FilterType = itk::MaximumProjectionImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();
  filter->SetInput(reader->GetOutput());
  filter->SetProjectionDimension(dim);
  // to be sure that the result is ok with several threads, even on a single
  // proc computer
  filter->SetNumberOfWorkUnits(2);

  const itk::SimpleFilterWatcher watcher(filter, "filter");

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[3]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
