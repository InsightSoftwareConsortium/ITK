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

#include "itkShapeLabelObjectAccessors.h"

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkLabelMapToAttributeImageFilter.h"

#include "itkTestingMacros.h"

int
itkLabelMapToAttributeImageFilterTest1(int argc, char * argv[])
{

  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using PixelType = unsigned short;

  using ImageType = itk::Image<PixelType, dim>;

  using ShapeLabelObjectType = itk::ShapeLabelObject<PixelType, dim>;
  using LabelMapType = itk::LabelMap<ShapeLabelObjectType>;

  // Reading Image File
  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Converting LabelImage to ShapeLabelMap
  using I2LType = itk::LabelImageToShapeLabelMapFilter<ImageType, LabelMapType>;
  auto i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  using L2ImageType =
    itk::LabelMapToAttributeImageFilter<LabelMapType,
                                        ImageType,
                                        itk::Functor::NumberOfPixelsLabelObjectAccessor<LabelMapType::LabelObjectType>>;
  auto l2i = L2ImageType::New();
  l2i->SetInput(i2l->GetOutput());
  itk::SimpleFilterWatcher watcher(l2i, "filter");

  using WriterType = itk::ImageFileWriter<ImageType>;

  auto writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
