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

#include "itkLabelImageToLabelMapFilter.h"
#include "itkLabelMapToRGBImageFilter.h"
#include "itkTestingMacros.h"


int
itkLabelMapToRGBImageFilterTest2(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int dim = 2;

  using IType = itk::Image<unsigned char, dim>;
  using OType = itk::VectorImage<unsigned char, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using ConverterType = itk::LabelImageToLabelMapFilter<IType>;
  auto converter = ConverterType::New();
  converter->SetInput(reader->GetOutput());

  using ColorizerType = itk::LabelMapToRGBImageFilter<ConverterType::OutputImageType, OType>;
  auto colorizer = ColorizerType::New();
  colorizer->SetInput(converter->GetOutput());

  itk::SimpleFilterWatcher watcher(colorizer, "filter");

  using WriterType = itk::ImageFileWriter<OType>;
  auto writer = WriterType::New();
  writer->SetInput(colorizer->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
