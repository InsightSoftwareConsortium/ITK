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

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkConvertLabelMapFilter.h"
#include "itkTestingMacros.h"
#include "itkSimpleFilterWatcher.h"

int
itkConvertLabelMapFilterTest1(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputLabelImage outputLabelImage";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, dim>;


  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using L2SType = itk::LabelImageToShapeLabelMapFilter<ImageType>;
  auto l2s = L2SType::New();
  l2s->SetInput(reader->GetOutput());

  using LabelObjectType = itk::LabelObject<PixelType, dim>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using CastType = itk::ConvertLabelMapFilter<L2SType::OutputImageType, LabelMapType>;
  auto cast = CastType::New();
  cast->SetInput(l2s->GetOutput());
  itk::SimpleFilterWatcher watcher(cast, "cast");

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  auto l2i = L2IType::New();
  l2i->SetInput(cast->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // for visual validation
  std::cout << " ============ original label map ============" << std::endl;
  l2s->GetOutput()->PrintLabelObjects();
  std::cout << " ============ casted label map ============" << std::endl;
  cast->GetOutput()->PrintLabelObjects();

  return EXIT_SUCCESS;
}
