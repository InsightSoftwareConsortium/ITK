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
#include "itkLabelImageToLabelMapFilter.h"
#include "itkLabelMapToBinaryImageFilter.h"

#include "itkTestingMacros.h"
#include "itkSimpleFilterWatcher.h"


int
itkLabelMapToBinaryImageFilterTest(int argc, char * argv[])
{

  if (argc != 5)
  {
    std::cerr << "usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputLabelImage outputBinaryImage";
    std::cerr << " foregroundValue backgroundValue";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using BinaryPixelType = unsigned char;
  using LabelPixelType = unsigned char;

  using BinaryImageType = itk::Image<BinaryPixelType, Dimension>;
  using LabelImageType = itk::Image<LabelPixelType, Dimension>;

  using LabelObjectType = itk::LabelObject<LabelPixelType, Dimension>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<LabelImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToLabelMapFilter<LabelImageType, LabelMapType>;
  auto i2l = I2LType::New();

  using L2IType = itk::LabelMapToBinaryImageFilter<LabelMapType, BinaryImageType>;
  auto l2i = L2IType::New();

  l2i->SetForegroundValue(std::stoi(argv[3]));
  ITK_TEST_SET_GET_VALUE(std::stoi(argv[3]), l2i->GetForegroundValue());

  l2i->SetBackgroundValue(std::stoi(argv[4]));
  ITK_TEST_SET_GET_VALUE(std::stoi(argv[4]), l2i->GetBackgroundValue());

  itk::SimpleFilterWatcher watcher(l2i);

  using WriterType = itk::ImageFileWriter<BinaryImageType>;
  auto writer = WriterType::New();

  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();


  i2l->SetInput(reader->GetOutput());
  l2i->SetInput(i2l->GetOutput());
  writer->SetInput(l2i->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  i2l->GetOutput()->PrintLabelObjects();

  std::cout << l2i->GetNameOfClass() << std::endl;

  l2i->Print(std::cout);

  return EXIT_SUCCESS;
}
