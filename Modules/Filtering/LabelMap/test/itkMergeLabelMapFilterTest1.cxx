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
#include "itkMergeLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int
itkMergeLabelMapFilterTest1(int argc, char * argv[])
{
  if (argc != 8)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input1 input2 output background1 background2 method expectfailure" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;
  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, dim>;

  using LabelObjectType = itk::LabelObject<PixelType, dim>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  auto i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  const PixelType background1 = std::stoi(argv[4]);
  i2l->SetBackgroundValue(background1);
  ITK_TEST_SET_GET_VALUE(background1, i2l->GetBackgroundValue());

  auto reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);
  auto i2l2 = I2LType::New();
  i2l2->SetInput(reader2->GetOutput());

  const PixelType background2 = std::stoi(argv[5]);
  i2l2->SetBackgroundValue(background2);
  ITK_TEST_SET_GET_VALUE(background2, i2l2->GetBackgroundValue());

  using ChangeType = itk::MergeLabelMapFilter<LabelMapType>;
  auto change = ChangeType::New();
  change->SetInput(i2l->GetOutput());
  change->SetInput(1, i2l2->GetOutput());
  std::cout << "======" << change->GetInputNames()[0] << std::endl;
  std::cout << "======" << change->GetInputNames()[1] << std::endl;

  auto method = static_cast<itk::ChoiceMethodEnum>(std::stoi(argv[6]));

  change->SetMethod(itk::ChoiceMethodEnum::STRICT);
  ITK_TEST_SET_GET_VALUE(itk::ChoiceMethodEnum::STRICT, change->GetMethod());

  change->SetMethod(itk::ChoiceMethodEnum::KEEP);
  ITK_TEST_SET_GET_VALUE(itk::ChoiceMethodEnum::KEEP, change->GetMethod());

  change->SetMethod(method);
  itk::SimpleFilterWatcher watcher6(change, "filter");

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  auto l2i = L2IType::New();
  l2i->SetInput(change->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[3]);
  writer->UseCompressionOn();

  bool expectfailure = std::stoi(argv[7]);

  if (expectfailure)
  {
    ITK_TRY_EXPECT_EXCEPTION(writer->Update());
  }
  else
  {
    ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());
  }

  ITK_TEST_SET_GET_VALUE(background1, change->GetOutput()->GetBackgroundValue());

  // Test streaming enumeration for MergeLabelMapFilterEnums::ChoiceMethod elements
  const std::set<itk::MergeLabelMapFilterEnums::ChoiceMethod> allChoiceMethod{
    itk::MergeLabelMapFilterEnums::ChoiceMethod::KEEP,
    itk::MergeLabelMapFilterEnums::ChoiceMethod::AGGREGATE,
    itk::MergeLabelMapFilterEnums::ChoiceMethod::PACK,
    itk::MergeLabelMapFilterEnums::ChoiceMethod::STRICT
  };
  for (const auto & ee : allChoiceMethod)
  {
    std::cout << "STREAMED ENUM VALUE MergeLabelMapFilterEnums::ChoiceMethod: " << ee << std::endl;
  }

  return EXIT_SUCCESS;
}
