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

#include "itkShapeRelabelImageFilter.h"

#include "itkTestingMacros.h"

int
itkShapeRelabelImageFilterTest1(int argc, char * argv[])
{

  if (argc != 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " background";
    std::cerr << " reverseOrdering attribute" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using IType = itk::Image<unsigned char, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using RelabelType = itk::ShapeRelabelImageFilter<IType>;
  auto opening = RelabelType::New();

  opening->SetInput(reader->GetOutput());

  // testing get/set BackgroundValue macro
  int BackgroundValue = (std::stoi(argv[3]));
  opening->SetBackgroundValue(BackgroundValue);
  ITK_TEST_SET_GET_VALUE(BackgroundValue, opening->GetBackgroundValue());

  // testing boolean macro for ReverseOrdering
  opening->ReverseOrderingOn();
  ITK_TEST_SET_GET_VALUE(true, opening->GetReverseOrdering());

  opening->ReverseOrderingOff();
  ITK_TEST_SET_GET_VALUE(false, opening->GetReverseOrdering());

  // testing get and set macros or ReverseOrdering
  bool reverseOrdering = std::stoi(argv[4]);
  opening->SetReverseOrdering(reverseOrdering);
  ITK_TEST_SET_GET_VALUE(reverseOrdering, opening->GetReverseOrdering());

  // testing get and set macros for Attribute
  opening->SetAttribute(RelabelType::LabelObjectType::FERET_DIAMETER);
  ITK_TEST_SET_GET_VALUE(RelabelType::LabelObjectType::FERET_DIAMETER, opening->GetAttribute());

  const std::string attributeByName{ argv[5] };
  opening->SetAttribute(attributeByName); // SetAttribute accepts a string for conversion to internal label code
  const RelabelType::AttributeType attributeByCode = RelabelType::LabelObjectType::LABEL;
  ITK_TEST_SET_GET_VALUE(attributeByCode, opening->GetAttribute());

  itk::SimpleFilterWatcher watcher(opening, "filter");

  using WriterType = itk::ImageFileWriter<IType>;
  auto writer = WriterType::New();
  writer->SetInput(opening->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Test Complete!" << std::endl;

  return EXIT_SUCCESS;
}
