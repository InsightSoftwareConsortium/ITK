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

#include "itkLabelShapeKeepNObjectsImageFilter.h"

#include "itkTestingMacros.h"

int
itkLabelShapeKeepNObjectsImageFilterTest1(int argc, char * argv[])
{

  if (argc != 7)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output";
    std::cerr << " background numberOfObjectsToKeep";
    std::cerr << "reverseOrdering attribute" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using IType = itk::Image<unsigned char, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using LabelKeepNObjectsType = itk::LabelShapeKeepNObjectsImageFilter<IType>;
  auto KeepNObjects = LabelKeepNObjectsType::New();

  KeepNObjects->SetInput(reader->GetOutput());

  // testing get/set BackgroundValue macro
  int BackgroundValue = (std::stoi(argv[3]));
  KeepNObjects->SetBackgroundValue(BackgroundValue);
  ITK_TEST_SET_GET_VALUE(BackgroundValue, KeepNObjects->GetBackgroundValue());

  // testing get and set macros for NumberOfObjects
  unsigned int numberOfObjects = std::stoi(argv[4]);
  KeepNObjects->SetNumberOfObjects(numberOfObjects);
  ITK_TEST_SET_GET_VALUE(numberOfObjects, KeepNObjects->GetNumberOfObjects());

  // testing boolean macro for ReverseOrdering
  KeepNObjects->ReverseOrderingOn();
  ITK_TEST_SET_GET_VALUE(true, KeepNObjects->GetReverseOrdering());

  KeepNObjects->ReverseOrderingOff();
  ITK_TEST_SET_GET_VALUE(false, KeepNObjects->GetReverseOrdering());

  // testing get and set macros or ReverseOrdering
  bool reverseOrdering = std::stoi(argv[5]);
  KeepNObjects->SetReverseOrdering(reverseOrdering);
  ITK_TEST_SET_GET_VALUE(reverseOrdering, KeepNObjects->GetReverseOrdering());

  // testing get and set macros for Attribute
  KeepNObjects->SetAttribute(LabelKeepNObjectsType::LabelObjectType::PERIMETER_ON_BORDER);
  ITK_TEST_SET_GET_VALUE(LabelKeepNObjectsType::LabelObjectType::PERIMETER_ON_BORDER, KeepNObjects->GetAttribute());

  const std::string attributeByName{ argv[6] };
  KeepNObjects->SetAttribute(attributeByName); // SetAttribute accepts a string for conversion to internal label code
  const LabelKeepNObjectsType::AttributeType attributeByCode = LabelKeepNObjectsType::LabelObjectType::LABEL;
  ITK_TEST_SET_GET_VALUE(attributeByCode, KeepNObjects->GetAttribute());

  itk::SimpleFilterWatcher watcher(KeepNObjects, "filter");

  using WriterType = itk::ImageFileWriter<IType>;
  auto writer = WriterType::New();
  writer->SetInput(KeepNObjects->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Test Complete!" << std::endl;

  return EXIT_SUCCESS;
}
