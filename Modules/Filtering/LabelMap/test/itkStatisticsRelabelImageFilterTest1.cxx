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

#include "itkStatisticsRelabelImageFilter.h"

#include "itkTestingMacros.h"

int
itkStatisticsRelabelImageFilterTest1(int argc, char * argv[])
{

  if (argc != 7)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input feature output";
    std::cerr << " background";
    std::cerr << " reverseOrdering attribute" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using IType = itk::Image<unsigned char, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  auto reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  using RelabelType = itk::StatisticsRelabelImageFilter<IType, IType>;
  auto statisticsRelabel = RelabelType::New();

  statisticsRelabel->SetInput(reader->GetOutput());
  statisticsRelabel->SetFeatureImage(reader2->GetOutput());

  // testing get/set BackgroundValue macro
  int BackgroundValue = (std::stoi(argv[4]));
  statisticsRelabel->SetBackgroundValue(BackgroundValue);
  ITK_TEST_SET_GET_VALUE(BackgroundValue, statisticsRelabel->GetBackgroundValue());

  // testing boolean macro for ReverseOrdering
  statisticsRelabel->ReverseOrderingOn();
  ITK_TEST_SET_GET_VALUE(true, statisticsRelabel->GetReverseOrdering());

  statisticsRelabel->ReverseOrderingOff();
  ITK_TEST_SET_GET_VALUE(false, statisticsRelabel->GetReverseOrdering());

  // testing get and set macros or ReverseOrdering
  bool reverseOrdering = std::stoi(argv[5]);
  statisticsRelabel->SetReverseOrdering(reverseOrdering);
  ITK_TEST_SET_GET_VALUE(reverseOrdering, statisticsRelabel->GetReverseOrdering());

  // testing get and set macros for Attribute
  RelabelType::AttributeType attribute = std::stoi(argv[6]);
  statisticsRelabel->SetAttribute(attribute);
  ITK_TEST_SET_GET_VALUE(attribute, statisticsRelabel->GetAttribute());

  itk::SimpleFilterWatcher watcher(statisticsRelabel, "filter");

  using WriterType = itk::ImageFileWriter<IType>;
  auto writer = WriterType::New();
  writer->SetInput(statisticsRelabel->GetOutput());
  writer->SetFileName(argv[3]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Test Complete!" << std::endl;

  return EXIT_SUCCESS;
}
