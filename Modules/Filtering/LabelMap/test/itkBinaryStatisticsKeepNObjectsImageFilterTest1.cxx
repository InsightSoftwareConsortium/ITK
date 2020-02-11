/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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

#include "itkBinaryStatisticsKeepNObjectsImageFilter.h"

#include "itkTestingMacros.h"

int
itkBinaryStatisticsKeepNObjectsImageFilterTest1(int argc, char * argv[])
{

  if (argc != 10)
  {
    std::cerr << "Usage: " << argv[0] << " input feature output";
    std::cerr << " foreground background numberOfObjectsToKeep";
    std::cerr << "reverseOrdering connectivity attribute" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using IType = itk::Image<unsigned char, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  using BinaryKeepNObjectsType = itk::BinaryStatisticsKeepNObjectsImageFilter<IType, IType>;
  BinaryKeepNObjectsType::Pointer KeepNObjects = BinaryKeepNObjectsType::New();

  KeepNObjects->SetInput(reader->GetOutput());
  KeepNObjects->SetFeatureImage(reader2->GetOutput());

  // testing get/set ForegroundValue macro
  int ForegroundValue = (std::stoi(argv[4]));
  KeepNObjects->SetForegroundValue(ForegroundValue);
  ITK_TEST_SET_GET_VALUE(ForegroundValue, KeepNObjects->GetForegroundValue());

  // testing get/set BackgroundValue macro
  int BackgroundValue = (std::stoi(argv[5]));
  KeepNObjects->SetBackgroundValue(BackgroundValue);
  ITK_TEST_SET_GET_VALUE(BackgroundValue, KeepNObjects->GetBackgroundValue());

  // testing get and set macros for NumberOfObjects
  unsigned int numberOfObjects = std::stoi(argv[6]);
  KeepNObjects->SetNumberOfObjects(numberOfObjects);
  ITK_TEST_SET_GET_VALUE(numberOfObjects, KeepNObjects->GetNumberOfObjects());

  // testing boolean macro for ReverseOrdering
  KeepNObjects->ReverseOrderingOn();
  ITK_TEST_SET_GET_VALUE(true, KeepNObjects->GetReverseOrdering());

  KeepNObjects->ReverseOrderingOff();
  ITK_TEST_SET_GET_VALUE(false, KeepNObjects->GetReverseOrdering());

  // testing get and set macros or ReverseOrdering
  bool reverseOrdering = std::stoi(argv[7]);
  KeepNObjects->SetReverseOrdering(reverseOrdering);
  ITK_TEST_SET_GET_VALUE(reverseOrdering, KeepNObjects->GetReverseOrdering());

  // testing boolean macro for FullyConnected
  KeepNObjects->FullyConnectedOn();
  ITK_TEST_SET_GET_VALUE(true, KeepNObjects->GetFullyConnected());

  KeepNObjects->FullyConnectedOff();
  ITK_TEST_SET_GET_VALUE(false, KeepNObjects->GetFullyConnected());

  // testing get and set macros or FullyConnected
  bool fullyConnected = std::stoi(argv[8]);
  KeepNObjects->SetFullyConnected(fullyConnected);
  ITK_TEST_SET_GET_VALUE(fullyConnected, KeepNObjects->GetFullyConnected());

  // testing get and set macros for Attribute
  BinaryKeepNObjectsType::AttributeType attribute = std::stoi(argv[9]);
  KeepNObjects->SetAttribute(attribute);
  ITK_TEST_SET_GET_VALUE(attribute, KeepNObjects->GetAttribute());

  itk::SimpleFilterWatcher watcher(KeepNObjects, "filter");

  using WriterType = itk::ImageFileWriter<IType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(KeepNObjects->GetOutput());
  writer->SetFileName(argv[3]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Test Complete!" << std::endl;

  return EXIT_SUCCESS;
}
