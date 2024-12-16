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

#include "itkVectorContainerToListSampleAdaptor.h"
#include "itkTestingMacros.h"

int
itkVectorContainerToListSampleAdaptorTest(int, char *[])
{

  using VectorType = itk::Vector<double, 5>;

  using ContainerType = itk::VectorContainer<VectorType>;

  using AdaptorType = itk::Statistics::VectorContainerToListSampleAdaptor<ContainerType>;

  auto adaptor = AdaptorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(adaptor, VectorContainerToListSampleAdaptor, ListSample);

  // Test the exceptions
  ITK_TRY_EXPECT_EXCEPTION(adaptor->Size());

  constexpr typename AdaptorType::InstanceIdentifier instance = 0;
  ITK_TRY_EXPECT_EXCEPTION(adaptor->GetMeasurementVector(instance));

  ITK_TRY_EXPECT_EXCEPTION(adaptor->GetFrequency(instance));

  ITK_TRY_EXPECT_EXCEPTION(adaptor->GetTotalFrequency());

  // Set the vector container
  constexpr unsigned int containerSize = 3;
  auto                   container = ContainerType::New();
  container->Reserve(containerSize);
  for (unsigned int i = 0; i < container->Size(); ++i)
  {
    auto vector = itk::MakeFilled<VectorType>(std::pow(i, 2));
    container->InsertElement(i, vector);
  }

  adaptor->SetVectorContainer(container);
  ITK_TEST_SET_GET_VALUE(container, adaptor->GetVectorContainer());

  constexpr typename AdaptorType::InstanceIdentifier expectedSize = 3;
  const typename AdaptorType::InstanceIdentifier     size = adaptor->Size();
  ITK_TEST_EXPECT_EQUAL(expectedSize, size);

  constexpr typename AdaptorType::AbsoluteFrequencyType expectedFreq = 1;
  const typename AdaptorType::AbsoluteFrequencyType     freq = adaptor->GetFrequency(instance);
  ITK_TEST_EXPECT_EQUAL(expectedFreq, freq);

  constexpr typename AdaptorType::TotalAbsoluteFrequencyType expectedTotalFreq = 3;
  const typename AdaptorType::TotalAbsoluteFrequencyType     totalFreq = adaptor->GetTotalFrequency();
  ITK_TEST_EXPECT_EQUAL(expectedTotalFreq, totalFreq);


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
