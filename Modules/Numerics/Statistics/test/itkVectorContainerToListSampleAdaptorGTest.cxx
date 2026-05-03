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

#include "itkGTest.h"


TEST(VectorContainerToListSampleAdaptor, ConvertedLegacyTest)
{
  using VectorType = itk::Vector<double, 5>;

  using ContainerType = itk::VectorContainer<VectorType>;

  using AdaptorType = itk::Statistics::VectorContainerToListSampleAdaptor<ContainerType>;

  auto adaptor = AdaptorType::New();

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(adaptor, VectorContainerToListSampleAdaptor, ListSample);

  // Test the exceptions
  EXPECT_THROW(adaptor->Size(), itk::ExceptionObject);

  constexpr typename AdaptorType::InstanceIdentifier instance = 0;
  EXPECT_THROW(adaptor->GetMeasurementVector(instance), itk::ExceptionObject);

  EXPECT_THROW(adaptor->GetFrequency(instance), itk::ExceptionObject);

  EXPECT_THROW(adaptor->GetTotalFrequency(), itk::ExceptionObject);

  // Set the vector container
  constexpr unsigned int containerSize{ 3 };
  auto                   container = ContainerType::New();
  container->Reserve(containerSize);
  for (unsigned int i = 0; i < container->Size(); ++i)
  {
    auto vector = itk::MakeFilled<VectorType>(std::pow(i, 2));
    container->InsertElement(i, vector);
  }

  adaptor->SetVectorContainer(container);
  EXPECT_EQ(container, adaptor->GetVectorContainer());

  constexpr typename AdaptorType::InstanceIdentifier expectedSize = 3;
  const typename AdaptorType::InstanceIdentifier     size = adaptor->Size();
  EXPECT_EQ(expectedSize, size);

  constexpr typename AdaptorType::AbsoluteFrequencyType expectedFreq = 1;
  const typename AdaptorType::AbsoluteFrequencyType     freq = adaptor->GetFrequency(instance);
  EXPECT_EQ(expectedFreq, freq);

  constexpr typename AdaptorType::TotalAbsoluteFrequencyType expectedTotalFreq = 3;
  const typename AdaptorType::TotalAbsoluteFrequencyType     totalFreq = adaptor->GetTotalFrequency();
  EXPECT_EQ(expectedTotalFreq, totalFreq);
}
