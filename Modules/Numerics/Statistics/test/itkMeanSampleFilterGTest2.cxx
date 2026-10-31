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

#include "itkMeanSampleFilter.h"
#include "itkListSample.h"
#include "itkGTest.h"

TEST(MeanSampleFilter, ConvertedLegacyTest2)
{
  std::cout << "MeanSampleFilter test \n \n";

  constexpr unsigned int MeasurementVectorSize{ 2 };
  constexpr unsigned int numberOfMeasurementVectors{ 2 };

  using MeasurementVectorType = itk::FixedArray<int, MeasurementVectorSize>;

  using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;

  auto sample = SampleType::New();

  sample->SetMeasurementVectorSize(MeasurementVectorSize);

  MeasurementVectorType measure;

  unsigned int counter = 0;

  std::cout << "Input sample values " << std::endl;
  while (counter < numberOfMeasurementVectors)
  {
    for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
    {
      measure[i] = counter;
    }
    std::cout << counter << " : " << measure << std::endl;
    sample->PushBack(measure);
    counter++;
  }

  using FilterType = itk::Statistics::MeanSampleFilter<SampleType>;

  auto filter = FilterType::New();

  filter->SetInput(sample);

  ASSERT_NO_THROW(filter->Update());

  const FilterType::MeasurementVectorDecoratedType * decorator = filter->GetOutput();
  FilterType::MeasurementVectorRealType              meanOutput = decorator->Get();

  FilterType::MeasurementVectorRealType expectedMean;

  expectedMean[0] = 0.5;
  expectedMean[1] = 0.5;

  std::cout << meanOutput[0] << ' ' << expectedMean[0] << ' ' << meanOutput[1] << ' ' << expectedMean[1] << ' '
            << std::endl;

  // FilterType::MeasurementVectorType::ValueType is an int in this case
  constexpr FilterType::MeasurementVectorType::ValueType epsilon{ 0 };

  EXPECT_NEAR(meanOutput[0], expectedMean[0], epsilon) << "The result is not what is expected";
  EXPECT_NEAR(meanOutput[1], expectedMean[1], epsilon) << "The result is not what is expected";
}
