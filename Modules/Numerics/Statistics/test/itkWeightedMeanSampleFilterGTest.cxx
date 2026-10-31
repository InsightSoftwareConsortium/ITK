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

#include "itkWeightedMeanSampleFilter.h"
#include "itkListSample.h"
#include "itkGTest.h"

namespace
{
constexpr unsigned int MeasurementVectorSize{ 2 };

using MeasurementVectorType = itk::FixedArray<float, MeasurementVectorSize>;

class WeightedMeanTestFunction : public itk::FunctionBase<MeasurementVectorType, double>
{
public:
  /** Standard class type aliases. */
  using Self = WeightedMeanTestFunction;

  using Superclass = itk::FunctionBase<MeasurementVectorType, double>;

  using Pointer = itk::SmartPointer<Self>;

  using ConstPointer = itk::SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(WeightedMeanTestFunction);
  itkNewMacro(Self);

  /** Input type */
  using InputType = MeasurementVectorType;

  /** Output type */
  using OutputType = double;

  /**Evaluate at the specified input position */
  OutputType
  Evaluate(const InputType & input) const override
  {
    auto measurements = itk::MakeFilled<MeasurementVectorType>(2.0f);
    // set the weight factor of the measurement
    // vector with valuev[2, 2] to 0.5.
    if (input != measurements)
    {
      return 0.5;
    }

    return 1.0;
  }

protected:
  WeightedMeanTestFunction() = default;
  ~WeightedMeanTestFunction() override = default;
}; // end of class
} // namespace

TEST(WeightedMeanSampleFilter, ConvertedLegacyTest)
{
  std::cout << "WeightedMeanSampleFilter test \n \n";

  constexpr unsigned int numberOfMeasurementVectors{ 5 };

  using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;

  auto sample = SampleType::New();

  sample->SetMeasurementVectorSize(MeasurementVectorSize);

  MeasurementVectorType measure;

  // reset counter
  unsigned int counter = 0;

  while (counter < numberOfMeasurementVectors)
  {
    for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
    {
      measure[i] = counter;
    }
    sample->PushBack(measure);
    counter++;
  }

  using FilterType = itk::Statistics::WeightedMeanSampleFilter<SampleType>;

  auto filter = FilterType::New();

  std::cout << filter->GetNameOfClass() << std::endl;
  filter->Print(std::cout);

  // Invoke update before adding an input. An exception should be
  // thrown.
  EXPECT_THROW(filter->Update(), itk::ExceptionObject)
    << "Exception should have been thrown since Update() is invoked without setting an input ";

  EXPECT_EQ(filter->GetInput(), nullptr) << "GetInput() should return nullptr if the input has not been set";

  filter->ResetPipeline();
  filter->SetInput(sample);

  // run the filters without weighting coefficients
  ASSERT_NO_THROW(filter->Update());

  const FilterType::MeasurementVectorDecoratedType * decorator = filter->GetOutput();
  FilterType::MeasurementVectorRealType              meanOutput = decorator->Get();

  FilterType::MeasurementVectorRealType mean;

  mean[0] = 2.0;
  mean[1] = 2.0;

  constexpr FilterType::MeasurementVectorType::ValueType epsilon{ 1e-6 };

  EXPECT_NEAR(meanOutput[0], mean[0], epsilon) << "The result is not what is expected";
  EXPECT_NEAR(meanOutput[1], mean[1], epsilon) << "The result is not what is expected";

  using WeightArrayType = FilterType::WeightArrayType;
  WeightArrayType weightArray(sample->Size(), 1.0);

  filter->SetWeights(weightArray);

  ASSERT_NO_THROW(filter->Update());

  decorator = filter->GetOutput();
  meanOutput = decorator->Get();

  mean[0] = 2.0;
  mean[1] = 2.0;

  EXPECT_NEAR(meanOutput[0], mean[0], epsilon) << "The result is not what is expected";
  EXPECT_NEAR(meanOutput[1], mean[1], epsilon) << "The result is not what is expected";

  // change the weight of the last element to 0.5 and recompute
  weightArray[numberOfMeasurementVectors - 1] = 0.5;
  filter->SetWeights(weightArray);

  ASSERT_NO_THROW(filter->Update());

  decorator = filter->GetOutput();
  meanOutput = decorator->Get();

  mean[0] = 1.7777778;
  mean[1] = 1.7777778;

  EXPECT_NEAR(meanOutput[0], mean[0], epsilon) << "The result is not what is expected";
  EXPECT_NEAR(meanOutput[1], mean[1], epsilon) << "The result is not what is expected";

  // set the weight using a function
  auto weightFunction = WeightedMeanTestFunction::New();
  filter->SetWeightingFunction(weightFunction);

  ASSERT_NO_THROW(filter->Update());

  decorator = filter->GetOutput();
  meanOutput = decorator->Get();

  mean[0] = 2.0;
  mean[1] = 2.0;

  EXPECT_NEAR(meanOutput[0], mean[0], epsilon) << "The result is not what is expected";
  EXPECT_NEAR(meanOutput[1], mean[1], epsilon) << "The result is not what is expected";
}
