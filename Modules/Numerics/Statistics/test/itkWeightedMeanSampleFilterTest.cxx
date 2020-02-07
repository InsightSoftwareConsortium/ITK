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

#include "itkWeightedMeanSampleFilter.h"
#include "itkListSample.h"

constexpr unsigned int MeasurementVectorSize = 2;

using MeasurementVectorType = itk::FixedArray<float, MeasurementVectorSize>;

class WeightedMeanTestFunction : public itk::FunctionBase<MeasurementVectorType, double>
{
public:
  /** Standard class type aliases. */
  using Self = WeightedMeanTestFunction;

  using Superclass = itk::FunctionBase<MeasurementVectorType, double>;

  using Pointer = itk::SmartPointer<Self>;

  using ConstPointer = itk::SmartPointer<const Self>;

  /** Standard macros. */
  itkTypeMacro(WeightedMeanTestFunction, FunctionBase);
  itkNewMacro(Self);

  /** Input type */
  using InputType = MeasurementVectorType;

  /** Output type */
  using OutputType = double;

  /**Evaluate at the specified input position */
  OutputType
  Evaluate(const InputType & input) const override
  {
    MeasurementVectorType measurements;
    // set the weight factor of the measurement
    // vector with valuev[2, 2] to 0.5.
    measurements.Fill(2.0f);
    if (input != measurements)
    {
      return 0.5;
    }
    else
    {
      return 1.0;
    }
  }

protected:
  WeightedMeanTestFunction() = default;
  ~WeightedMeanTestFunction() override = default;
}; // end of class


int
itkWeightedMeanSampleFilterTest(int, char *[])
{
  std::cout << "WeightedMeanSampleFilter test \n \n";

  constexpr unsigned int numberOfMeasurementVectors = 5;
  unsigned int           counter;

  using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;

  SampleType::Pointer sample = SampleType::New();

  sample->SetMeasurementVectorSize(MeasurementVectorSize);

  MeasurementVectorType measure;

  // reset counter
  counter = 0;

  while (counter < numberOfMeasurementVectors)
  {
    for (unsigned int i = 0; i < MeasurementVectorSize; i++)
    {
      measure[i] = counter;
    }
    sample->PushBack(measure);
    counter++;
  }

  using FilterType = itk::Statistics::WeightedMeanSampleFilter<SampleType>;

  FilterType::Pointer filter = FilterType::New();

  std::cout << filter->GetNameOfClass() << std::endl;
  filter->Print(std::cout);

  // Invoke update before adding an input. An exception should be
  // thrown.
  try
  {
    filter->Update();
    std::cerr << "Exception should have been thrown since \
                    Update() is invoked without setting an input "
              << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught: " << excp << std::endl;
  }

  if (filter->GetInput() != nullptr)
  {
    std::cerr << "GetInput() should return nullptr if the input \
                     has not been set"
              << std::endl;
    return EXIT_FAILURE;
  }

  filter->ResetPipeline();
  filter->SetInput(sample);

  // run the filters without weighting coefficients
  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught: " << excp << std::endl;
  }

  const FilterType::MeasurementVectorDecoratedType * decorator = filter->GetOutput();
  FilterType::MeasurementVectorRealType              meanOutput = decorator->Get();

  FilterType::MeasurementVectorRealType mean;

  mean[0] = 2.0;
  mean[1] = 2.0;

  FilterType::MeasurementVectorType::ValueType epsilon = 1e-6;

  if ((std::fabs(meanOutput[0] - mean[0]) > epsilon) || (std::fabs(meanOutput[1] - mean[1]) > epsilon))
  {
    std::cerr << "Wrong result " << std::endl;
    std::cerr << meanOutput[0] << " " << mean[0] << " " << meanOutput[1] << " " << mean[1] << " " << std::endl;
    std::cerr << "The result is not what is expected" << std::endl;
    return EXIT_FAILURE;
  }

  using WeightArrayType = FilterType::WeightArrayType;
  WeightArrayType weightArray(sample->Size());
  weightArray.Fill(1.0);

  filter->SetWeights(weightArray);

  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught: " << excp << std::endl;
  }

  decorator = filter->GetOutput();
  meanOutput = decorator->Get();

  mean[0] = 2.0;
  mean[1] = 2.0;

  if ((std::fabs(meanOutput[0] - mean[0]) > epsilon) || (std::fabs(meanOutput[1] - mean[1]) > epsilon))
  {
    std::cerr << "Wrong result " << std::endl;
    std::cerr << meanOutput[0] << " " << mean[0] << " " << meanOutput[1] << " " << mean[1] << " " << std::endl;
    std::cerr << "The result is not what is expected" << std::endl;
    return EXIT_FAILURE;
  }

  // change the weight of the last element to 0.5 and recompute
  weightArray[numberOfMeasurementVectors - 1] = 0.5;
  filter->SetWeights(weightArray);

  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught: " << excp << std::endl;
  }

  decorator = filter->GetOutput();
  meanOutput = decorator->Get();

  mean[0] = 1.7777778;
  mean[1] = 1.7777778;

  if ((std::fabs(meanOutput[0] - mean[0]) > epsilon) || (std::fabs(meanOutput[1] - mean[1]) > epsilon))
  {
    std::cerr << "Wrong result" << std::endl;
    std::cerr << meanOutput[0] << " " << mean[0] << " " << meanOutput[1] << " " << mean[1] << " " << std::endl;

    std::cerr << "The result is not what is expected" << std::endl;
    return EXIT_FAILURE;
  }

  // set the weight using a function
  WeightedMeanTestFunction::Pointer weightFunction = WeightedMeanTestFunction::New();
  filter->SetWeightingFunction(weightFunction);

  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught: " << excp << std::endl;
  }

  decorator = filter->GetOutput();
  meanOutput = decorator->Get();

  mean[0] = 2.0;
  mean[1] = 2.0;

  if ((std::fabs(meanOutput[0] - mean[0]) > epsilon) || (std::fabs(meanOutput[1] - mean[1]) > epsilon))
  {
    std::cerr << "Wrong result" << std::endl;
    std::cerr << meanOutput[0] << " " << mean[0] << " " << meanOutput[1] << " " << mean[1] << " " << std::endl;
    std::cerr << "The result is not what is expected" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
