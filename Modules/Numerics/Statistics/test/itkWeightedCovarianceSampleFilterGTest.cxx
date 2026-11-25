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

#include "itkWeightedCovarianceSampleFilter.h"
#include "itkListSample.h"
#include "itkGTest.h"

namespace
{
constexpr unsigned int MeasurementVectorSize{ 3 };

using MeasurementVectorType = itk::FixedArray<float, MeasurementVectorSize>;

template <typename TSample>
class MyWeightedCovarianceSampleFilter : public itk::Statistics::WeightedCovarianceSampleFilter<TSample>
{
public:
  using Self = MyWeightedCovarianceSampleFilter;

  using Superclass = itk::Statistics::WeightedCovarianceSampleFilter<TSample>;

  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  using SampleType = TSample;

  itkNewMacro(Self);

  // method to invoke MakeOutput with index value different
  // from one or zero. This is to check if an exception will be
  // thrown
  void
  CreateInvalidOutput()
  {
    constexpr unsigned int index{ 3 };
    Superclass::MakeOutput(index);
  }
};
} // namespace


TEST(WeightedCovarianceSampleFilter, ConvertedLegacyTest)
{
  std::cout << "WeightedCovarianceSampleFilter test \n \n";

  using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;

  using FilterType = MyWeightedCovarianceSampleFilter<SampleType>;

  using MeasurementVectorRealType = FilterType::MeasurementVectorRealType;
  using CovarianceMatrixType = FilterType::MatrixType;

  auto filter = FilterType::New();

  MeasurementVectorType measure;

  auto sample = SampleType::New();

  sample->SetMeasurementVectorSize(MeasurementVectorSize);

  measure[0] = 4.00;
  measure[1] = 2.00;
  measure[2] = 0.60;
  sample->PushBack(measure);

  measure[0] = 4.20;
  measure[1] = 2.10;
  measure[2] = 0.59;
  sample->PushBack(measure);

  measure[0] = 3.90;
  measure[1] = 2.00;
  measure[2] = 0.58;
  sample->PushBack(measure);

  measure[0] = 4.30;
  measure[1] = 2.10;
  measure[2] = 0.62;
  sample->PushBack(measure);

  measure[0] = 4.10;
  measure[1] = 2.20;
  measure[2] = 0.63;
  sample->PushBack(measure);

  std::cout << filter->GetNameOfClass() << std::endl;
  filter->Print(std::cout);

  // Invoke update before adding an input. An exception should be
  // thrown.
  EXPECT_THROW(filter->Update(), itk::ExceptionObject)
    << "Exception should have been thrown since Update() is invoked without setting an input";

  EXPECT_EQ(filter->GetInput(), nullptr) << "GetInput() should return nullptr if the input has not been set";

  // test if exception is thrown if a derived class tries to create
  // an invalid output
  EXPECT_THROW(filter->CreateInvalidOutput(), itk::ExceptionObject) << "Exception should have been thrown: ";

  filter->ResetPipeline();

  // Run the filter with no weights
  filter->SetInput(sample);

  ASSERT_NO_THROW(filter->Update());

  MeasurementVectorRealType mean = filter->GetMean();
  CovarianceMatrixType      matrix = filter->GetCovarianceMatrix();

  std::cout << "Mean: " << mean << std::endl;
  std::cout << "Covariance Matrix: " << matrix << std::endl;

  // Check the results

  constexpr double epsilon{ 1e-2 };

  MeasurementVectorRealType meanExpected33;
  itk::NumericTraits<MeasurementVectorRealType>::SetLength(meanExpected33, MeasurementVectorSize);
  meanExpected33[0] = 4.10;
  meanExpected33[1] = 2.08;
  meanExpected33[2] = 0.604;

  for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
  {
    EXPECT_NEAR(meanExpected33[i], mean[i], epsilon) << "The computed mean value is incorrect";
  }

  CovarianceMatrixType matrixExpected33(MeasurementVectorSize, MeasurementVectorSize);

  matrixExpected33[0][0] = 0.025;
  matrixExpected33[0][1] = 0.0075;
  matrixExpected33[0][2] = 0.00175;

  matrixExpected33[1][0] = 0.0075;
  matrixExpected33[1][1] = 0.0070;
  matrixExpected33[1][2] = 0.00135;

  matrixExpected33[2][0] = 0.00175;
  matrixExpected33[2][1] = 0.00135;
  matrixExpected33[2][2] = 0.00043;

  for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
  {
    for (unsigned int j = 0; j < MeasurementVectorSize; ++j)
      EXPECT_NEAR(matrixExpected33[i][j], matrix[i][j], epsilon) << "Computed covariance matrix value is incorrect";
  }

  // Specify weight
  using WeightArrayType = FilterType::WeightArrayType;
  WeightArrayType weightArray(sample->Size(), 1.0);

  filter->SetWeights(weightArray);

  // run with equal weights
  ASSERT_NO_THROW(filter->Update());


  mean = filter->GetMean();
  matrix = filter->GetCovarianceMatrix();

  std::cout << "Mean: " << mean << std::endl;
  std::cout << "Covariance Matrix: " << matrix << std::endl;

  MeasurementVectorRealType meanExpected3;

  itk::NumericTraits<MeasurementVectorRealType>::SetLength(meanExpected3, MeasurementVectorSize);
  meanExpected3[0] = 4.10;
  meanExpected3[1] = 2.08;
  meanExpected3[2] = 0.604;

  for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
  {
    EXPECT_NEAR(meanExpected3[i], mean[i], epsilon) << "The computed mean value is incorrect";
  }

  CovarianceMatrixType matrixExpected(MeasurementVectorSize, MeasurementVectorSize);

  matrixExpected[0][0] = 0.025;
  matrixExpected[0][1] = 0.0075;
  matrixExpected[0][2] = 0.00175;

  matrixExpected[1][0] = 0.0075;
  matrixExpected[1][1] = 0.0070;
  matrixExpected[1][2] = 0.00135;

  matrixExpected[2][0] = 0.00175;
  matrixExpected[2][1] = 0.00135;
  matrixExpected[2][2] = 0.00043;

  for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
  {
    for (unsigned int j = 0; j < MeasurementVectorSize; ++j)
    {
      EXPECT_NEAR(matrixExpected[i][j], matrix[i][j], epsilon) << "Computed covariance matrix value is incorrect";
    }
  }

  filter->SetWeights(weightArray);

  ASSERT_NO_THROW(filter->Update());

  mean = filter->GetMean();
  matrix = filter->GetCovarianceMatrix();

  std::cout << "Mean: " << mean << std::endl;
  std::cout << "Covariance Matrix: " << matrix << std::endl;

  for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
  {
    EXPECT_NEAR(meanExpected3[i], mean[i], epsilon) << "The computed mean value is incorrect";
  }

  for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
  {
    for (unsigned int j = 0; j < MeasurementVectorSize; ++j)
    {
      EXPECT_NEAR(matrixExpected[i][j], matrix[i][j], epsilon) << "Computed covariance matrix value is incorrect";
    }
  }

  // Class is defined only for this function.
  class WeightedCovarianceSampleTestFunction1 : public itk::FunctionBase<MeasurementVectorType, double>
  {
  public:
    /** Standard class type aliases. */
    using Self = WeightedCovarianceSampleTestFunction1;
    using Pointer = itk::SmartPointer<Self>;

    /** Standard macros. */
    itkOverrideGetNameOfClassMacro(WeightedCovarianceSampleTestFunction1);
    itkNewMacro(Self);

    /** Input type */
    using InputType = MeasurementVectorType;

    /** Output type */
    using OutputType = double;

    /**Evaluate at the specified input position */
    OutputType
    Evaluate(const InputType & itkNotUsed(input)) const override
    {
      // MeasurementVectorType measurements;
      // set the weight factor of the measurement
      // vector with valuev[2, 2] to 0.5.
      return 1.0;
    }

  protected:
    WeightedCovarianceSampleTestFunction1() = default;
    ~WeightedCovarianceSampleTestFunction1() override = default;
  }; // end of class

  // set  a constant 1.0 weight using a function
  auto weightFunction = WeightedCovarianceSampleTestFunction1::New();
  filter->SetWeightingFunction(weightFunction);

  ASSERT_NO_THROW(filter->Update());

  mean = filter->GetMean();
  matrix = filter->GetCovarianceMatrix();

  std::cout << "Mean: " << mean << std::endl;
  std::cout << "Covariance Matrix: " << matrix << std::endl;

  for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
  {
    EXPECT_NEAR(meanExpected3[i], mean[i], epsilon) << "The computed mean value is incorrect";
  }

  for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
  {
    for (unsigned int j = 0; j < MeasurementVectorSize; ++j)
    {
      EXPECT_NEAR(matrixExpected[i][j], matrix[i][j], epsilon) << "Computed covariance matrix value is incorrect";
    }
  }
}
