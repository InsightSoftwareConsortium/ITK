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

#include <iostream>
#include "itkGaussianMembershipFunction.h"
#include "itkTestingMacros.h"

int
itkGaussianMembershipFunctionTest(int, char *[])
{
  constexpr unsigned int MeasurementVectorSize = 1;

  using MeasurementVectorType = itk::FixedArray<float, MeasurementVectorSize>;

  using MembershipFunctionType = itk::Statistics::GaussianMembershipFunction<MeasurementVectorType>;
  using MeasurementVectorSizeType = MembershipFunctionType::MeasurementVectorSizeType;

  auto function = MembershipFunctionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(function, GaussianMembershipFunction, MembershipFunctionBase);


  // Test if an exception will be thrown if we try to resize the measurement vector
  // size
  MeasurementVectorSizeType measurementVector2 = MeasurementVectorSize + 1;
  ITK_TRY_EXPECT_EXCEPTION(function->SetMeasurementVectorSize(measurementVector2));

  // Test non-square covariance matrix exception
  MembershipFunctionType::CovarianceMatrixType covariance;
  covariance.SetSize(MeasurementVectorSize, MeasurementVectorSize + 1);
  covariance.SetIdentity();
  ITK_TRY_EXPECT_EXCEPTION(function->SetCovariance(covariance));

  // Test covariance matrix and measurement vector size mismatch exception
  covariance.SetSize(MeasurementVectorSize + 1, MeasurementVectorSize + 1);
  covariance.SetIdentity();
  ITK_TRY_EXPECT_EXCEPTION(function->SetCovariance(covariance));

  covariance.SetSize(MeasurementVectorSize, MeasurementVectorSize);
  covariance.SetIdentity();
  function->SetCovariance(covariance);
  ITK_TEST_SET_GET_VALUE(covariance, function->GetCovariance());

  ITK_TEST_SET_GET_VALUE(covariance.GetInverse(), function->GetInverseCovariance().GetVnlMatrix());

  function->SetMeasurementVectorSize(MeasurementVectorSize);
  ITK_TEST_SET_GET_VALUE(MeasurementVectorSize, function->GetMeasurementVectorSize());

  // Test if the membership function value computed is correct
  MembershipFunctionType::MeanVectorType mean;
  itk::NumericTraits<MembershipFunctionType::MeanVectorType>::SetLength(mean, MeasurementVectorSize);
  mean[0] = 1.5;
  function->SetMean(mean);

  constexpr double tolerance = 0.001;

  if (itk::Math::abs(function->GetMean()[0] - mean[0]) > tolerance)
  {
    std::cerr << "Error in GetMean() method" << std::endl;
    return EXIT_FAILURE;
  }

  MeasurementVectorType measurement;
  itk::NumericTraits<MeasurementVectorType>::SetLength(measurement, MeasurementVectorSize);
  measurement[0] = 1.5;

  double trueValue = 0.3989;
  double distanceComputed = function->Evaluate(measurement);

  if (itk::Math::abs(distanceComputed - trueValue) > tolerance)
  {
    std::cerr << "Distance computed not correct: "
              << "truevalue= " << trueValue << ", ComputedValue=" << distanceComputed << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
