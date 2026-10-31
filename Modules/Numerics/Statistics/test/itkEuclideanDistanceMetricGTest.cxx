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

#include "itkEuclideanDistanceMetric.h"
#include "itkGTest.h"

TEST(EuclideanDistanceMetric, ConvertedLegacyTest)
{
  constexpr unsigned int MeasurementVectorSize{ 3 };

  using MeasurementVectorType = itk::Array<float>;

  using DistanceMetricType = itk::Statistics::EuclideanDistanceMetric<MeasurementVectorType>;

  auto distance = DistanceMetricType::New();

  std::cout << distance->GetNameOfClass() << std::endl;

  distance->Print(std::cout);

  MeasurementVectorType measurementNew;
  itk::NumericTraits<MeasurementVectorType>::SetLength(measurementNew, 3);
  measurementNew[0] = 2.5;
  measurementNew[1] = 3.3;
  measurementNew[2] = 4.0;

  // Attempting to compute distance before setting a measurement vector should
  // throw an exception

  EXPECT_THROW(distance->Evaluate(measurementNew), itk::ExceptionObject);

  distance->SetMeasurementVectorSize(MeasurementVectorSize);

  EXPECT_EQ(distance->GetMeasurementVectorSize(), MeasurementVectorSize) << "GetMeasurementVectorSize() Failed !";

  // Test if the distance computed is correct
  DistanceMetricType::OriginType origin;
  itk::NumericTraits<DistanceMetricType::OriginType>::SetLength(origin, 3);
  origin[0] = 1.5;
  origin[1] = 2.3;
  origin[2] = 1.0;
  distance->SetOrigin(origin);

  MeasurementVectorType measurement;
  itk::NumericTraits<MeasurementVectorType>::SetLength(measurement, 3);
  measurement[0] = 2.5;
  measurement[1] = 3.3;
  measurement[2] = 4.0;

  constexpr double trueValue{ 3.31662 };
  const double     distanceComputed = distance->Evaluate(measurement);
  constexpr double tolerance{ 0.001 };

  EXPECT_NEAR(distanceComputed, trueValue, tolerance) << "Distance computed not correct";

  // Compute distance between two measurement vectors
  MeasurementVectorType measurement2;
  itk::NumericTraits<MeasurementVectorType>::SetLength(measurement2, 3);
  measurement2[0] = 1.5;
  measurement2[1] = 3.5;
  measurement2[2] = 3.5;

  constexpr double trueValue2{ 1.136 };
  const double     distanceComputed2 = distance->Evaluate(measurement, measurement2);

  EXPECT_NEAR(distanceComputed2, trueValue2, tolerance) << "Distance computed not correct";
}
