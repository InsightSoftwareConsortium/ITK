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

#include "itkMahalanobisDistanceMetric.h"

int
itkMahalanobisDistanceMetricTest(int, char *[])
{
  constexpr unsigned int MeasurementVectorSize = 3;

  using MeasurementVectorType = itk::Array<float>;

  using DistanceMetricType = itk::Statistics::MahalanobisDistanceMetric<MeasurementVectorType>;

  auto distance = DistanceMetricType::New();

  std::cout << distance->GetNameOfClass() << std::endl;

  distance->Print(std::cout);

  distance->SetMeasurementVectorSize(MeasurementVectorSize);

  if (distance->GetMeasurementVectorSize() != MeasurementVectorSize)
  {
    std::cerr << "GetMeasurementVectorSize() Failed !" << std::endl;
    return EXIT_FAILURE;
  }

  // Test if the distance computed is correct
  DistanceMetricType::OriginType origin;
  itk::NumericTraits<DistanceMetricType::OriginType>::SetLength(origin, 3);
  origin[0] = 1.5;
  origin[1] = 2.3;
  origin[2] = 1.0;
  distance->SetMean(origin);

  // double value comparison tolerance
  constexpr double tolerance = 0.001;
  if (itk::Math::abs(distance->GetMean()[0] - origin[0]) > tolerance ||
      itk::Math::abs(distance->GetMean()[1] - origin[1]) > tolerance ||
      itk::Math::abs(distance->GetMean()[2] - origin[2]) > tolerance)
  {
    std::cerr << " Set/Get Origin error " << std::endl;
    return EXIT_FAILURE;
  }

  MeasurementVectorType measurement;
  itk::NumericTraits<MeasurementVectorType>::SetLength(measurement, 3);
  measurement[0] = 2.5;
  measurement[1] = 3.3;
  measurement[2] = 4.0;

  double trueValue = 3.31662;
  double distanceComputed = distance->Evaluate(measurement);

  if (itk::Math::abs(distanceComputed - trueValue) > tolerance)
  {
    std::cerr << "Distance computed not correct: "
              << "truevalue= " << trueValue << "ComputedValue=" << distanceComputed << std::endl;
    return EXIT_FAILURE;
  }

  // Test if we get the same result with identity covariance matrix set
  DistanceMetricType::CovarianceMatrixType covarianceMatrix;
  covarianceMatrix.set_size(MeasurementVectorSize, MeasurementVectorSize);
  covarianceMatrix.set_identity();
  distance->SetCovariance(covarianceMatrix);

  if (distance->GetCovariance() != covarianceMatrix)
  {
    std::cerr << "Get/SetCovariance method error" << std::endl;
    return EXIT_FAILURE;
  }

  double epsilon = 1e-200;
  double doubleMax = 1e+25;

  distance->SetEpsilon(epsilon);
  distance->SetDoubleMax(doubleMax);

  // Test Set/Get Epsilon method
  if (itk::Math::abs(distance->GetEpsilon() - epsilon) > tolerance)
  {
    std::cerr << "Get/SetEpsilon method error" << std::endl;
    return EXIT_FAILURE;
  }

  // Test Set/Get DoubleMax method
  if (itk::Math::abs(distance->GetDoubleMax() - doubleMax) > tolerance)
  {
    std::cerr << "Get/SetDoubleMax method error" << std::endl;
    return EXIT_FAILURE;
  }

  if (itk::Math::abs(distanceComputed - trueValue) > tolerance)
  {
    std::cerr << "Distance computed not correct: "
              << "truevalue= " << trueValue << "ComputedValue=" << distanceComputed << std::endl;
    return EXIT_FAILURE;
  }


  // Test if an exception is thrown if a covariance matrix is set with different
  // size
  DistanceMetricType::CovarianceMatrixType      covarianceMatrix2;
  DistanceMetricType::MeasurementVectorSizeType measurementSize2 = 4;
  covarianceMatrix2.set_size(measurementSize2, measurementSize2);

  try
  {
    distance->SetCovariance(covarianceMatrix2);
    std::cerr << "Exception should have been thrown: " << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & excpt)
  {
    std::cerr << "Exception caught: " << excpt << std::endl;
  }

  // Set a covariance matrix and check if the computed inverse matrix is
  // correct
  //
  DistanceMetricType::CovarianceMatrixType covarianceMatrix3;
  covarianceMatrix3.set_size(MeasurementVectorSize, MeasurementVectorSize);
  covarianceMatrix3[0][0] = 2.0;
  covarianceMatrix3[0][1] = 1.4;
  covarianceMatrix3[0][2] = 5.0;

  covarianceMatrix3[1][0] = 3.0;
  covarianceMatrix3[1][1] = 2.0;
  covarianceMatrix3[1][2] = 5.4;

  covarianceMatrix3[2][0] = 3.2;
  covarianceMatrix3[2][1] = 1.4;
  covarianceMatrix3[2][2] = 7.4;

  distance->SetCovariance(covarianceMatrix3);

  // establish the true inverse covariance matrix
  DistanceMetricType::CovarianceMatrixType trueInverseCovarianceMatrix;
  trueInverseCovarianceMatrix.set_size(MeasurementVectorSize, MeasurementVectorSize);

  trueInverseCovarianceMatrix[0][0] = -2.124;
  trueInverseCovarianceMatrix[0][1] = 0.986;
  trueInverseCovarianceMatrix[0][2] = 0.716;

  trueInverseCovarianceMatrix[1][0] = 1.444;
  trueInverseCovarianceMatrix[1][1] = 0.352;
  trueInverseCovarianceMatrix[1][2] = -1.232;

  trueInverseCovarianceMatrix[2][0] = 0.646;
  trueInverseCovarianceMatrix[2][1] = -0.493;
  trueInverseCovarianceMatrix[2][2] = 0.059;

  // Get the computed inverse covariance matrix
  DistanceMetricType::CovarianceMatrixType computedInverseCovarianceMatrix;
  computedInverseCovarianceMatrix = distance->GetInverseCovariance();

  if (itk::Math::abs(trueInverseCovarianceMatrix[0][0] - computedInverseCovarianceMatrix[0][0]) > tolerance ||
      itk::Math::abs(trueInverseCovarianceMatrix[0][1] - computedInverseCovarianceMatrix[0][1]) > tolerance ||
      itk::Math::abs(trueInverseCovarianceMatrix[0][2] - computedInverseCovarianceMatrix[0][2]) > tolerance ||
      itk::Math::abs(trueInverseCovarianceMatrix[1][0] - computedInverseCovarianceMatrix[1][0]) > tolerance ||
      itk::Math::abs(trueInverseCovarianceMatrix[1][1] - computedInverseCovarianceMatrix[1][1]) > tolerance ||
      itk::Math::abs(trueInverseCovarianceMatrix[1][2] - computedInverseCovarianceMatrix[1][2]) > tolerance ||
      itk::Math::abs(trueInverseCovarianceMatrix[2][0] - computedInverseCovarianceMatrix[2][0]) > tolerance ||
      itk::Math::abs(trueInverseCovarianceMatrix[2][1] - computedInverseCovarianceMatrix[2][1]) > tolerance ||
      itk::Math::abs(trueInverseCovarianceMatrix[2][2] - computedInverseCovarianceMatrix[2][2]) > tolerance)
  {
    std::cerr << "Inverse computation error" << std::endl;
    return EXIT_FAILURE;
  }

  // Run the distance metric with a single component measurement vector size
  DistanceMetricType::MeasurementVectorSizeType singleComponentMeasurementVectorSize = 1;

  distance->SetMeasurementVectorSize(singleComponentMeasurementVectorSize);

  itk::NumericTraits<DistanceMetricType::OriginType>::SetLength(origin, 1);
  origin[0] = 1.5;
  distance->SetMean(origin);

  if (itk::Math::abs(distance->GetMean()[0] - origin[0]) > tolerance)
  {
    std::cerr << " Set/Get Origin error " << std::endl;
    return EXIT_FAILURE;
  }
  covarianceMatrix.set_size(singleComponentMeasurementVectorSize, singleComponentMeasurementVectorSize);
  covarianceMatrix[0][0] = 1.0;
  distance->SetCovariance(covarianceMatrix);

  MeasurementVectorType measurementSingleComponent;
  itk::NumericTraits<MeasurementVectorType>::SetLength(measurementSingleComponent, 1);
  measurementSingleComponent[0] = 2.5;

  trueValue = 1.0;
  distanceComputed = distance->Evaluate(measurementSingleComponent);

  if (itk::Math::abs(distanceComputed - trueValue) > tolerance)
  {
    std::cerr << "Distance computed not correct: "
              << "truevalue= " << trueValue << "ComputedValue=" << distanceComputed << std::endl;
    return EXIT_FAILURE;
  }

  // Compute distance between two measurement vectors
  MeasurementVectorType measurementSingleComponent2;
  itk::NumericTraits<MeasurementVectorType>::SetLength(measurementSingleComponent2, 1);
  measurementSingleComponent2[0] = 1.5;

  trueValue = 1.0;
  distanceComputed = distance->Evaluate(measurementSingleComponent, measurementSingleComponent2);

  if (itk::Math::abs(distanceComputed - trueValue) > tolerance)
  {
    std::cerr << "Distance computed not correct: "
              << "truevalue= " << trueValue << "ComputedValue=" << distanceComputed << std::endl;
    return EXIT_FAILURE;
  }

  // Attempt to compute distance between two unequal size measurement vectors
  MeasurementVectorType measurementSingleComponent3;
  itk::NumericTraits<MeasurementVectorType>::SetLength(measurementSingleComponent3, 2);
  measurementSingleComponent3[0] = 1.5;
  measurementSingleComponent3[1] = 2.5;

  distance->Evaluate(measurementSingleComponent, measurementSingleComponent2);

  try
  {
    distance->Evaluate(measurementSingleComponent, measurementSingleComponent3);
    std::cerr
      << "Attempting to compute distance between unequal size measurement vectors. Exception should have been thrown: "
      << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & excpt)
  {
    std::cerr << "Exception caught: " << excpt << std::endl;
  }


  return EXIT_SUCCESS;
}
