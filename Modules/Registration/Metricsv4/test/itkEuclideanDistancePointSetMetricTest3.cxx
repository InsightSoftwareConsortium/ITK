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

#include "itkEuclideanDistancePointSetToPointSetMetricv4.h"
#include "itkTranslationTransform.h"
#include "itkTestingMacros.h"

#include <fstream>
#include "itkMath.h"

/*
 * Test with a translation transform
 */

template <unsigned int Dimension>
int
itkEuclideanDistancePointSetMetricTest3Run(double distanceThreshold)
{
  using PointSetType = itk::PointSet<float, Dimension>;
  using PointType = typename PointSetType::PointType;
  using IdentifierType = itk::IdentifierType;
  using PointsContainerType = itk::VectorContainer<IdentifierType, PointType>;
  using PointsLocatorType = itk::PointsLocator<PointsContainerType>;
  auto pointsLocator = PointsLocatorType::New();

  auto fixedPoints = PointSetType::New();
  fixedPoints->Initialize();

  auto movingPoints = PointSetType::New();
  movingPoints->Initialize();

  // Create a few points and apply a small offset to make the moving points
  auto      pointMax = static_cast<float>(1.0);
  PointType fixedPoint;
  fixedPoint.Fill(0.0);
  fixedPoint[0] = 0.0;
  fixedPoint[1] = 0.0;
  fixedPoints->SetPoint(0, fixedPoint);
  fixedPoint[0] = pointMax;
  fixedPoint[1] = 0.0;
  fixedPoints->SetPoint(1, fixedPoint);
  fixedPoint[0] = 0.0;
  fixedPoint[1] = pointMax;
  fixedPoints->SetPoint(2, fixedPoint);
  if (Dimension == 3)
  {
    fixedPoint[0] = 0.0;
    fixedPoint[1] = 0.0;
    fixedPoint[2] = pointMax;
    fixedPoints->SetPoint(3, fixedPoint);
  }
  unsigned int numberOfPoints = fixedPoints->GetNumberOfPoints();

  PointType movingPoint;
  for (unsigned int n = 0; n < numberOfPoints; ++n)
  {
    movingPoint.Fill(0);
    fixedPoint = fixedPoints->GetPoint(n);
    if (n == 0)
    {
      movingPoint[0] = fixedPoint[0] + 0.5;
      movingPoint[1] = fixedPoint[1] + 0.75;
    }
    else if (n == 1)
    {
      movingPoint[0] = fixedPoint[0];
      movingPoint[1] = fixedPoint[1] + 0.25;
    }
    else if (n == 2)
    {
      movingPoint[0] = fixedPoint[0] + 0.25;
      movingPoint[1] = fixedPoint[1];
    }
    if (Dimension == 3)
    {
      movingPoint[2] = fixedPoint[2] + 0.75;
    }
    movingPoints->SetPoint(n, movingPoint);
  }

  pointsLocator->SetPoints(movingPoints->GetPoints());
  pointsLocator->Initialize();

  // Calculate distance between nearest points (correspondence points)
  std::vector<double> distanceArray;
  for (unsigned int n = 0; n < numberOfPoints; ++n)
  {
    auto tempFixedPoint = fixedPoints->GetPoint(n);
    auto pointId = pointsLocator->FindClosestPoint(tempFixedPoint);
    auto closestPoint = movingPoints->GetPoint(pointId);
    distanceArray.push_back(closestPoint.EuclideanDistanceTo(tempFixedPoint));
    std::cout << n << " " << tempFixedPoint << " , " << closestPoint << " "
              << closestPoint.EuclideanDistanceTo(tempFixedPoint) << std::endl;
  }

  // Test with Translation transform
  std::cout << "Testing with Translation Transform." << std::endl;
  using TranslationTransformType = itk::TranslationTransform<double, Dimension>;
  auto translationTransform = TranslationTransformType::New();

  // Instantiate the metric
  using PointSetMetricType = itk::EuclideanDistancePointSetToPointSetMetricv4<PointSetType>;
  auto metric = PointSetMetricType::New();
  metric->SetFixedPointSet(fixedPoints);
  metric->SetMovingPointSet(movingPoints);
  metric->SetMovingTransform(translationTransform);
  metric->SetDistanceThreshold(distanceThreshold);

  ITK_TEST_SET_GET_VALUE(distanceThreshold, metric->GetDistanceThreshold());

  metric->Initialize();

  // test
  typename PointSetMetricType::MeasureType    value = metric->GetValue(), value2;
  typename PointSetMetricType::DerivativeType derivative, derivative2;
  metric->GetDerivative(derivative);
  metric->GetValueAndDerivative(value2, derivative2);

  std::cout << "value: " << value << std::endl;

  // Check for the same results from different methods
  if (itk::Math::NotExactlyEquals(value, value2))
  {
    std::cerr << "value does not match between calls to different methods: "
              << "value: " << value << " value2: " << value2 << std::endl;
  }
  if (derivative != derivative2)
  {
    std::cerr << "derivative does not match between calls to different methods: "
              << "derivative: " << derivative << " derivative2: " << derivative2 << std::endl;
  }

  // Check if the points outside threshold are skipped in metric calculation
  double distanceSum = 0.0;
  for (unsigned int n = 0; n < numberOfPoints; ++n)
  {
    if (distanceThreshold <= 0 || distanceArray[n] < distanceThreshold)
    {
      distanceSum = distanceSum + distanceArray[n];
    }
  }

  double valueTest = distanceSum / numberOfPoints;
  if (itk::Math::NotExactlyEquals(valueTest, value2))
  {
    std::cerr << "Value calculation is wrong when used threshold : " << distanceThreshold << "valueTest: " << valueTest
              << " value2: " << value2 << std::endl;
    return EXIT_FAILURE;
  }


  // Check if the point outside threshold is skipped in derivative calculation
  typename PointSetMetricType::DerivativeType derivativeTest;
  derivativeTest.SetSize(Dimension);
  derivativeTest.Fill(0);
  for (unsigned int n = 0; n < numberOfPoints; ++n)
  {
    auto tempFixedPoint = fixedPoints->GetPoint(n);
    auto tempMovingPoint = movingPoints->GetPoint(n);
    auto tempDerivative = tempMovingPoint - tempFixedPoint;

    if (distanceThreshold <= 0 || distanceArray[n] < distanceThreshold)
    {
      derivativeTest[0] = derivativeTest[0] + tempDerivative[0];
      derivativeTest[1] = derivativeTest[1] + tempDerivative[1];
      if (Dimension == 3)
      {
        derivativeTest[2] = derivativeTest[2] + tempDerivative[2];
      }
    }
  }

  auto derivativeTestMean = derivativeTest / numberOfPoints;
  std::cout << "Derivative is [ " << derivativeTestMean << " ]" << std::endl;

  for (unsigned int i = 0; i << Dimension; ++i)
  {
    if (itk::Math::NotExactlyEquals(derivativeTestMean[i], derivative2[i]))
    {
      std::cerr << "Derivative calculation is wrong when used threshold : " << distanceThreshold
                << "derivativeTestMean: " << derivativeTestMean << " derivative2: " << derivative2 << std::endl;
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}

int
itkEuclideanDistancePointSetMetricTest3(int, char *[])
{
  int result = EXIT_SUCCESS;

  double distanceThresholdPositive = 0.5;
  double distanceThresholdZero = 0.0;
  double distanceThresholdNegative = -8.0;

  const unsigned int dimension2D = 2;
  const unsigned int dimension3D = 3;

  // Test for positive distance threshold
  if (itkEuclideanDistancePointSetMetricTest3Run<dimension2D>(distanceThresholdPositive) == EXIT_FAILURE)
  {
    std::cerr << "Failed for Dimension " << dimension2D << " for distanceThrehold = " << distanceThresholdPositive
              << std::endl;
    result = EXIT_FAILURE;
  }

  // Test for zero distance threshold
  if (itkEuclideanDistancePointSetMetricTest3Run<dimension2D>(distanceThresholdZero) == EXIT_FAILURE)
  {
    std::cerr << "Failed for Dimension " << dimension2D << " for distanceThrehold = " << distanceThresholdZero
              << std::endl;
    result = EXIT_FAILURE;
  }

  // Test for negative distance threshold
  if (itkEuclideanDistancePointSetMetricTest3Run<dimension2D>(distanceThresholdNegative) == EXIT_FAILURE)
  {
    std::cerr << "Failed for Dimension " << dimension2D << " for distanceThrehold = " << distanceThresholdNegative
              << std::endl;
    result = EXIT_FAILURE;
  }

  // Test for positive distance threshold
  if (itkEuclideanDistancePointSetMetricTest3Run<dimension3D>(distanceThresholdPositive) == EXIT_FAILURE)
  {
    std::cerr << "Failed for Dimension " << dimension3D << " for distanceThrehold = " << distanceThresholdPositive
              << std::endl;
    result = EXIT_FAILURE;
  }

  // Test for zero distance threshold
  if (itkEuclideanDistancePointSetMetricTest3Run<dimension3D>(distanceThresholdZero) == EXIT_FAILURE)
  {
    std::cerr << "Failed for Dimension " << dimension3D << " for distanceThrehold = " << distanceThresholdZero
              << std::endl;
    result = EXIT_FAILURE;
  }

  // Test for negative distance threshold
  if (itkEuclideanDistancePointSetMetricTest3Run<dimension3D>(distanceThresholdNegative) == EXIT_FAILURE)
  {
    std::cerr << "Failed for Dimension " << dimension3D << " for distanceThrehold = " << distanceThresholdNegative
              << std::endl;
    result = EXIT_FAILURE;
  }

  return result;
}
