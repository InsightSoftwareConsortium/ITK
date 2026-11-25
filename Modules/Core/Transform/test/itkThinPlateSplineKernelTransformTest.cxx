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

/**
 * This tests the itk::ThinPlateSplineKernelTransform class by warping a
 * unit square (2D) / unit cube (3D) into a square/cube with side length 3.
 * It performs the test for both 2D and 3D, using a single templated helper
 * function, to make sure the class works properly for different dimensions.
 */
#include "itkThinPlateSplineKernelTransform.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

namespace
{

// Exercises itk::ThinPlateSplineKernelTransform<double, VDimension> by
// warping the corners of a unit square/cube (side length 1) into a
// square/cube of side length 3, and checks that:
// - the number of parameters is as expected,
// - every landmark is mapped exactly onto its target,
// - the transform correctly reports as being non-linear,
// - the inverse transform correctly maps every target back to its source,
// - unsupported operations correctly throw exceptions.
template <unsigned int VDimension>
[[nodiscard]] int
TestThinPlateSplineKernelTransform()
{
  constexpr double epsilon{ 1e-12 };

  using ParametersValueType = double;
  using TPSTransformType = itk::ThinPlateSplineKernelTransform<ParametersValueType, VDimension>;

  using PointType = typename TPSTransformType::InputPointType;
  using PointsIteratorType = typename TPSTransformType::PointsIterator;
  using PointSetType = typename TPSTransformType::PointSetType;

  auto tps = TPSTransformType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(tps, ThinPlateSplineKernelTransform, KernelTransform);

  // Number of corners of a VDimension-dimensional unit hypercube.
  const unsigned int numberOfLandmarks = 1u << VDimension;

  auto sourceLandmarks = PointSetType::New();
  auto targetLandmarks = PointSetType::New();

  sourceLandmarks->GetPoints()->Reserve(numberOfLandmarks);
  targetLandmarks->GetPoints()->Reserve(numberOfLandmarks);

  // Create landmark sets: every corner of the unit hypercube maps to the
  // corresponding corner of the hypercube of side length 3.
  PointsIteratorType sourceIt = sourceLandmarks->GetPoints()->Begin();
  PointsIteratorType targetIt = targetLandmarks->GetPoints()->Begin();

  for (unsigned int landmark = 0; landmark < numberOfLandmarks; ++landmark)
  {
    PointType sourcePoint;
    PointType targetPoint;
    for (unsigned int dim = 0; dim < VDimension; ++dim)
    {
      const unsigned int coordinate = (landmark >> dim) & 1u;
      sourcePoint[dim] = coordinate;
      targetPoint[dim] = 3 * coordinate;
    }
    sourceIt.Value() = sourcePoint;
    targetIt.Value() = targetPoint;
    ++sourceIt;
    ++targetIt;
  }

  std::cout << "TPS " << VDimension << "D Test:" << std::endl;
  tps->SetSourceLandmarks(sourceLandmarks);
  tps->SetTargetLandmarks(targetLandmarks);

  tps->ComputeWMatrix();

  { // Testing the number of parameters
    const typename TPSTransformType::ParametersType parameters1 = tps->GetParameters();
    const unsigned int                              numberOfParameters = parameters1.Size();
    const unsigned int                              expectedNumberOfParameters = numberOfLandmarks * VDimension;
    if (numberOfParameters != expectedNumberOfParameters)
    {
      std::cerr << "Number of parameters was not updated after" << std::endl;
      std::cerr << "invoking SetSourceLandmarks and SetTargetLandmarks" << std::endl;
      std::cerr << "Number of parameters is = " << numberOfParameters << std::endl;
      std::cerr << "While we were expecting = " << expectedNumberOfParameters << std::endl;
      return EXIT_FAILURE;
    }
  }

  const PointsIteratorType sourceEnd = sourceLandmarks->GetPoints()->End();

  sourceIt = sourceLandmarks->GetPoints()->Begin();
  targetIt = targetLandmarks->GetPoints()->Begin();

  while (sourceIt != sourceEnd)
  {
    const PointType sourcePoint = sourceIt.Value();
    const PointType targetPoint = targetIt.Value();
    const PointType mappedPoint = tps->TransformPoint(sourcePoint);
    std::cout << sourcePoint << " : " << targetPoint;
    std::cout << " warps to: " << mappedPoint << std::endl;
    if (mappedPoint.EuclideanDistanceTo(targetPoint) > epsilon)
    {
      std::cerr << "Failed to warp point " << sourcePoint << " to " << targetPoint << std::endl;
      return EXIT_FAILURE;
    }
    ++sourceIt;
    ++targetIt;
  }
  std::cout << std::endl;

  if (tps->IsLinear()) // NOTE TPS is never linear!
  {
    std::cerr << "ERROR:  " << VDimension << "D TPS reports as being a linear transform." << std::endl;
    return EXIT_FAILURE;
  }

  // NOTE: The following should set the default values explicitly
  {
    constexpr double TestValue{ 0.012345 };
    tps->SetStiffness(TestValue); // This value should not change the result at all.

    if (itk::Math::NotExactlyEquals(tps->GetStiffness(), TestValue))
    {
      std::cerr << "ERROR:  Explicitly set stiffness value not retained." << std::endl;
      return EXIT_FAILURE;
    }
    tps->SetStiffness(0.0);
  }

  { // Just for code coverage
    const typename TPSTransformType::VectorSetType::ConstPointer tempDisplacements = tps->GetDisplacements();

    {
      typename TPSTransformType::InputVectorType testVector{};
      testVector[VDimension - 1] = 1.0;
      ITK_TRY_EXPECT_EXCEPTION(tps->TransformVector(testVector));
    }
    {
      typename TPSTransformType::InputVnlVectorType testVector{};
      testVector[VDimension - 1] = 1.0;
      ITK_TRY_EXPECT_EXCEPTION(tps->TransformVector(testVector));
    }
    {
      typename TPSTransformType::InputCovariantVectorType testVector{};
      testVector[VDimension - 1] = 1.0;
      ITK_TRY_EXPECT_EXCEPTION(tps->TransformCovariantVector(testVector));
    }
    {
      typename TPSTransformType::JacobianPositionType testJacobian;
      typename TPSTransformType::InputPointType       testPoint{};
      testPoint[VDimension - 1] = 1.0;
      ITK_TRY_EXPECT_EXCEPTION(tps->ComputeJacobianWithRespectToPosition(testPoint, testJacobian));
    }
  }

  // Exercise GetInverse()
  {
    auto tpsInverse = TPSTransformType::New();
    if (!tps->GetInverse(tpsInverse))
    {
      std::cerr << "ERROR: Could not compute inverse transform for " << VDimension << "D TPS." << std::endl;
      return EXIT_FAILURE;
    }

    sourceIt = sourceLandmarks->GetPoints()->Begin();
    targetIt = targetLandmarks->GetPoints()->Begin();
    while (sourceIt != sourceEnd)
    {
      const PointType sourcePoint = sourceIt.Value();
      const PointType targetPoint = targetIt.Value();
      const PointType backMappedPoint = tpsInverse->TransformPoint(targetPoint);
      if (backMappedPoint.EuclideanDistanceTo(sourcePoint) > epsilon)
      {
        std::cerr << "Failed to warp point " << targetPoint << " back to " << sourcePoint << std::endl;
        return EXIT_FAILURE;
      }
      ++sourceIt;
      ++targetIt;
    }
  }

  return EXIT_SUCCESS;
}

} // namespace


int
itkThinPlateSplineKernelTransformTest(int, char *[])
{
  if (TestThinPlateSplineKernelTransform<2>() == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  if (TestThinPlateSplineKernelTransform<3>() == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
