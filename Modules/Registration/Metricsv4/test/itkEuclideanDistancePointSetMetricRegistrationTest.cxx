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
#include "itkPointToPlanePointSetToPointSetMetricv4.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkAffineTransform.h"
#include "itkRigid2DTransform.h"
#include "itkCommand.h"
#include "itkMath.h"

template <typename TFilter>
class itkEuclideanDistancePointSetMetricRegistrationTestCommandIterationUpdate : public itk::Command
{
public:
  using Self = itkEuclideanDistancePointSetMetricRegistrationTestCommandIterationUpdate;

  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  itkEuclideanDistancePointSetMetricRegistrationTestCommandIterationUpdate() = default;

public:
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    if (typeid(event) != typeid(itk::IterationEvent))
    {
      return;
    }
    const auto * optimizer = dynamic_cast<const TFilter *>(object);

    if (!optimizer)
    {
      itkGenericExceptionMacro("Error dynamic_cast failed");
    }
    std::cout << "It: " << optimizer->GetCurrentIteration() << " metric value: " << optimizer->GetCurrentMetricValue();
    std::cout << std::endl;
  }
};

// Transform type
using itkEuclideanDistancePointSetMetricRegistrationTestTransformType = itk::Transform<double, 2, 2>;

/////////////////////////////////////////////////////////
template <typename TTransform, typename TMetric, typename TPointSet>
int
itkEuclideanDistancePointSetMetricRegistrationTestRun(unsigned int                   numberOfIterations,
                                                      double                         maximumPhysicalStepSize,
                                                      double                         pointMax,
                                                      typename TTransform::Pointer & transform,
                                                      typename TMetric::Pointer &    metric)
{
  using PointSetType = TPointSet;
  using PointType = typename PointSetType::PointType;
  using CoordRepType = typename PointType::CoordRepType;

  auto fixedPoints = PointSetType::New();
  fixedPoints->Initialize();

  auto movingPoints = PointSetType::New();
  movingPoints->Initialize();

  // Create a few points and apply a small rotation to make the moving point set

  int num_of_points = 30;

  float x[num_of_points] = { 0.0,  1.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0,  10.0, 11.0, 12.0, 13.0, 14.0,
                             15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0 };

  float y[num_of_points] = { 0.0,    0.096,  0.337,  0.598,  0.727,  0.598,  0.169, -0.491, -1.211, -1.76,
                             -1.918, -1.552, -0.671, 0.559,  1.84,   2.814,  3.166, 2.715,  1.484,  -0.286,
                             -2.176, -3.695, -4.4,   -4.027, -2.576, -0.332, 2.185, 4.34,   5.547,  5.422 };

  float nx[num_of_points] = { 0.0,     -0.166,  -0.2437, -0.1918, 0.0,     0.2688,  0.4784,  0.568,   0.5356,  0.3333,
                              -0.1031, -0.5292, -0.726,  -0.7821, -0.7481, -0.5527, 0.0495,  0.6437,  0.8321,  0.8775,
                              0.8625,  0.7435,  0.1639,  -0.6739, -0.8795, -0.9219, -0.9193, -0.8595, -0.4758, 0.0 };

  float ny[num_of_points] = { 0.0,    0.9861, 0.9698, 0.9814, 1.0,    0.9632, 0.8781, 0.823,  0.8445, 0.9428,
                              0.9947, 0.8485, 0.6877, 0.6231, 0.6636, 0.8334, 0.9988, 0.7653, 0.5546, 0.4796,
                              0.506,  0.6687, 0.9865, 0.7388, 0.476,  0.3873, 0.3935, 0.5112, 0.8795, 0.0 };

  float     theta = itk::Math::pi / static_cast<float>(180.0) * static_cast<float>(25.0);
  PointType fixedPoint;

  using VectorType = itk::Vector<float, 2>;
  VectorType v;

  for (int i = 0; i < num_of_points; ++i)
  {
    fixedPoint[0] = static_cast<CoordRepType>(x[i]);
    fixedPoint[1] = static_cast<CoordRepType>(y[i]);
    fixedPoints->SetPoint(i, fixedPoint);
    v[0] = nx[i];
    v[1] = ny[i];
    fixedPoints->SetPointData(i, v);
  }

  unsigned int numberOfPoints = fixedPoints->GetNumberOfPoints();

  PointType movingPoint;
  for (unsigned int n = 0; n < numberOfPoints; ++n)
  {
    fixedPoint = fixedPoints->GetPoint(n);
    movingPoint[0] = fixedPoint[0] * std::cos(theta) - fixedPoint[1] * std::sin(theta);
    movingPoint[1] = fixedPoint[0] * std::sin(theta) + fixedPoint[1] * std::cos(theta);
    movingPoints->SetPoint(n, movingPoint);
    std::cout << fixedPoint << " -> " << movingPoint << std::endl;
  }

  // Finish setting up the metric
  metric->SetFixedPointSet(fixedPoints);
  metric->SetMovingPointSet(movingPoints);
  metric->SetMovingTransform(transform);
  metric->Initialize();

  // scales estimator
  using RegistrationParameterScalesFromShiftType = itk::RegistrationParameterScalesFromPhysicalShift<TMetric>;
  typename RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator =
    RegistrationParameterScalesFromShiftType::New();
  shiftScaleEstimator->SetMetric(metric);
  // needed with pointset metrics
  shiftScaleEstimator->SetVirtualDomainPointSet(metric->GetVirtualTransformedPointSet());

  // optimizer
  using OptimizerType = itk::GradientDescentOptimizerv4;
  auto optimizer = OptimizerType::New();
  optimizer->SetMetric(metric);
  optimizer->SetLearningRate(0.0001);
  optimizer->SetNumberOfIterations(numberOfIterations);
  optimizer->SetScalesEstimator(shiftScaleEstimator);
  optimizer->SetMaximumStepSizeInPhysicalUnits(maximumPhysicalStepSize);

  using CommandType = itkEuclideanDistancePointSetMetricRegistrationTestCommandIterationUpdate<OptimizerType>;
  auto observer = CommandType::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);

  // start
  optimizer->StartOptimization();

  std::cout << "numberOfIterations: " << numberOfIterations << std::endl;
  std::cout << "maximumPhysicalStepSize: " << maximumPhysicalStepSize << std::endl;
  std::cout << "Optimizer scales: " << optimizer->GetScales() << std::endl;
  std::cout << "Optimizer learning rate: " << optimizer->GetLearningRate() << std::endl;
  std::cout << "Moving-source final value: " << optimizer->GetCurrentMetricValue() << std::endl;
  if (transform->GetTransformCategory() == TTransform::TransformCategoryEnum::DisplacementField)
  {
    std::cout << "local-support transform non-zero parameters: " << std::endl;
    typename TTransform::ParametersType params = transform->GetParameters();
    for (itk::SizeValueType n = 0; n < transform->GetNumberOfParameters(); n += transform->GetNumberOfLocalParameters())
    {
      typename TTransform::ParametersValueType zero =
        itk::NumericTraits<typename TTransform::ParametersValueType>::ZeroValue();
      if (itk::Math::NotExactlyEquals(params[n], zero) && itk::Math::NotExactlyEquals(params[n + 1], zero))
      {
        std::cout << n << ", " << n + 1 << " : " << params[n] << ", " << params[n + 1] << std::endl;
      }
    }
  }
  else
  {
    std::cout << "Moving-source final position: " << optimizer->GetCurrentPosition() << std::endl;
  }

  // applying the resultant transform and verify result
  std::cout << "Fixed\tMoving\tMovingTransformed\tFixedTransformed\tDiff" << std::endl;
  bool                                             passed = true;
  auto                                             tolerance = static_cast<typename PointType::ValueType>(1e-4);
  typename TTransform::InverseTransformBasePointer fixedInverse = metric->GetFixedTransform()->GetInverseTransform();
  for (unsigned int n = 0; n < numberOfPoints; ++n)
  {
    // compare the points in moving domain so we don't have to worry about an inverse
    // of the displacement field transform
    PointType transformedFixedPoint = fixedInverse->TransformPoint(fixedPoints->GetPoint(n));
    transformedFixedPoint = metric->GetMovingTransform()->TransformPoint(transformedFixedPoint);
    PointType difference;
    movingPoint = movingPoints->GetPoint(n);
    difference[0] = movingPoint[0] - transformedFixedPoint[0];
    difference[1] = movingPoint[1] - transformedFixedPoint[1];
    std::cout << fixedPoints->GetPoint(n) << "->" << movingPoint << "->" << transformedFixedPoint << "->" << difference
              << std::endl;
    if (itk::Math::abs(difference[0]) > tolerance || itk::Math::abs(difference[1]) > tolerance)
    {
      passed = false;
    }
  }
  if (!passed)
  {
    std::cerr << "Results do not match truth within tolerance." << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

////////////////////////////////////////////////////////////////////////////////
int
itkEuclideanDistancePointSetMetricRegistrationTest(int argc, char * argv[])
{
  constexpr unsigned int Dimension = 2;

  int finalResult = EXIT_SUCCESS;

  unsigned int numberOfIterations = 500;
  auto         maximumPhysicalStepSize = static_cast<double>(0.01);
  if (argc > 1)
  {
    numberOfIterations = std::stoi(argv[1]);
  }
  if (argc > 2)
  {
    maximumPhysicalStepSize = std::stod(argv[2]);
  }

  auto pointMax = static_cast<double>(100.0);

  //
  // Test with affine transform
  //

  // metric
  using PointSetType = itk::PointSet<itk::Vector<float, 2>, Dimension>;
  using PointSetMetricType = itk::PointToPlanePointSetToPointSetMetricv4<PointSetType>;
  // using PointSetMetricType = itk::EuclideanDistancePointSetToPointSetMetricv4<PointSetType>;
  auto metric = PointSetMetricType::New();

  // transform
  // using AffineTransformType = itk::AffineTransform<double, Dimension>;
  using AffineTransformType = itk::Rigid2DTransform<double>;
  auto affineTransform = AffineTransformType::New();
  affineTransform->SetIdentity();
  std::cout << "XX Test with affine transform: " << std::endl;
  int oneResult =
    itkEuclideanDistancePointSetMetricRegistrationTestRun<AffineTransformType, PointSetMetricType, PointSetType>(
      numberOfIterations, maximumPhysicalStepSize, pointMax, affineTransform, metric);
  if (oneResult == EXIT_FAILURE)
  {
    finalResult = EXIT_FAILURE;
    std::cerr << "Failed for affine transform." << std::endl;
  }

  // //
  // // Displacement field transform
  // //

  // using DisplacementFieldTransformType = itk::DisplacementFieldTransform<double, Dimension>;
  // auto displacementTransform = DisplacementFieldTransformType::New();

  // // Setup the physical space to match the point set virtual domain,
  // // which is defined by the fixed point set since the fixed transform
  // // is identity.
  // using FieldType = DisplacementFieldTransformType::DisplacementFieldType;
  // using RegionType = FieldType::RegionType;
  // using RealType = DisplacementFieldTransformType::ScalarType;

  // FieldType::SpacingType spacing;
  // spacing.Fill(static_cast<RealType>(1.0));

  // FieldType::DirectionType direction;
  // direction.Fill(static_cast<RealType>(0.0));
  // for (unsigned int d = 0; d < Dimension; ++d)
  // {
  //   direction[d][d] = static_cast<RealType>(1.0);
  // }

  // FieldType::PointType origin;
  // origin.Fill(static_cast<RealType>(0.0));

  // RegionType::SizeType regionSize;
  // regionSize.Fill(static_cast<itk::SizeValueType>(pointMax) + 1);

  // RegionType::IndexType regionIndex;
  // regionIndex.Fill(0);

  // RegionType region;
  // region.SetSize(regionSize);
  // region.SetIndex(regionIndex);

  // auto displacementField = FieldType::New();
  // displacementField->SetOrigin(origin);
  // displacementField->SetDirection(direction);
  // displacementField->SetSpacing(spacing);
  // displacementField->SetRegions(region);
  // displacementField->Allocate();
  // DisplacementFieldTransformType::OutputVectorType zeroVector;
  // zeroVector.Fill(static_cast<RealType>(0.0));
  // displacementField->FillBuffer(zeroVector);
  // displacementTransform->SetDisplacementField(displacementField);

  // // metric
  // using PointSetMetricType = itk::PointToPlanePointSetToPointSetMetricv4<PointSetType>;
  // auto metric2 = PointSetMetricType::New();
  // // If we don't set the virtual domain when using a displacement field transform, the
  // // metric takes it from the transform during initialization.
  // // metric2->SetVirtualDomain( spacing, origin, direction, region );

  // std::cout << "XX Testing with displacement field transform." << std::endl;
  // oneResult = itkEuclideanDistancePointSetMetricRegistrationTestRun<DisplacementFieldTransformType,
  //                                                                   PointSetMetricType,
  //                                                                   PointSetType>(
  //   numberOfIterations, maximumPhysicalStepSize, pointMax, displacementTransform, metric2);
  // if (oneResult == EXIT_FAILURE)
  // {
  //   finalResult = EXIT_FAILURE;
  //   std::cerr << "Failed for displacement transform." << std::endl;
  // }

  return finalResult;
}
