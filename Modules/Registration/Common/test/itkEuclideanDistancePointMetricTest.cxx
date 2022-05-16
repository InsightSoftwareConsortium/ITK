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

#include "itkIdentityTransform.h"
#include "itkQuadEdgeMesh.h"
#include "itkRegularSphereMeshSource.h"
#include "itkEuclideanDistancePointMetric.h"
#include "itkTestingMacros.h"

/**
 *  This test uses EuclideanDistancePointMetric to compare a Mesh and a
 *  QuadEdgeMesh. The purpose of the test is to expose a bug caused by using
 *  the same iterator for both fixed and moving point sets in the parent class.
 *
 */

template <typename TFixedMesh, typename TMovingMesh>
double
CompareMeshSources(bool computeSquaredDistance)
{

  using FixedSourceType = itk::RegularSphereMeshSource<TFixedMesh>;
  using MovingSourceType = itk::RegularSphereMeshSource<TMovingMesh>;

  using MetricType = itk::EuclideanDistancePointMetric<TFixedMesh, TMovingMesh>;

  using IdentityType = itk::IdentityTransform<double, 3>;

  auto fixed = FixedSourceType::New();
  fixed->Update();

  auto moving = MovingSourceType::New();
  moving->Update();

  auto identity = IdentityType::New();

  auto metric = MetricType::New();

  metric->SetComputeSquaredDistance(computeSquaredDistance);
  ITK_TEST_SET_GET_VALUE(computeSquaredDistance, metric->GetComputeSquaredDistance());

  ITK_TEST_SET_GET_BOOLEAN(metric, ComputeSquaredDistance, computeSquaredDistance);


  metric->SetFixedPointSet(fixed->GetOutput());
  metric->SetMovingPointSet(moving->GetOutput());
  metric->SetTransform(identity);

  typename MetricType::MeasureType measure;
  measure = metric->GetValue(identity->GetParameters());

  double sum = 0;
  for (unsigned int i = 0; i < measure.Size(); ++i)
  {
    sum += measure[i];
  }

  return sum;
}

int
itkEuclideanDistancePointMetricTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " computeSquaredDistance " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;
  using ScalarType = double;
  constexpr double Epsilon = 10e-6;

  using MeshType = itk::Mesh<ScalarType, Dimension>;
  using QuadEdgeMeshType = itk::QuadEdgeMesh<ScalarType, Dimension>;

  using MetricType = itk::EuclideanDistancePointMetric<MeshType, QuadEdgeMeshType>;

  auto metric = MetricType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(metric, EuclideanDistancePointMetric, PointSetToPointSetMetric);

  bool computeSquaredDistance = static_cast<bool>(std::stoi(argv[1]));

  if (CompareMeshSources<MeshType, MeshType>(computeSquaredDistance) > Epsilon)
  {
    std::cerr << "Comparison between the following was nonzero:" << std::endl;
    std::cerr << "Fixed: itk::Mesh" << std::endl;
    std::cerr << "Moving: itk::Mesh" << std::endl;
    return EXIT_FAILURE;
  }

  if (CompareMeshSources<MeshType, QuadEdgeMeshType>(computeSquaredDistance) > Epsilon)
  {
    std::cerr << "Comparison between the following was nonzero:" << std::endl;
    std::cerr << "Fixed: itk::Mesh" << std::endl;
    std::cerr << "Moving: itk::QuadEdgeMesh" << std::endl;
    return EXIT_FAILURE;
  }

  if (CompareMeshSources<QuadEdgeMeshType, MeshType>(computeSquaredDistance) > Epsilon)
  {
    std::cerr << "Comparison between the following was nonzero:" << std::endl;
    std::cerr << "Fixed: itk::QuadEdgeMesh" << std::endl;
    std::cerr << "Moving: itk::Mesh" << std::endl;
    return EXIT_FAILURE;
  }

  if (CompareMeshSources<QuadEdgeMeshType, QuadEdgeMeshType>(computeSquaredDistance) > Epsilon)
  {
    std::cerr << "Comparison between the following was nonzero:" << std::endl;
    std::cerr << "Fixed: itk::QuadEdgeMesh" << std::endl;
    std::cerr << "Moving: itk::QuadEdgeMesh" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
