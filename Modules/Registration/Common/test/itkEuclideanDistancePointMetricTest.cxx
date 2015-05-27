/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkIdentityTransform.h"
#include "itkQuadEdgeMesh.h"
#include "itkRegularSphereMeshSource.h"
#include "itkEuclideanDistancePointMetric.h"

/**
 *  This test uses EuclideanDistancePointMetric to compare a Mesh and a
 *  QuadEdgeMesh.  The purpose of the test is to expose a bug caused by using
 *  the same iterator for both fixed and moving point sets in the parent class.
 *
 */

template< class TFixedMesh, class TMovingMesh >
double
CompareMeshSources()
{

  typedef itk::RegularSphereMeshSource< TFixedMesh >  FixedSourceType;
  typedef itk::RegularSphereMeshSource< TMovingMesh > MovingSourceType;

  typedef itk::EuclideanDistancePointMetric< TFixedMesh,
                                             TMovingMesh > MetricType;

  typedef itk::IdentityTransform< double, 3 > IdentityType;

  typename FixedSourceType::Pointer fixed = FixedSourceType::New();
  fixed->Update();

  typename MovingSourceType::Pointer moving = MovingSourceType::New();
  moving->Update();

  typename IdentityType::Pointer identity = IdentityType::New();

  typename MetricType::Pointer metric = MetricType::New();
  metric->SetFixedPointSet( fixed->GetOutput() );
  metric->SetMovingPointSet( moving->GetOutput() );
  metric->SetTransform( identity );

  typename MetricType::MeasureType measure;
  measure = metric->GetValue(identity->GetParameters());

  double sum = 0;
  for( unsigned int i = 0; i < measure.Size(); ++i )
    {
    sum += measure[i];
    }

  return sum;

}

int itkEuclideanDistancePointMetricTest(int, char* [] )
{

  const unsigned int Dimension = 3;
  typedef double ScalarType;
  const double Epsilon = 10e-6;

  typedef itk::Mesh< ScalarType, Dimension >         MeshType;
  typedef itk::QuadEdgeMesh< ScalarType, Dimension > QuadEdgeMeshType;

  if ( CompareMeshSources< MeshType, MeshType >() > Epsilon )
    {
    std::cerr << "Comparison between the following was nonzero:" << std::endl;
    std::cerr << "Fixed: itk::Mesh" << std::endl;
    std::cerr << "Moving: itk::Mesh" << std::endl;
    return EXIT_FAILURE;
    }

  if ( CompareMeshSources< MeshType, QuadEdgeMeshType >() > Epsilon )
    {
    std::cerr << "Comparison between the following was nonzero:" << std::endl;
    std::cerr << "Fixed: itk::Mesh" << std::endl;
    std::cerr << "Moving: itk::QuadEdgeMesh" << std::endl;
    return EXIT_FAILURE;
    }

  if ( CompareMeshSources< QuadEdgeMeshType, MeshType >() > Epsilon )
    {
    std::cerr << "Comparison between the following was nonzero:" << std::endl;
    std::cerr << "Fixed: itk::QuadEdgeMesh" << std::endl;
    std::cerr << "Moving: itk::Mesh" << std::endl;
    return EXIT_FAILURE;
    }

  if ( CompareMeshSources< QuadEdgeMeshType, QuadEdgeMeshType >() > Epsilon )
    {
    std::cerr << "Comparison between the following was nonzero:" << std::endl;
    std::cerr << "Fixed: itk::QuadEdgeMesh" << std::endl;
    std::cerr << "Moving: itk::QuadEdgeMesh" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
