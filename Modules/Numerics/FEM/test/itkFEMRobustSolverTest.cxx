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

#include "itkFEMObject.h"
#include "itkFEMRobustSolver.h"
#include "itkFEMLoadNoisyLandmark.h"
#include "itkFEMElement2DC0LinearQuadrilateralStrain.h"
#include "itkImageToRectilinearFEMObjectFilter.h"


/**
 * RobustSolver requires a FEMObject as input.
 * In this test, we create a FEMObject manually based on a simple 2D mesh and feature points.
 *
 * In most cases, users have a mesh and feature points rather than the FEMObject as inputs.
 * Users do not need to manually convert the mesh and the feature points into a FEMOjbect.
 * We develop a FEMScatteredDataPointSetToImageFilter to facilitate this conversion.
 *
 *Example:
 * The image is 5x5, the mesh is 2x2, and four feature points represent four
 * cases: two points are on the boundary, one point is inside the element, and
 * one point is the node.
 *
 * 4   ----------------
 *   |    |   |   |   |
 * 3 |----|---|-------*
 *   |    |   |   |   |
 * 2 |----|---*---|---|
 *   |    |   |   |   |
 * 1 |----|---|---*---|
 *   |    |   |   |   |
 *    ----*-----------
 *  0     1   2   3   4
 *
 */
int itkFEMRobustSolverTest(int, char *[])
{
  const unsigned int DataDimension = 2;
  const unsigned int ParameterDimension = 2;

  /** Solver typedef suppot */
  typedef itk::fem::RobustSolver<DataDimension>    SolverType;

  /** FEMObject typedef suppport */
  typedef itk::fem::FEMObject<DataDimension>       FEMObjectType;

  /** FEM element typedef support */
  typedef itk::fem::Element2DC0LinearQuadrilateralStrain     ElementType;

  /** FEM node typedef support */
  typedef itk::fem::Element::Node            NodeType;

  /** FEM Load typedef support */
  typedef itk::fem::LoadNoisyLandmark          LoadType;

  /** FEM material typedef support */
  typedef itk::fem::MaterialLinearElasticity           MaterialType;

  /** FEM element typedef support */
  typedef itk::fem::Element::VectorType                  FEMVectorType;

  /** FEM container typedef support */
  typedef FEMObjectType::LoadContainerType        LoadContainerType;
  typedef FEMObjectType::NodeContainerType        NodeContainerType;
  typedef FEMObjectType::ElementContainerType     ElementContainerType;
  typedef FEMObjectType::MaterialContainerType    MaterialContainerType;

  /** intepolation grid typedef support */
  typedef itk::Image<itk::fem::Element::ConstPointer, ParameterDimension> InterpolationGridType;

  SolverType::Pointer solver = SolverType::New();

  FEMObjectType::Pointer femObject = FEMObjectType::New();

  /** initialize material */
  MaterialContainerType *materialContainer = femObject->GetModifiableMaterialContainer();

  if(!materialContainer)
    {
    std::cerr << "Missing material container!" << std::endl;
    return EXIT_FAILURE;
    }

  materialContainer->Initialize();

  MaterialType::Pointer material = MaterialType::New();
  material->SetYoungsModulus(3000.0);
  material->SetPoissonsRatio(0.45);

  /** fix material to linear elasticity */
  femObject->AddNextMaterial(material);

  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  /** initialize nodes */
  NodeContainerType *nodeContainer = femObject->GetModifiableNodeContainer();

  if(!nodeContainer)
    {
    std::cerr << "Missing node container!" << std::endl;
    return EXIT_FAILURE;
    }

  nodeContainer->Initialize();

  unsigned int elementDimensionX = 2;
  unsigned int elementDimensionY = 2;

  FEMVectorType point(ParameterDimension);
  unsigned int globalNumbering = 0;

  for(unsigned int j = 0; j <= elementDimensionY; j++)
    {
    for(unsigned int i = 0; i <= elementDimensionX; i++)
      {
      point[0] = i;
      point[1] = j;

      NodeType::Pointer n =  NodeType::New();
      n->SetCoordinates(point);
      n->SetGlobalNumber(globalNumbering);

      femObject->AddNextNode(n);

      globalNumbering++;
      }
    }

  /** initialize elements */
  ElementContainerType *elementContainer = femObject->GetModifiableElementContainer();

  if(!elementContainer)
    {
    std::cerr << "Missing element container!" << std::endl;
    return EXIT_FAILURE;
    }

  elementContainer->Initialize();

  globalNumbering = 0;

  for( unsigned int j = 0; j < elementDimensionY; j++ )
    {
    for( unsigned int i = 0; i < elementDimensionX; i++ )
      {
      unsigned int leftBottomNodeIndex = i + ( elementDimensionX + 1 ) * j;
      unsigned int rightBottomNodeIndex = i + 1 + ( elementDimensionX + 1 ) * j;
      unsigned int rigthUpperNodeIndex = i + 1 + ( elementDimensionX + 1 ) * ( j + 1 );
      unsigned int leftUpperNodeIndex = i + ( elementDimensionX + 1 ) * ( j + 1 );

      ElementType::Pointer quadrilateral = ElementType::New();

      quadrilateral->SetNode(0, femObject->GetNode(leftBottomNodeIndex));
      quadrilateral->SetNode(1, femObject->GetNode(rightBottomNodeIndex));
      quadrilateral->SetNode(2, femObject->GetNode(rigthUpperNodeIndex));
      quadrilateral->SetNode(3, femObject->GetNode(leftUpperNodeIndex));

      quadrilateral->SetGlobalNumber(globalNumbering);
      quadrilateral->SetMaterial( static_cast<MaterialType *>( femObject->GetMaterial(0).GetPointer() ) );

      femObject->AddNextElement(quadrilateral.GetPointer());

      globalNumbering++;
      }
    }

  /** initialize loads */
  LoadContainerType *loadContainer = femObject->GetModifiableLoadContainer();

  if(!loadContainer)
    {
    std::cerr << "Missing load container!" << std::endl;
    return EXIT_FAILURE;
    }

  loadContainer->Initialize();

  FEMVectorType featurePoint0(ParameterDimension);
  FEMVectorType featurePoint1(ParameterDimension);
  FEMVectorType featurePoint2(ParameterDimension);
  FEMVectorType featurePoint3(ParameterDimension);

  FEMVectorType displacement0(DataDimension);
  FEMVectorType displacement1(DataDimension);
  FEMVectorType displacement2(DataDimension);
  FEMVectorType displacement3(DataDimension);

  // feature point is on the bottom boundary
  featurePoint0[0] = 1.0;
  featurePoint0[1] = 0.0;
  // feature point is inside an element
  featurePoint1[0] = 3.0;
  featurePoint1[1] = 1.0;
  // feature point is the node
  featurePoint2[0] = 2.0;
  featurePoint2[1] = 2.0;
  // feature point is on the right boundary
  featurePoint3[0] = 4.0;
  featurePoint3[1] = 3.0;

  // displacement associated with the feature point on the bottorm boundary
  displacement0[0] = 1.0;
  displacement0[1] = 1.0;
  // displacement assoaiate with the feature point inside the element
  displacement1[0] = 1.0;
  displacement1[1] = 1.0;
  // displacement associated with the fature point, coincidentally being the node of the mesh
  displacement2[0] = 1.0;
  displacement2[1] = 1.0;
  // displacement associated with the feature point on the right boundary
  displacement3[0] = 1.0;
  displacement3[1] = 1.0;

  LoadType::Pointer load0 =  LoadType::New();
  load0->SetSource(featurePoint0);
  load0->SetRealDisplacement(displacement0);

  LoadType::Pointer load1 =  LoadType::New();
  load1->SetSource(featurePoint1);
  load1->SetRealDisplacement(displacement1);

  LoadType::Pointer load2 =  LoadType::New();
  load2->SetSource(featurePoint2);
  load2->SetRealDisplacement(displacement2);

  LoadType::Pointer load3 =  LoadType::New();
  load3->SetSource(featurePoint3);
  load3->SetRealDisplacement(displacement3);

  femObject->AddNextLoad(itk::fem::Load::Pointer(load0));
  femObject->AddNextLoad(itk::fem::Load::Pointer(load1));
  femObject->AddNextLoad(itk::fem::Load::Pointer(load2));
  femObject->AddNextLoad(itk::fem::Load::Pointer(load3));

  /** finialize mesh to produce global DOF */
  femObject->FinalizeMesh();

  /** set interpolation grid */

  InterpolationGridType::PointType        origin;
  origin[0] = 0.0;
  origin[1] = 0.0;
  solver->SetOrigin(origin);

  InterpolationGridType::SpacingType      spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;
  solver->SetSpacing(spacing);

  InterpolationGridType::SizeType         size;
  size[0] = 5;
  size[1] = 5;
  InterpolationGridType::IndexType        start;
  start[0] = 0;
  start[1] = 0;
  InterpolationGridType::RegionType       region;
  region.SetSize(size);
  region.SetIndex(start);
  solver->SetRegion(region);

  InterpolationGridType::DirectionType    direction;
  direction[0][0] = 1.0;
  direction[0][1] = 0.0;
  direction[1][0] = 0.0;
  direction[1][1] = 1.0;
  solver->SetDirection(direction);

  // if the feature points are the grid point of the interpolation grid, set true.
  // note that since feature points come from the image, this setting is always true.
  solver->SetUseInterpolationGrid(true);

  solver->SetInput( femObject );
  solver->Update();

  int numOfDOF = femObject->GetNumberOfDegreesOfFreedom();
  FEMVectorType solution(numOfDOF);

  bool  hasError = false;
  float groundTruthSolution[18] = {1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                                   1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                                   1.0, 1.0, 1.0, 1.0, 1.0, 1.0};

  for( int i = 0; i < numOfDOF; i++ )
    {
    solution[i] = solver->GetSolution(i);

    std::cout << "Solution[" << i << "]:" << solution[i] << std::endl;

    if( std::fabs(groundTruthSolution[i] - solution[i]) > 0.0001 )
      {
      std::cerr << "ERROR: Index " << i << ". Groundtruth " << groundTruthSolution[i] << " Solution " << solution[i] << std::endl;
      hasError = true;
      }
    }

  if( hasError )
    {
    std::cerr << "Test FAILED!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
