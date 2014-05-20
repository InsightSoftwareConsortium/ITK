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

#include "itkFEMElement2DC0LinearQuadrilateralStrain.h"

#include <iostream>

//
int itkFEMElement2DStrainTest(int, char *[])
{

  typedef itk::fem::Element ElementType;
  typedef ElementType::Node NodeType;

  NodeType::Pointer       n0, n1, n2, n3;
  ElementType::VectorType pt(2);

  n0 = NodeType::New();
  pt[0] = 0.;
  pt[1] = 0.;
  n0->SetCoordinates(pt);

  n1 = NodeType::New();
  pt[0] = 1.;
  pt[1] = 1.;
  n1->SetCoordinates(pt);

  n2 = NodeType::New();
  pt[0] = 3.;
  pt[1] = 2.;
  n2->SetCoordinates(pt);

  n3 = NodeType::New();
  pt[0] = 0.;
  pt[1] = 3.;
  n3->SetCoordinates(pt);

  typedef itk::fem::MaterialLinearElasticity ElasticityType;

  ElasticityType::Pointer m = ElasticityType::New();

  m->SetGlobalNumber(0);
  m->SetYoungsModulus(30000.0);
  m->SetCrossSectionalArea(0.02);
  m->SetMomentOfInertia(0.004);

  typedef itk::fem::Element2DC0LinearQuadrilateralStrain StrainType;

  StrainType::Pointer e0 = StrainType::New();

  e0->SetGlobalNumber(0);
  e0->SetNode(0, n0);
  e0->SetNode(1, n1);
  e0->SetNode(2, n2);
  e0->SetNode(3, n3);
  if (dynamic_cast<ElasticityType *>( m.GetPointer() ))
    {
    e0->SetMaterial( dynamic_cast<ElasticityType *>( m.GetPointer() ) );
    }

  ElementType::MatrixType D, Me;

  e0->GetMassMatrix(Me);
  e0->GetMaterialMatrix(D);
  std::cout << "Mass matrix: " << std::endl << Me << std::endl;
  std::cout << "Material matrix: " << std::endl << D << std::endl;
  std::cout << "#dof per node = " << e0->GetNumberOfDegreesOfFreedomPerNode() << std::endl;

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
