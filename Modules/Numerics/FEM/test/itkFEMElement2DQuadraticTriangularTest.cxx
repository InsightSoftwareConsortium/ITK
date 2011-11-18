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

#include "itkFEMElement2DC0QuadraticTriangularStrain.h"

#include <iostream>

//
int itkFEMElement2DQuadraticTriangularTest(int , char *[])
{

  typedef itk::fem::Element ElementType;
  typedef ElementType::Node NodeType;

  NodeType::Pointer       n0, n1, n2;
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
  pt[0] = 0.;
  pt[1] = 2.;
  n2->SetCoordinates(pt);

  typedef itk::fem::MaterialLinearElasticity ElasticityType;

  ElasticityType::Pointer m = ElasticityType::New();

  m->SetGlobalNumber(0);
  m->SetYoungsModulus(300.0);
  m->SetCrossSectionalArea(0.02);
  m->SetMomentOfInertia(0.004);

  typedef itk::fem::Element2DC0QuadraticTriangularStrain StrainType;
  StrainType::Pointer e0 = StrainType::New();

  e0->SetGlobalNumber(0);
  e0->SetNode(0, n0);
  e0->SetNode(1, n1);
  e0->SetNode(2, n2);
  e0->SetMaterial(m.GetPointer());

  pt[0] = 0.5;
  pt[1] = 0.5;

  std::cout << "#integration points = " << e0->GetNumberOfIntegrationPoints(2) << std::endl;
  std::cout << "shape fxns at " << pt << ":\n" << e0->ShapeFunctions(pt) << std::endl;

  ElementType::MatrixType shapeD;
  e0->ShapeFunctionDerivatives(pt, shapeD);
  std::cout << "shape fxn derivatives:" << std::endl << shapeD << std::endl;

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
