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
#include "itkFEMElementBase.h"
#include "itkFEMObject.h"

#include <iostream>
using std::ofstream;
using std::ifstream;

//
int itkFEMLandmarkLoadImplementationTest(int, char *[])
{
  //Need to register default FEM object types,
  //and setup SpatialReader to recognize FEM types
  //which is all currently done as a HACK in
  //the initializaiton of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  typedef itk::fem::FEMObject<2> FEMObjectType;
  FEMObjectType::Pointer femObject = FEMObjectType::New();

  itk::fem::Node::Pointer n1;

  n1 = itk::fem::Node::New();
  itk::fem::Element::VectorType pt(2);

  pt[0] = 0.;
  pt[1] = 0.;
  n1->SetGlobalNumber(0);
  n1->SetCoordinates(pt);

  femObject->AddNextNode(n1.GetPointer());

  n1 = itk::fem::Node::New();
  pt[0] = 1.;
  pt[1] = 1.;
  n1->SetGlobalNumber(1);
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1.GetPointer());

  n1 = itk::fem::Node::New();
  pt[0] = 3.;
  pt[1] = 2.;
  n1->SetGlobalNumber(2);
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1.GetPointer());

  n1 = itk::fem::Node::New();
  pt[0] = 0.;
  pt[1] = 3.;
  n1->SetGlobalNumber(3);
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1.GetPointer());

  femObject->RenumberNodeContainer();

  // std::cout << "Nodes\n";

  itk::fem::MaterialLinearElasticity::Pointer m;
  m = itk::fem::MaterialLinearElasticity::New();
  m->SetGlobalNumber(0);
  m->SetYoungsModulus(30000.0);
  m->SetCrossSectionalArea(0.02);
  m->SetMomentOfInertia(0.004);
  femObject->AddNextMaterial(m.GetPointer());

  // std::cout << "Material\n";

  itk::fem::Element2DC0LinearQuadrilateralMembrane::Pointer e0 =
    itk::fem::Element2DC0LinearQuadrilateralMembrane::New();

  e0->SetGlobalNumber(0);
  e0->SetNode( 0, femObject->GetNode(0).GetPointer() );
  e0->SetNode( 1, femObject->GetNode(1).GetPOinter() );
  e0->SetNode( 2, femObject->GetNode(2).GetPointer() );
  e0->SetNode( 3, femObject->GetNode(3).GetPointer() );
  e0->SetMaterial( femObject->GetMaterial(0).GetPointer() );

  femObject->AddNextElement( e0.GetPointer());

  // std::cout << "Element\n";

  itk::fem::LoadBC::Pointer l1 = itk::fem::LoadBC::New();
  l1->SetElement(e0);
  l1->SetGlobalNumber(0);
  l1->SetDegreeOfFreedom(0);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1 );

  // std::cout << "BC\n";
  itk::fem::LoadPoint::Pointer lm0 = itk::fem::LoadPoint::New();
  lm0->SetGlobalNumber(1);
  vnl_vector<double> pt1(2);
  pt1[0] = 0.5; pt1[1] = 0.5;
  // it is assumed that source is same as the point.
  lm0->SetPoint( pt1 );
  pt1[0] = 0.0; pt1[1] = 1.0;
  lm0->SetForce( pt1 );
  lm0->AddNextElement(e0.GetPointer());
  femObject->AddNextLoad( lm0 );

  femObject->Solve();

  int               numDOF = femObject->GetNumberOfDegreesOfFreedom();
  vnl_vector<float> soln(numDOF);
  for( int i = 0; i < numDOF; i++ )
    {
    soln[i] = femObject->GetSolution(i);
    }

  std::cout << "Test PASSED!\n";
  return EXIT_SUCCESS;
}
