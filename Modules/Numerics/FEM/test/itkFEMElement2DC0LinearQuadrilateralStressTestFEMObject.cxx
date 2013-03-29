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

//  Example taken from 'Fundamentals of the Finite ELement Method' - Grandin
int itkFEMElement2DC0LinearQuadrilateralStressTestFEMObject(int argc, char *argv[])
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

  pt[0] = 2.0;
  pt[1] = 2.0;
  n1->SetCoordinates(pt);

  femObject->AddNextNode(n1.GetPointer());

  n1 = itk::fem::Node::New();
  pt[0] = 8.0;
  pt[1] = 3.0;
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1.GetPointer());

  n1 = itk::fem::Node::New();
  pt[0] = 8.0;
  pt[1] = 6.0;
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1.GetPointer());

  n1 = itk::fem::Node::New();
  pt[0] = 2.0;
  pt[1] = 9.0;
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1.GetPointer());

  femObject->RenumberNodeContainer();

  itk::fem::MaterialLinearElasticity::Pointer m;
  m = itk::fem::MaterialLinearElasticity::New();
  m->SetGlobalNumber(0);           /* Global number of the material */
  m->SetYoungsModulus(30000000.0); /* Young modulus */
  m->SetPoissonsRatio(0.3);
  m->SetCrossSectionalArea(.0);   /* Crossection area */
  m->SetMomentOfInterita(1.0);    /* Momemt of inertia */
  femObject->AddNextMaterial(m.GetPointer());

  itk::fem::Element2DC0LinearQuadrilateralStress::Pointer e1;

  e1 = itk::fem::Element2DC0LinearQuadrilateralStress::New();

  e1->SetGlobalNumber(0);
  e1->SetNode( 0, femObject->GetNode(0).GetPointer() );
  e1->SetNode( 1, femObject->GetNode(1).GetPointer() );
  e1->SetNode( 2, femObject->GetNode(2).GetPointer() );
  e1->SetNode( 3, femObject->GetNode(3).GetPointer() );

  e1->SetMaterial( femObject->GetMaterial(0).GetPointer() );
  femObject->AddNextElement( e1.GetPointer());

  itk::fem::LoadBC::Pointer l1;
  l1 = itk::fem::LoadBC::New();
  l1->SetElement( femObject->GetElement(0) )
  l1->SetDegreeOfFreedom(0);
  l1->SetValue( vnl_vector<double>(0, 0.0) );
  femObject->AddNextLoad( l1 );

  l1 = itk::fem::LoadBC::New();
  l1->SetElement( femObject->GetElement(0) )
  l1->SetDegreeOfFreedom(1);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1 );

  l1 = itk::fem::LoadBC::New();
  l1->SetElement( femObject->GetElement(0) );
  l1->SetDegreeOfFreedom(6);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1 );

  l1 = itk::fem::LoadBC::New();
  l1->SetElement( femObject->GetElement(0) );
  l1->SetDegreeOfFreedom(7);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1 );

  itk::fem::LoadNode::Pointer l2;

  l2 = itk::fem::LoadNode::New();
  l2->SetElement( femObject->GetElement(0) );
  l2->SetNode(1);
  vnl_vector<double> F(2);
  F[0] = 5;
  F[1] = 0;
  l2->SetForce(F);
  femObject->AddNextLoad( l2 );

  l2 = itk::fem::LoadNode::New();
  l2->SetElement( femObject->GetElement(0) );
  l2->SetNode(2);
  vnl_vector<double> F1(2);
  F1[0] = 10;
  F1[1] = 0;
  l2->SetForce(F1);
  femObject->AddNextLoad( l2 );

  femObject->Solve();

  float soln[8];
  for( int i = 0; i < 8; i++ )
    {
    soln[i] = femObject->GetSolution(i);
    std::cout << "Solution[" << i << "]:" << soln[i] << std::endl;
    }
  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
