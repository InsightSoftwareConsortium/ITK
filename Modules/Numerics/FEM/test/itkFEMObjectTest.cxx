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


#include "itkFEMElement2DC0LinearLineStress.h"
#include "itkFEMSpatialObjectWriter.h"


int itkFEMObjectTest(int, char *[])
{
  //Need to register default FEM object types,
  //and setup SpatialReader to recognize FEM types
  //which is all currently done as a HACK in
  //the initializaiton of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();


  // Testing the fe mesh validity
  typedef itk::fem::FEMObject<2>      FEMObjectType;
  typedef FEMObjectType::Pointer      FEMObjectTypePointer;

  FEMObjectTypePointer femObject = FEMObjectType::New();

  typedef itk::fem::Element::Node NodeType;

  NodeType::Pointer n1;

  itk::fem::Element::VectorType pt(2);

  n1 = NodeType::New();
  pt[0] = 0.0;
  pt[1] = 0.0;
  n1->SetCoordinates(pt);
  n1->SetGlobalNumber(0);
  femObject->AddNextNode(n1.GetPointer());

  n1 = NodeType::New();
  pt[0] = 1500.0;
  pt[1] = 0.0;
  n1->SetCoordinates(pt);
  n1->SetGlobalNumber(1);
  femObject->AddNextNode(n1.GetPointer());

  n1 = NodeType::New();
  pt[0] = 3000.0;
  pt[1] = 0.0;
  n1->SetCoordinates(pt);
  n1->SetGlobalNumber(2);
  femObject->AddNextNode(n1.GetPointer());

  n1 = NodeType::New();
  pt[0] = 3000.0;
  pt[1] = 3000.0;
  n1->SetCoordinates(pt);
  n1->SetGlobalNumber(3);
  femObject->AddNextNode(n1.GetPointer());

  n1 = NodeType::New();
  pt[0] = 0.0;
  pt[1] = 4500.0;
  n1->SetCoordinates(pt);
  n1->SetGlobalNumber(4);
  femObject->AddNextNode(n1.GetPointer());

  itk::fem::MaterialLinearElasticity::Pointer m;
  m = itk::fem::MaterialLinearElasticity::New();
  m->SetGlobalNumber(0);               /* Global number of the material */
  m->SetYoungsModulus(200000000000.0); /* Young modulus */
  m->SetPoissonsRatio(0.3);
  m->SetCrossSectionalArea(2000.0); /* Crossection area */
  m->SetMomentOfInertia(1.0);       /* Momemt of inertia */
  femObject->AddNextMaterial(m);

  m = itk::fem::MaterialLinearElasticity::New();
  m->SetGlobalNumber(1);         /* Global number of the material */
  m->SetYoungsModulus(200000.0); /* Young modulus */
  m->SetPoissonsRatio(0.3);
  m->SetCrossSectionalArea(1200.0); /* Crossection area */
  m->SetMomentOfInertia(1.0);       /* Momemt of inertia */
  femObject->AddNextMaterial(m);

  m = itk::fem::MaterialLinearElasticity::New();
  m->SetGlobalNumber(2);        /* Global number of the material */
  m->SetYoungsModulus(70000.0); /* Young modulus */
  m->SetPoissonsRatio(0.3);
  m->SetCrossSectionalArea(900.0); /* Crossection area */
  m->SetMomentOfInertia(1.0);      /* Momemt of inertia */
  femObject->AddNextMaterial(m);

  itk::fem::Element2DC0LinearLineStress::Pointer e1;

  e1 = itk::fem::Element2DC0LinearLineStress::New();
  e1->SetGlobalNumber(0);
  e1->SetNode( 0, femObject->GetNode(0) );
  e1->SetNode( 1, femObject->GetNode(1) );
  if ( dynamic_cast<itk::fem::MaterialLinearElasticity *>( femObject->GetMaterial(1).GetPointer() ))
    {
    e1->SetMaterial( dynamic_cast<itk::fem::MaterialLinearElasticity *>( femObject->GetMaterial(0).GetPointer() ) );
    }
  femObject->AddNextElement( e1.GetPointer());

  e1 = itk::fem::Element2DC0LinearLineStress::New();
  e1->SetGlobalNumber(1);
  e1->SetNode( 0, femObject->GetNode(1) );
  e1->SetNode( 1, femObject->GetNode(2) );
  if ( dynamic_cast<itk::fem::MaterialLinearElasticity *>( femObject->GetMaterial(0).GetPointer() ))
    {
    e1->SetMaterial( dynamic_cast<itk::fem::MaterialLinearElasticity *>( femObject->GetMaterial(0).GetPointer() ) );
    }
  femObject->AddNextElement( e1.GetPointer());

  e1 = itk::fem::Element2DC0LinearLineStress::New();
  e1->SetGlobalNumber(2);
  e1->SetNode( 0, femObject->GetNode(1) );
  e1->SetNode( 1, femObject->GetNode(3) );
  if ( dynamic_cast<itk::fem::MaterialLinearElasticity *>( femObject->GetMaterial(2).GetPointer() ))
    {
    e1->SetMaterial( dynamic_cast<itk::fem::MaterialLinearElasticity *>( femObject->GetMaterial(2).GetPointer() ) );
    }
  femObject->AddNextElement( e1.GetPointer());

  e1 = itk::fem::Element2DC0LinearLineStress::New();
  e1->SetGlobalNumber(3);
  e1->SetNode( 0, femObject->GetNode(0) );
  e1->SetNode( 1, femObject->GetNode(4) );
  if ( dynamic_cast<itk::fem::MaterialLinearElasticity *>( femObject->GetMaterial(1).GetPointer() ))
    {
    e1->SetMaterial( dynamic_cast<itk::fem::MaterialLinearElasticity *>( femObject->GetMaterial(1).GetPointer() ) );
    }
  femObject->AddNextElement( e1.GetPointer());

  itk::fem::LoadBC::Pointer l1;

  l1 = itk::fem::LoadBC::New();
  l1->SetGlobalNumber(0);
  l1->SetElement( femObject->GetElement(2) );
  l1->SetDegreeOfFreedom(2);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1 );

  l1 = itk::fem::LoadBC::New();
  l1->SetGlobalNumber(1);
  l1->SetElement( femObject->GetElement(2) );
  l1->SetDegreeOfFreedom(3);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1 );

  l1 = itk::fem::LoadBC::New();
  l1->SetGlobalNumber(2);
  l1->SetElement( femObject->GetElement(3) );
  l1->SetDegreeOfFreedom(2);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1 );

  l1 = itk::fem::LoadBC::New();
  l1->SetGlobalNumber(3);
  l1->SetElement( femObject->GetElement(3) );
  l1->SetDegreeOfFreedom(3);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1 );

  itk::fem::LoadNode::Pointer l2;

  l2 = itk::fem::LoadNode::New();
  l2->SetGlobalNumber(4);
  l2->SetElement( femObject->GetElement(1) );
  l2->SetNode(0);
  vnl_vector<double> F(2);
  F[0] = 0;
  F[1] = 30000;
  l2->SetForce(F);
  femObject->AddNextLoad( l2 );

  itk::fem::LoadBCMFC::Pointer bcmfc = itk::fem::LoadBCMFC::New();
  bcmfc->SetGlobalNumber(5);
  bcmfc->AddLeftHandSideTerm( itk::fem::LoadBCMFC::MFCTerm(femObject->GetElement(0).GetPointer(), 1, 1) );
  bcmfc->AddLeftHandSideTerm( itk::fem::LoadBCMFC::MFCTerm(femObject->GetElement(1).GetPointer(), 3, -1) );
  bcmfc->AddRightHandSideTerm(0.0);
  femObject->AddNextLoad( bcmfc );
  femObject->FinalizeMesh();

  std::cout << "Overall Test : [PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
