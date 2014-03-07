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


#include "itkFEMSolver.h"
#include "itkFEMSpatialObjectWriter.h"
#include "itkFEMElement2DC0LinearQuadrilateralStress.h"

int itkFEMElement2DC0LinearQuadrilateralStressTest(int argc, char *argv[])
{
  if(argc < 1)
    {
    std::cerr << "Missing Spatial Object Filename" << std::endl;
    return EXIT_FAILURE;
    }
  itk::FEMFactoryBase::RegisterDefaultTypes();

  const unsigned int Dimension = 2;
  typedef itk::fem::Solver<Dimension> Solver2DType;
  Solver2DType::Pointer solver = Solver2DType::New();

  typedef itk::fem::FEMObject<Dimension> FEMObjectType;
  FEMObjectType::Pointer femObject = FEMObjectType::New();

  typedef itk::fem::Element::Node NodeType;
  NodeType::Pointer n1;

  n1 = NodeType::New();
  itk::fem::Element::VectorType pt(Dimension);

  pt[0] = 2.0;
  pt[1] = 2.0;
  n1->SetCoordinates(pt);

  femObject->AddNextNode(n1);

  n1 = NodeType::New();
  pt[0] = 8.0;
  pt[1] = 3.0;
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1);

  n1 = NodeType::New();
  pt[0] = 8.0;
  pt[1] = 6.0;
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1);

  n1 = NodeType::New();
  pt[0] = 2.0;
  pt[1] = 9.0;
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1);

  femObject->RenumberNodeContainer();

  itk::fem::MaterialLinearElasticity::Pointer m;
  m = itk::fem::MaterialLinearElasticity::New();
  m->SetGlobalNumber(0);           /* Global number of the material */
  m->SetYoungsModulus(30000000.0); /* Young modulus */
  m->SetPoissonsRatio(0.3);
  m->SetCrossSectionalArea(.0);  /* Crossection area */
  m->SetMomentOfInertia(1.0);    /* Momemt of inertia */
  femObject->AddNextMaterial(m);

  itk::fem::Element2DC0LinearQuadrilateralStress::Pointer e1;

  e1 = itk::fem::Element2DC0LinearQuadrilateralStress::New();

  e1->SetGlobalNumber(0);
  e1->SetNode( 0, femObject->GetNode(0) );
  e1->SetNode( 1, femObject->GetNode(1) );
  e1->SetNode( 2, femObject->GetNode(2) );
  e1->SetNode( 3, femObject->GetNode(3) );

  e1->SetMaterial( femObject->GetMaterial(0) );
  femObject->AddNextElement( e1.GetPointer() );

  itk::fem::LoadBC::Pointer l1;
  l1 = itk::fem::LoadBC::New();
  l1->SetGlobalNumber(0);
  l1->SetElement( femObject->GetElement(0) );
  l1->SetDegreeOfFreedom(0);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1 );

  l1 = itk::fem::LoadBC::New();
  l1->SetGlobalNumber(1);
  l1->SetElement( femObject->GetElement(0) );
  l1->SetDegreeOfFreedom(1);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1  );

  l1 = itk::fem::LoadBC::New();
  l1->SetGlobalNumber(2);
  l1->SetElement( femObject->GetElement(0) );
  l1->SetDegreeOfFreedom(6);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1  );

  l1 = itk::fem::LoadBC::New();
  l1->SetGlobalNumber(3);
  l1->SetElement( femObject->GetElement(0) );
  l1->SetDegreeOfFreedom(7);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1  );

  itk::fem::LoadNode::Pointer l2;

  l2 = itk::fem::LoadNode::New();
  l2->SetGlobalNumber(4);
  l2->SetElement( femObject->GetElement(0) );
  l2->SetNode(1);

  vnl_vector<double> F(Dimension);
  F[0] = 5;
  F[1] = 0;
  l2->SetForce(F);
  femObject->AddNextLoad( l2 );

  l2 = itk::fem::LoadNode::New();
  l2->SetGlobalNumber(5);
  l2->SetElement( femObject->GetElement(0) );
  l2->SetNode(2);

  vnl_vector<double> F1(Dimension);
  F1[0] = 10;
  F1[1] = 0;
  l2->SetForce(F1);
  femObject->AddNextLoad( l2 );

  femObject->FinalizeMesh();

  solver->SetInput( femObject );
  solver->Update();

  // to write the deformed mesh
  // Testing the fe mesh validity
  typedef itk::FEMObjectSpatialObject<Dimension> FEMObjectSpatialObjectType;
  FEMObjectSpatialObjectType::Pointer femSODef = FEMObjectSpatialObjectType::New();
  femSODef->SetFEMObject(solver->GetOutput() );

  typedef itk::FEMSpatialObjectWriter<Dimension> FEMSpatialObjectWriterType;
  typedef FEMSpatialObjectWriterType::Pointer    FEMSpatialObjectWriterPointer;
  FEMSpatialObjectWriterPointer SpatialWriter = FEMSpatialObjectWriterType::New();
  SpatialWriter->SetInput(femSODef);
  SpatialWriter->SetFileName( argv[1] );
  SpatialWriter->Update();

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
