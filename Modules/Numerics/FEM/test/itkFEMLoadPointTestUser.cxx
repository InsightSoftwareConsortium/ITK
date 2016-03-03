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

#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEMElementBase.h"
#include "itkFEMLinearSystemWrapperDenseVNL.h"
#include "itkFEMLinearSystemWrapperItpack.h"
#include "itkFEMLinearSystemWrapperVNL.h"
#include "itkFEMLoadNode.h"
#include "itkFEMLoadPoint.h"
#include "itkFEMObject.h"
#include "itkFEMSolver.h"
#include "itkTestingMacros.h"


#include <iostream>
using std::ofstream;
using std::ifstream;

//
int itkFEMLoadPointTestUser(int, char *[])
{

  //Need to register default FEM object types,
  //and setup SpatialReader to recognize FEM types
  //which is all currently done as a HACK in
  //the initializaiton of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  typedef itk::fem::FEMObject<2> FEMObjectType;
  FEMObjectType::Pointer femObject = FEMObjectType::New();

  typedef itk::fem::Solver<2> Solver2DType;
  Solver2DType::Pointer solver = Solver2DType::New();


  itk::fem::Element::Node::Pointer n1;

  n1 = itk::fem::Element::Node::New();
  itk::fem::Element::VectorType pt(2);

  pt[0] = 0.;
  pt[1] = 0.;
  n1->SetGlobalNumber(0);
  n1->SetCoordinates(pt);

  femObject->AddNextNode(n1);

  n1 = itk::fem::Element::Node::New();
  pt[0] = 1.;
  pt[1] = 1.;
  n1->SetGlobalNumber(1);
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1);

  n1 = itk::fem::Element::Node::New();
  pt[0] = 3.;
  pt[1] = 2.;
  n1->SetGlobalNumber(2);
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1);

  n1 = itk::fem::Element::Node::New();
  pt[0] = 0.;
  pt[1] = 3.;
  n1->SetGlobalNumber(3);
  n1->SetCoordinates(pt);
  femObject->AddNextNode(n1);

  femObject->RenumberNodeContainer();


  itk::fem::MaterialLinearElasticity::Pointer m;
  m = itk::fem::MaterialLinearElasticity::New();
  m->SetGlobalNumber(0);
  m->SetYoungsModulus(30000.0);
  m->SetCrossSectionalArea(0.02);
  m->SetMomentOfInertia(0.004);
  femObject->AddNextMaterial(m);


  itk::fem::Element2DC0LinearQuadrilateralMembrane::Pointer e0 =
    itk::fem::Element2DC0LinearQuadrilateralMembrane::New();

  e0->SetGlobalNumber(0);
  e0->SetNode( 0, femObject->GetNode(0) );
  e0->SetNode( 1, femObject->GetNode(1) );
  e0->SetNode( 2, femObject->GetNode(2) );
  e0->SetNode( 3, femObject->GetNode(3) );
  e0->SetMaterial( femObject->GetMaterial(0).GetPointer() );

  femObject->AddNextElement(e0.GetPointer());


  itk::fem::LoadBC::Pointer l1 = itk::fem::LoadBC::New();
  l1->SetElement(e0);
  l1->SetGlobalNumber(0);
  l1->SetDegreeOfFreedom(0);
  l1->SetValue( vnl_vector<double>(1, 0.0) );
  femObject->AddNextLoad( l1 );


  itk::fem::LoadPoint::Pointer lm0 = itk::fem::LoadPoint::New();

  EXERCISE_BASIC_OBJECT_METHODS( lm0, LoadPoint, LoadElement );

  lm0->SetGlobalNumber(1);
  vnl_vector<double> pt1(2);
  pt1[0] = 0.5;
  pt1[1] = 0.5;
  // it is assumed that source is same as the point.
  lm0->SetPoint( pt1 );
  TEST_SET_GET_VALUE( pt1, lm0->GetPoint() );

  pt1[0] = 0.0;
  pt1[1] = 1.0;
  lm0->SetForce( pt1 );
  TEST_SET_GET_VALUE( pt1, lm0->GetForce() );

  femObject->AddNextLoad(lm0.GetPointer());

  femObject->FinalizeMesh();

  solver->SetInput(femObject);

  itk::fem::LinearSystemWrapperDenseVNL lsw_dvnl;
  itk::fem::LinearSystemWrapperItpack   lsw_itpack;
  itk::fem::LinearSystemWrapperVNL      lsw_vnl;

  // Solvers being tested
  int numsolvers = 3;

  for( int s = 0; s < numsolvers; s++ )
    {
    if( s == 2 )
      {
      // Itpack
      std::cout << "Using LinearSystemWrapperItpack" << std::endl;
      lsw_itpack.SetMaximumNonZeroValuesInMatrix(1000);
      solver->SetLinearSystemWrapper(&lsw_itpack);
      }
    else if( s == 1 )
      {
      // Dense VNL
      std::cout << "Using LinearSystemWrapperDenseVNL" << std::endl;
      solver->SetLinearSystemWrapper(&lsw_dvnl);
      }
    else
      {
      // Sparse VNL - default
      std::cout << "Using LinearSystemWrapperVNL" << std::endl;
      solver->SetLinearSystemWrapper(&lsw_vnl);
      }

    solver->Update();

    int numDOF = femObject->GetNumberOfDegreesOfFreedom();
    vnl_vector<float> soln(numDOF);
    for( int i = 0; i < numDOF; i++ )
      {
      soln[i] = solver->GetSolution(i);
      }

    }

  std::cout << "Test PASSED!\n";
  return EXIT_SUCCESS;
}
