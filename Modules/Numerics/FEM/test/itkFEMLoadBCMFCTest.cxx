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
#include "itkFEMSpatialObjectReader.h"
#include "itkFEMSpatialObjectWriter.h"

int itkFEMLoadBCMFCTest(int argc, char *argv[])
{
  if(argc < 1)
    {
    std::cerr << "Missing Spatial Object Filename" << std::endl;
    return EXIT_FAILURE;
    }
  //Need to register default FEM object types,
  //and setup SpatialReader to recognize FEM types
  //which is all currently done as a HACK in
  //the initializaiton of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  typedef itk::fem::Solver<2> Solver2DType;
  Solver2DType::Pointer solver = Solver2DType::New();

  typedef itk::FEMSpatialObjectReader<2>      FEMSpatialObjectReaderType;
  typedef FEMSpatialObjectReaderType::Pointer FEMSpatialObjectReaderPointer;
  FEMSpatialObjectReaderPointer SpatialReader = FEMSpatialObjectReaderType::New();
  SpatialReader->SetFileName( argv[1] );
//  SpatialReader->SetFileName("C:/Research/ITKGit/ITK/Testing/Data/Input/FEM/LoadBCMFCTest.meta");
  SpatialReader->Update();

  FEMSpatialObjectReaderType::ScenePointer myScene = SpatialReader->GetScene();
  if( !myScene )
    {
    std::cout << "No Scene : [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [PASSED]" << std::endl;

  // Testing the fe mesh validity
  typedef itk::FEMObjectSpatialObject<2>      FEMObjectSpatialObjectType;

  FEMObjectSpatialObjectType::ChildrenListType* children = SpatialReader->GetGroup()->GetChildren();

  if( strcmp( (*(children->begin() ) )->GetTypeName(), "FEMObjectSpatialObject") )
    {
    std::cout << " [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  FEMObjectSpatialObjectType::Pointer femSO =
    dynamic_cast<FEMObjectSpatialObjectType *>( (*(children->begin() ) ).GetPointer() );
  if (!femSO)
    {
    std::cout << " dynamic_cast [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  delete children;

  femSO->GetFEMObject()->FinalizeMesh();
  //   femSO->GetFEMObject()->Solve();
  solver->SetInput( femSO->GetFEMObject() );
  solver->Update();

  int               numDOF = femSO->GetFEMObject()->GetNumberOfDegreesOfFreedom();
  vnl_vector<float> soln(numDOF);
  float             expectedResult[10] = {0.283525f, 0.0f, 0.283525f, 1.70115f, 0.283525f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f};

  bool foundError = false;
  for( int i = 0; i < numDOF; i++ )
    {
    soln[i] = solver->GetSolution(i);
    // std::cout << "Solution[" << i << "]:" << soln[i] << std::endl;
    if( std::abs(expectedResult[i] - soln[i]) > 0.0001 )
      {
      std::cout << "ERROR: Index " << i << ". Expected " << expectedResult[i] << " Solution " << soln[i] << std::endl;
      foundError = true;
      }
    }

  if( foundError )
    {
    std::cout << "Test FAILED!" << std::endl;
    return EXIT_FAILURE;
    }

  // to write the deformed mesh
  FEMObjectSpatialObjectType::Pointer femSODef = FEMObjectSpatialObjectType::New();
  femSODef->SetFEMObject(solver->GetOutput() );
  typedef itk::FEMSpatialObjectWriter<2>      FEMSpatialObjectWriterType;
  typedef FEMSpatialObjectWriterType::Pointer FEMSpatialObjectWriterPointer;
  FEMSpatialObjectWriterPointer SpatialWriter = FEMSpatialObjectWriterType::New();
  SpatialWriter->SetInput(femSODef);
  SpatialWriter->SetFileName( argv[2] );
  SpatialWriter->Update();

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
