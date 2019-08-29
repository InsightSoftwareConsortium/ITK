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
#include "itkFEMObject.h"
#include "itkFEMObjectSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkSpatialObject.h"
#include "itkFEMSpatialObjectReader.h"
#include "itkFEMSpatialObjectWriter.h"

int
itkFEMSolverTest2D(int argc, char * argv[])
{
  // Need to register default FEM object types,
  // and setup SpatialReader to recognize FEM types
  // which is all currently done as a HACK in
  // the initializaiton of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  using Solver2DType = itk::fem::Solver<2>;
  Solver2DType::Pointer solver = Solver2DType::New();

  using SpatialObjectType = itk::SpatialObject<2>;
  using SpatialObjectPointer = SpatialObjectType::Pointer;
  SpatialObjectPointer Spatial = SpatialObjectType::New();

  using FEMSpatialObjectReaderType = itk::FEMSpatialObjectReader<2>;
  using FEMSpatialObjectReaderPointer = FEMSpatialObjectReaderType::Pointer;
  FEMSpatialObjectReaderPointer SpatialReader = FEMSpatialObjectReaderType::New();
  SpatialReader->SetFileName(argv[1]);
  SpatialReader->Update();

  FEMSpatialObjectReaderType::ScenePointer myScene = SpatialReader->GetScene();
  if (!myScene)
  {
    std::cout << "No Scene : [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << " [PASSED]" << std::endl;

  // Testing the FE mesh validity
  using FEMObjectSpatialObjectType = itk::FEMObjectSpatialObject<2>;
  using FEMObjectSpatialObjectPointer = FEMObjectSpatialObjectType::Pointer;

  FEMObjectSpatialObjectType::ChildrenListType * children = SpatialReader->GetGroup()->GetChildren();
  if (strcmp((*(children->begin()))->GetTypeName(), "FEMObjectSpatialObject"))
  {
    std::cout << " [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  FEMObjectSpatialObjectType::Pointer femSO =
    dynamic_cast<FEMObjectSpatialObjectType *>((*(children->begin())).GetPointer());
  if (!femSO)
  {
    std::cout << " dynamic_cast [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  delete children;

  femSO->GetFEMObject()->Solve();

  solver->SetInput(femSO->GetFEMObject());
  solver->Update();

  int               numDOF = femSO->GetFEMObject()->GetNumberOfDegreesOfFreedom();
  vnl_vector<float> soln(numDOF);
  for (int i = 0; i < numDOF; i++)
  {
    soln[i] = femSO->GetFEMObject()->GetSolution(i);
    std::cout << "Solution[" << i << "]:" << soln[i] << std::endl;
  }

  using FEMSpatialObjectWriterType = itk::FEMSpatialObjectWriter<2>;
  using FEMSpatialObjectWriterPointer = FEMSpatialObjectWriterType::Pointer;
  FEMSpatialObjectWriterPointer SpatialWriter = FEMSpatialObjectWriterType::New();
  SpatialWriter->SetInput(SpatialReader->GetScene());
  SpatialWriter->SetFileName(argv[2]);
  SpatialWriter->Update();

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
