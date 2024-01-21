/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/


#include "itkFEMObject.h"
#include "itkFEMObjectSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkSpatialObject.h"
#include "itkFEMSpatialObjectReader.h"
#include "itkFEMSpatialObjectWriter.h"

#include <iostream>

//  Example taken from 'Fundamentals of the Finite ELement Method' - Grandin
int
itkFEMElement2DC0LinearQuadrilateralStressTestFEMObjectReader(int argc, char * argv[])
{
  // Need to register default FEM object types,
  // and setup spatialReader to recognize FEM types
  // which is all currently done as a HACK in
  // the initialization of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  using SpatialObjectType = itk::SpatialObject<2>;
  using SpatialObjectPointer = SpatialObjectType::Pointer;
  SpatialObjectPointer Spatial = SpatialObjectType::New();

  using FEMSpatialObjectReaderType = itk::FEMSpatialObjectReader<2>;
  using FEMSpatialObjectReaderPointer = FEMSpatialObjectReaderType::Pointer;
  FEMSpatialObjectReaderPointer spatialReader = FEMSpatialObjectReaderType::New();
  spatialReader->SetFileName("C:/Research/ITKGit/ITK/Testing/Data/Input/FEM/Trial.meta");
  spatialReader->Update();

  using FEMSpatialObjectWriterType = itk::FEMSpatialObjectWriter<2>;
  using FEMSpatialObjectWriterPointer = FEMSpatialObjectWriterType::Pointer;
  FEMSpatialObjectWriterPointer spatialWriter = FEMSpatialObjectWriterType::New();
  spatialWriter->SetInput(spatialReader->GetScene());
  spatialWriter->SetFileName("C:/Research/ITKGit/ITK/Testing/Data/Input/FEM/TrialWrite.meta");
  spatialWriter->Update();

  FEMSpatialObjectReaderType::ScenePointer myScene = spatialReader->GetScene();
  if (!myScene)
  {
    std::cout << "No Scene : [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << " [PASSED]" << std::endl;

  // Testing the fe mesh validity
  using FEMObjectSpatialObjectType = itk::FEMObjectSpatialObject<2>;
  using FEMObjectSpatialObjectPointer = FEMObjectSpatialObjectType::Pointer;

  FEMObjectSpatialObjectType::ChildrenListType * children = spatialReader->GetGroup()->GetChildren();
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

  float soln[8];
  for (int i = 0; i < 8; ++i)
  {
    soln[i] = femSO->GetFEMObject()->GetSolution(i);
    std::cout << "Solution[" << i << "]:" << soln[i] << std::endl;
  }

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
