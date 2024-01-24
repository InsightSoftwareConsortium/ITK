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


#include "itkFEMSolver.h"
#include "itkFEMSpatialObjectReader.h"
#include "itkFEMSpatialObjectWriter.h"
#include "itkTestingMacros.h"

int
itkFEMElement2DC0QuadraticTriangleStrainTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName outputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  // Need to register default FEM object types,
  // and setup spatialReader to recognize FEM types
  // which is all currently done as a HACK in
  // the initialization of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  using Solver2DType = itk::fem::Solver<2>;
  auto solver = Solver2DType::New();

  using FEMSpatialObjectReaderType = itk::FEMSpatialObjectReader<2>;
  using FEMSpatialObjectReaderPointer = FEMSpatialObjectReaderType::Pointer;
  FEMSpatialObjectReaderPointer spatialReader = FEMSpatialObjectReaderType::New();
  spatialReader->SetFileName(argv[1]);
  spatialReader->Update();

  FEMSpatialObjectReaderType::GroupPointer myGroup = spatialReader->GetGroup();
  if (!myGroup)
  {
    std::cout << "No Group : [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << " [PASSED]" << std::endl;

  // Testing the fe mesh validity
  using FEMObjectSpatialObjectType = itk::FEMObjectSpatialObject<2>;

  FEMObjectSpatialObjectType::ChildrenListType * children = spatialReader->GetGroup()->GetChildren();

  if (children->front()->GetTypeName() != "FEMObjectSpatialObject")
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

  femSO->GetFEMObject()->FinalizeMesh();

  solver->SetInput(femSO->GetFEMObject());
  solver->Update();

  // to write the deformed mesh
  auto femSODef = FEMObjectSpatialObjectType::New();
  femSODef->SetFEMObject(solver->GetOutput());
  using FEMSpatialObjectWriterType = itk::FEMSpatialObjectWriter<2>;
  using FEMSpatialObjectWriterPointer = FEMSpatialObjectWriterType::Pointer;
  FEMSpatialObjectWriterPointer spatialWriter = FEMSpatialObjectWriterType::New();
  spatialWriter->SetInput(femSODef);
  spatialWriter->SetFileName(argv[2]);
  spatialWriter->Update();

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
