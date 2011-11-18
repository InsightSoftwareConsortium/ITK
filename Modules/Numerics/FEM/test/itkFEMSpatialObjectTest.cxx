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


#include "itkFEMSpatialObjectReader.h"


int itkFEMSpatialObjectTest(int argc, char *argv[])
{
  if(argc < 2)
    {
    return EXIT_FAILURE;
    }
  //Need to register default FEM object types,
  //and setup SpatialReader to recognize FEM types
  //which is all currently done as a HACK in
  //the initializaiton of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();


  std::cout << "Read Spatial Object" << std::endl;
  typedef itk::FEMSpatialObjectReader<2>      FEMSpatialObjectReaderType;
  typedef FEMSpatialObjectReaderType::Pointer FEMSpatialObjectReaderPointer;
  FEMSpatialObjectReaderPointer SpatialReader = FEMSpatialObjectReaderType::New();
  SpatialReader->SetFileName( argv[1] );
  SpatialReader->Update();


/*
  FEMSpatialObjectReaderType::ScenePointer myScene = SpatialReader->GetScene();

  typedef itk::FEMObjectSpatialObject<2>      FEMObjectSpatialObjectType;
  typedef FEMObjectSpatialObjectType::Pointer FEMObjectSpatialObjectPointer;
  FEMObjectSpatialObjectType::ChildrenListType* children = SpatialReader->GetGroup()->GetChildren();

  FEMObjectSpatialObjectType::Pointer femSO =
    dynamic_cast<FEMObjectSpatialObjectType *>( (*(children->begin() ) ).GetPointer() );

  delete children;
*/
  //femSO->GetFEMObject()->FinalizeMesh();


  std::cout << "Overall Test : [PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
