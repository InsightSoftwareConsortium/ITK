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

#include "itkSpatialObjectWriter.h"
#include "itkSpatialObjectReader.h"
#include "itkDefaultDynamicMeshTraits.h"
#include <iostream>
#include "itkMath.h"


int itkMeshSpatialObjectIOTest(int argc, char* argv[])
{
  typedef itk::DefaultDynamicMeshTraits< float , 3, 3 > MeshTrait;
  typedef itk::Mesh<float,3,MeshTrait>                  MeshType;
  typedef MeshType::CellTraits                          CellTraits;
  typedef itk::CellInterface< float, CellTraits >       CellInterfaceType;
  typedef itk::TetrahedronCell<CellInterfaceType>       TetraCellType;
  typedef itk::HexahedronCell<CellInterfaceType>        HexaCellType;
  typedef itk::MeshSpatialObject<MeshType>              MeshSpatialObjectType;
  typedef MeshType::PointType                           PointType;
  typedef MeshType::CellType                            CellType;
  typedef CellType::CellAutoPointer                     CellAutoPointer;

  // Create an itkMesh
  std::cout << "Creating Mesh File: ";
  MeshType::Pointer mesh = MeshType::New();

  MeshType::CoordRepType testPointCoords[8][3]
    = { {0,1,2}, {1,2,3}, {2,3,4}, {3,4,5}, {4,5,6}, {5,6,7}, {6,7,8}, {7,8,9}};


  MeshType::PointIdentifier tetraPoints[4] = {0,1,2,3};
  MeshType::PointIdentifier hexaPoints[8] = {0,1,2,3,4,5,6,7};

  unsigned int i;
  unsigned int j;
  for(i=0; i < 8; ++i)
    {
    mesh->SetPoint(i, PointType(testPointCoords[i]));
    }

  mesh->SetCellsAllocationMethod( MeshType::CellsAllocatedDynamicallyCellByCell );
  CellAutoPointer testCell1;
  testCell1.TakeOwnership(  new TetraCellType );
  testCell1->SetPointIds(tetraPoints);
  mesh->SetCell(0, testCell1 );

  CellAutoPointer testCell2;
  testCell2.TakeOwnership(  new HexaCellType );
  testCell2->SetPointIds(hexaPoints);
  mesh->SetCell(0, testCell1 );
  mesh->SetCell(1, testCell2 );

   // Add cell links
  typedef MeshType::CellLinksContainer CellLinksContainerType;
  CellLinksContainerType::Pointer linkContainer = CellLinksContainerType::New();
  MeshType::PointCellLinksContainer pcl;

  for(j=0;j<3;j++)
    {
    for(i=0;i<5;i++)
      {
      pcl.insert(j+i);
      }
    linkContainer->SetElement(j,pcl);
    }
  mesh->SetCellLinks(linkContainer);


  // Add point data
  typedef MeshType::PointDataContainer PointDataContainer;
  PointDataContainer::Pointer pointData = PointDataContainer::New();

  float data = 0.1;
  for(j=0;j<2;j++)
    {
    pointData->SetElement(j, data);
    data += (float)0.1;
    }
  mesh->SetPointData(pointData);

  // Add cell data
  typedef  MeshType::CellDataContainer CellDataContainer;
  CellDataContainer::Pointer cellData = CellDataContainer::New();

  data = 0.9;
  for(j=0;j<3;j++)
    {
    cellData->SetElement(j, data);
    data -= (float)0.2;
    }
  mesh->SetCellData(cellData);

  // Create the mesh Spatial Object
  MeshSpatialObjectType::Pointer meshSO = MeshSpatialObjectType::New();
  meshSO->SetMesh(mesh);
  meshSO->SetId(3);
  std::cout<<"[PASSED]"<<std::endl;

  // Writing the file
  std::cout<<"Testing Writing MeshSpatialObject: ";
  typedef itk::SpatialObjectWriter<3,float,MeshTrait> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(meshSO);
  if((argc > 2) && (!strcmp(argv[2],"binary")))
    {
    std::cout << "Writing binary points" << std::endl;
    writer->SetBinaryPoints(true);
    }
  writer->SetFileName(argv[1]);
  writer->Update();
  std::cout<<"[PASSED]"<<std::endl;

  // Reading the file
  std::cout<<"Testing Reading MeshSpatialObject: ";
  typedef itk::SpatialObjectReader<3,float,MeshTrait> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  if((argc > 2) && (strcmp(argv[2],"binary")))
    {
    reader->SetFileName(argv[2]);
    }
  else
    {
    reader->SetFileName(argv[1]);
    }
  reader->Update();
  ReaderType::ScenePointer myScene = reader->GetScene();
  if(!myScene)
  {
    std::cout<<"No Scene : [FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }
  std::cout<<" [PASSED]"<<std::endl;

  // Testing the mesh validity
  MeshSpatialObjectType::ChildrenListType* children = reader->GetGroup()->GetChildren();
  if(strcmp((*(children->begin()))->GetTypeName(),"MeshSpatialObject"))
    {
    std::cout<<" [FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }

  MeshSpatialObjectType::Pointer meshSO2 = dynamic_cast<MeshSpatialObjectType*>((*(children->begin())).GetPointer());

  std::cout << "Testing ID : ";
  if(meshSO2->GetId() != 3)
    {
    std::cout<<" [FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  std::cout<<" [PASSED]"<<std::endl;

  std::cout<<"Testing Points: ";
  MeshType::Pointer mesh2 = meshSO2->GetMesh();
  // Testing points
  const MeshType::PointsContainer* points = mesh2->GetPoints();
  MeshType::PointsContainer::ConstIterator it_points = points->Begin();

  j=0;
  while(it_points != points->End())
    {
    if((*it_points)->Index() != j)
      {
      std::cout<<" [FAILED]"<<std::endl;
      std::cout << "Index = " << (*it_points)->Index() << " v.s. " << j << std::endl;
      return EXIT_FAILURE;
      }
    for(i=0;i<3;i++)
      {
      if(itk::Math::NotExactlyEquals(((*it_points)->Value())[i], j+i))
        {
        std::cout<<" [FAILED]"<<std::endl;
        std::cout << "Value = " << (*it_points)->Value() << std::endl;
        return EXIT_FAILURE;
        }
      }
    j++;
    it_points++;
    }
  std::cout<<" [PASSED]"<<std::endl;


  // Testing cells
  std::cout<<"Testing Cells : ";
  const MeshType::CellsContainer* cells = mesh2->GetCells();
  MeshType::CellsContainer::ConstIterator it_cells = cells->Begin();

  j=0;
  while(it_cells != cells->End())
    {
    MeshType::CellTraits::PointIdConstIterator itptids = (*it_cells)->Value()->GetPointIds();
    if((*it_cells)->Index() != j)
      {
      std::cout<<" [FAILED]"<<std::endl;
      std::cout << (*it_cells)->Index() << " v.s " << j << std::endl;
      return EXIT_FAILURE;
      }

    unsigned int ii=0;
    while(itptids != (*it_cells)->Value()->PointIdsEnd())
      {
      if(*itptids != ii)
        {
        std::cout<<" [FAILED]"<<std::endl;
        std::cout << *itptids << " v.s. " << ii << std::endl;
        return EXIT_FAILURE;
        }
      ii++;
      itptids++;
      }
    j++;
    it_cells++;
    }
  std::cout<<" [PASSED]"<<std::endl;
  delete children;

  // Testing celllinks
  std::cout<<"Testing CellLinks : ";
  j=0;
  typedef MeshType::CellLinksContainer  CellLinksContainer;
  const CellLinksContainer* links = mesh2->GetCellLinks();
  MeshType::CellLinksContainer::ConstIterator it_celllinks = links->Begin();

  while(it_celllinks != links->End())
    {
    if((*it_celllinks)->Index() != j)
      {
      std::cout<<" [FAILED]"<<std::endl;
      std::cout << "Index = " << (*it_celllinks)->Index() << " v.s " << j << std::endl;
      return EXIT_FAILURE;
      }

    i =0;
    MeshType::PointCellLinksContainer::const_iterator it = (*it_celllinks)->Value().begin();
    while(it != (*it_celllinks)->Value().end())
      {
      if( (*it) != i)
        {
        std::cout<<" [FAILED]"<<std::endl;
        std::cout << (*it) << " v.s " << i << std::endl;
        return EXIT_FAILURE;
        }
      i++;
      it++;
      }
    j++;
    it_celllinks++;
    }

  std::cout<<" [PASSED]"<<std::endl;

  // Testing pointdata
  std::cout<<"Testing Pointdata : ";
  j=0;
  data = 0.1;
  typedef  MeshType::PointDataContainer PointDataContainer;
  const PointDataContainer* pd = mesh2->GetPointData();
  if(pd)
    {
    MeshType::PointDataContainer::ConstIterator it_pd = pd->Begin();

    while(it_pd != pd->End())
      {
      if((*it_pd)->Index() != j)
        {
        std::cout<<" [FAILED]"<<std::endl;
        std::cout << "Index = " << (*it_pd)->Index() << " v.s " << j << std::endl;
        return EXIT_FAILURE;
        }
      if(std::fabs((*it_pd)->Value()-data)>0.001)
        {
        std::cout<<" [FAILED]"<<std::endl;
        std::cout << "value = " << (*it_pd)->Value() << " v.s " << data << std::endl;
        return EXIT_FAILURE;
        }
      data += float(0.1);
      it_pd++;
      j++;
      }
    }
  else
    {
    std::cout<<"No Point Data" << std::endl;
    std::cout << "[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  std::cout<<" [PASSED]"<<std::endl;


 // Testing celldata
  std::cout<<"Testing Celldata : ";
  j=0;
  data = 0.9;
  typedef  MeshType::CellDataContainer PointDataContainer;
  const PointDataContainer* pc = mesh2->GetCellData();
  if(pc)
    {
    MeshType::CellDataContainer::ConstIterator it_pc = pc->Begin();

    while(it_pc != pc->End())
      {
      if((*it_pc)->Index() != j)
        {
        std::cout<<" [FAILED]"<<std::endl;
        std::cout << "Index = " << (*it_pc)->Index() << " v.s " << j << std::endl;
        return EXIT_FAILURE;
        }
      if(std::fabs((*it_pc)->Value()-data)>0.001)
        {
        std::cout<<" [FAILED]"<<std::endl;
        std::cout << "value = " << (*it_pc)->Value() << " v.s " << data << std::endl;
        return EXIT_FAILURE;
        }
      data -= float(0.2);
      it_pc++;
      j++;
      }
    }
  else
    {
    std::cout<<"No Cell Data" << std::endl;
    std::cout << "[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }

  std::cout<<" [PASSED]"<<std::endl;
  std::cout << " [TEST DONE]" << std::endl;

  return EXIT_SUCCESS;
}
