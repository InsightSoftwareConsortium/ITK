/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    testMetaMesh.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <stdio.h>
#include <iostream>
#include <ctype.h>
#include <metaMesh.h>
#include <metaScene.h>


bool TestingMetaMesh(MetaMesh* _mesh)
{
  int j;
  // Testing Points
  std::cout << "Testing Points : ";
  typedef MetaMesh::PointListType PointListType;
  PointListType::const_iterator it2 = _mesh->GetPoints().begin();
  for(j=0;j< _mesh->GetPoints().size();j++)
    {
    if( ((*it2)->m_Id != j)
      || ((*it2)->m_X[0] != j)
      || ((*it2)->m_X[1] != j)
      || ((*it2)->m_X[2] != j)
      )
      {
      std::cout <<  (*it2)->m_Id << " : " << (*it2)->m_X[0] 
      << " " << (*it2)->m_X[1] << " " << (*it2)->m_X[2] << std::endl;
      std::cout << "[FAILED]" << std::endl;
      return 1;
      }
    it2++;
    }
  std::cout << "[PASSED]" << std::endl;
      
  // Testing cells
  std::cout << "Testing Cells : ";
  typedef MetaMesh::CellListType CellListType;
  CellListType::const_iterator it3 = _mesh->GetCells(MET_TETRAHEDRON_CELL).begin();
  for(j=0;j< _mesh->GetCells(MET_TETRAHEDRON_CELL).size();j++)
    {
    if( ((*it3)->m_Dim != 4)
      || ((*it3)->m_Id != j)
      )
      {        
      std::cout << "Cell Type = " << (*it3)->m_Dim << " : " << (*it3)->m_Id << " : ";
      std::cout << "[FAILED]" << std::endl;
      return 1;
      }
    
    for(unsigned int k=0;k<(*it3)->m_Dim;k++)
      {
      if((*it3)->m_PointsId[k] != j+k)
        {
        std::cout << (*it3)->m_PointsId[k] << " ";
        std::cout << "[FAILED]" << std::endl;
        return 1;
        }
      }
      it3++;
    }
  it3 = _mesh->GetCells(MET_TRIANGLE_CELL).begin();
  for(j=0;j< _mesh->GetCells(MET_TRIANGLE_CELL).size();j++)
    {
    if( ((*it3)->m_Dim != 3)
      || ((*it3)->m_Id != j)
      )
      {        
      std::cout << "Cell Type = " << (*it3)->m_Dim << " : " << (*it3)->m_Id << " : ";
      std::cout << "[FAILED]" << std::endl;
      return 1;
      }
    for(unsigned int k=0;k<(*it3)->m_Dim;k++)
      {
      if((*it3)->m_PointsId[k] != j+k)
        {
        std::cout << (*it3)->m_PointsId[k] << " ";
        std::cout << "[FAILED]" << std::endl;
        return 1;
        }
      }
    it3++;
    }
  std::cout << "[PASSED]" << std::endl;

  // Testing cell links
  std::cout << "Testing CellLinks : ";
  typedef MetaMesh::CellLinkListType CellLinkListType;
  CellLinkListType::const_iterator it_link = _mesh->GetCellLinks().begin();
  for(j=0;j< _mesh->GetCellLinks().size();j++)
    {
    if((*it_link)->m_Id != j)
      {
      std::cout << "CellLink ID = " << (*it_link)->m_Id << " : " ;
      std::cout << "[FAILED]" << std::endl;
      return 1;
      }
    std::list<int>::const_iterator it_link2 = (*it_link)->m_Links.begin();
    while(it_link2 != (*it_link)->m_Links.end())
      {
      if(*it_link2 != j+1)
        {
        std::cout << "[FAILED]" << std::endl;
        return 1;
        }
      it_link2++;
      }
    it_link++;
    }
  std::cout << "[PASSED]" << std::endl;
      
  // Testing PointData
  std::cout << "Testing PointData : ";
  typedef MetaMesh::PointDataListType PointDataListType;
  PointDataListType::const_iterator it_pd = _mesh->GetPointData().begin();
  for(j=0;j< _mesh->GetPointData().size();j++)
    {
    if(((*it_pd)->m_Id != j) || ((int)(static_cast<MeshData<int>*>(*it_pd)->m_Data) != j))
      {
      std::cout << "PointData ID = " << (*it_pd)->m_Id << " : " << (int)(static_cast<MeshData<int>*>(*it_pd)->m_Data) << std::endl;    
      std::cout << "[FAILED]" << std::endl;
      return 1;
      }
    it_pd++;
    }
  std::cout << "[PASSED]" << std::endl;

  // Testing CellData
  std::cout << "Testing CellData : ";
  typedef MetaMesh::CellDataListType CellDataListType;
  CellDataListType::const_iterator it_cd = _mesh->GetCellData().begin();
  float f = (float)(0.1);
  for(j=0;j< _mesh->GetCellData().size();j++)
    {
    if(((*it_cd)->m_Id != j) || ((float)(static_cast<MeshData<float>*>(*it_cd)->m_Data) != f))
      {
      std::cout << "CellData ID = " << (*it_cd)->m_Id << " : " << (float)(static_cast<MeshData<float>*>(*it_cd)->m_Data) << " : " << f << std::endl;    
      std::cout << "[FAILED]" << std::endl;
      return 1;
      }
    f += (float)0.2;
    it_cd++;
   }
 std::cout << "[PASSED]" << std::endl;
 return 0;
}


/** Main */
int testMetaMesh(int , char * [])
{

  MetaScene myScene = MetaScene(3);
  
  std::cout << "Creating mesh: ";
  MetaMesh* mesh = new MetaMesh(3);
  mesh->ID(0);

  // Add Points
  MeshPoint* pnt;
  int i;
  for(i=0;i<10;i++)
  {
    pnt = new MeshPoint(3);
    pnt->m_X[0]=pnt->m_X[1]=pnt->m_X[2]=static_cast<float>(i);
    pnt->m_Id=i;
    mesh->GetPoints().push_back(pnt);
  }

  // Add Cells
  MeshCell* cell;
  for(i=0;i<6;i++)
  {
    cell = new MeshCell(4); // tetrahedra
    cell->m_Id = i;
    cell->m_PointsId[0]=i;
    cell->m_PointsId[1]=i+1;
    cell->m_PointsId[2]=i+2;
    cell->m_PointsId[3]=i+3;
    mesh->GetCells(MET_TETRAHEDRON_CELL).push_back(cell);
  }

   // Add other type of cells
  for(i=0;i<4;i++)
    {
    cell = new MeshCell(3); // triangle
    cell->m_Id = i;
    cell->m_PointsId[0]=i;
    cell->m_PointsId[1]=i+1;
    cell->m_PointsId[2]=i+2;
    mesh->GetCells(MET_TRIANGLE_CELL).push_back(cell);
    }

  // Add cell links
  for(i=0;i<3;i++)
    {
    MeshCellLink* link = new MeshCellLink();
    link->m_Id = i;
    link->m_Links.push_back(i+1);
    mesh->GetCellLinks().push_back(link);
    }

  // Add point data
  for(i=0;i<5;i++)
    {
    MeshData<int>* pd = new MeshData<int>();
    pd->m_Id = i;
    pd->m_Data = i;
    mesh->GetPointData().push_back(pd);
    }
  
  // Add cell data
  float f = (float)(0.1);
  for(i=0;i<4;i++)
    {
    MeshData<float>* cd = new MeshData<float>();
    cd->m_Id = i;
    cd->m_Data = f;
    f+=(float)(0.2);
    mesh->GetCellData().push_back(cd);
    }
  std::cout << "[PASSED]" << std::endl;

  // Write the mesh
  std::cout << "Writing non binary Mesh : ";
  myScene.AddObject(mesh);
  myScene.BinaryData(false);
  myScene.Write("metamesh.msh");
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Reading non binary Mesh : ";
  // Read the mesh 
  MetaScene myScene2 = MetaScene();
  myScene2.InitializeEssential(3);
  myScene2.Read("metamesh.msh");
  std::cout << "[PASSED]" << std::endl;

  typedef  MetaScene::ObjectListType ListType;
  ListType * list = myScene2.GetObjectList();
  ListType::iterator it = list->begin();
 
  for(i=0;i< list->size();i++)
    {
    if(!strncmp((*it)->ObjectTypeName(),"Mesh",4))
      {
      MetaMesh* mesh2 = dynamic_cast<MetaMesh*>(*it);
      if(TestingMetaMesh(mesh2))
        {
        std::cout << "[FAILED]" << std::endl;
        return 1;
        }
      (mesh2)->PrintInfo();
      }    
    it++;
    }

  // Now testing Binary mesh
  std::cout << "Writing binary Mesh : ";
  myScene.BinaryData(true);
  myScene.Write("metamesh.msh");
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Reading binary Mesh : ";
  // Read the mesh 
  MetaScene myScene3 = MetaScene();
  myScene3.InitializeEssential(3);
  myScene3.Read("metamesh.msh");
  std::cout << "[PASSED]" << std::endl;

  list = myScene3.GetObjectList();
  it = list->begin();
 
  for(i=0;i< list->size();i++)
    {
    if(!strncmp((*it)->ObjectTypeName(),"Mesh",4))
      {
      MetaMesh* mesh2 = dynamic_cast<MetaMesh*>(*it);
      if(TestingMetaMesh(mesh2))
        {
        std::cout << "[FAILED]" << std::endl;
        return 1;
        }
      (mesh2)->PrintInfo();
      }    
    it++;
    }
  std::cout << "[DONE]" << std::endl;
  return 0;
}
