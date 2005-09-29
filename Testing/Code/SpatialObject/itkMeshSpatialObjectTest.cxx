/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshSpatialObjectTest.cxx
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


#include <itkDefaultDynamicMeshTraits.h>
#include <itkMesh.h>
#include <itkMeshSpatialObject.h>
#include <itkTetrahedronCell.h>

int itkMeshSpatialObjectTest(int, char * [] ) 
{
  typedef itk::DefaultDynamicMeshTraits< float , 3, 3 > MeshTrait;
  typedef itk::Mesh<float,3,MeshTrait>                  MeshType;
  typedef MeshType::CellTraits                          CellTraits;
  typedef itk::CellInterface< float, CellTraits >       CellInterfaceType;
  typedef itk::TetrahedronCell<CellInterfaceType>       TetraCellType;
  typedef itk::MeshSpatialObject<MeshType>              MeshSpatialObjectType;
  typedef MeshType::PointType                           PointType;
  typedef MeshType::CellType                            CellType;
  typedef CellType::CellAutoPointer                     CellAutoPointer;

  // Create an itkMesh
  MeshType::Pointer mesh = MeshType::New();

  MeshType::CoordRepType testPointCoords[4][3]
    = { {0,0,0}, {9,0,0}, {9,9,0}, {0,0,9} };
  
  unsigned long tetraPoints[4] = {0,1,2,3};
 
  int i;
  for(i=0; i < 4 ; ++i)
    {
    mesh->SetPoint(i, PointType(testPointCoords[i]));
    }

  mesh->SetCellsAllocationMethod( MeshType::CellsAllocatedDynamicallyCellByCell );
  CellAutoPointer testCell1; 
  testCell1.TakeOwnership(  new TetraCellType ); 
  testCell1->SetPointIds(tetraPoints);
  mesh->SetCell(0, testCell1 );
  
  // Create the mesh Spatial Object

  MeshSpatialObjectType::Pointer meshSO = MeshSpatialObjectType::New();
  meshSO->SetMesh(mesh);
    
  std::cout << "Testing GetMesh(): ";

  if(mesh != meshSO->GetMesh())
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  std::cout<<"[PASSED]"<<std::endl;
  
  std::cout << "Testing Bounding Box: ";
  
  if( (meshSO->GetBoundingBox()->GetBounds()[0] != 0)
   || (meshSO->GetBoundingBox()->GetBounds()[1] != 9)
   || (meshSO->GetBoundingBox()->GetBounds()[2] != 0)
   || (meshSO->GetBoundingBox()->GetBounds()[3] != 9)
   || (meshSO->GetBoundingBox()->GetBounds()[4] != 0)
   || (meshSO->GetBoundingBox()->GetBounds()[5] != 9)
   )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  
  std::cout<<"[PASSED]"<<std::endl;


  // Testing is inside
  std::cout << "Testing IsInside: ";

  MeshSpatialObjectType::PointType inside;
  inside[0] = 1;
  inside[1] = 1;
  inside[2] = 1;
  MeshSpatialObjectType::PointType outside;
  outside[0] = 0;
  outside[1] = 3;
  outside[2] = 0;

  if(!meshSO->IsInside(inside) || meshSO->IsInside(outside))
    {
    std::cout<<"[FAILED]"<<std::endl;
    if(!meshSO->IsInside(inside))
      {
      std::cout << inside << " is not inside the mesh!" << std::endl;
      }
    if(meshSO->IsInside(outside))
      {
      std::cout << outside << " is inside the mesh!" << std::endl;
      }
    return EXIT_FAILURE;
    }
  std::cout<<"[PASSED]"<<std::endl;

  // Testing is valueAt
  std::cout << "Testing ValueAt: ";
  double value;
  meshSO->ValueAt(inside,value);
  if(!value)
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }

  meshSO->ValueAt(outside,value);
  if(value)
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }

  std::cout<<"[PASSED]"<<std::endl;


  // Testing IsInside() for triangle mesh
  std::cout << "Testing IsInside() Triangle: ";
  typedef itk::TriangleCell<CellInterfaceType>          TriangleCellType;

  // Create an itkMesh
  MeshType::Pointer meshTriangle = MeshType::New();

  MeshType::CoordRepType testTrianglePointCoords[4][3]
    = { {50,50,64}, {50,100,64}, {100,50,64} , {100,100,64}};
  
  unsigned long trianglePoint1[3] = {0,1,2};
 
  for(i=0; i < 4 ; ++i)
    {
    meshTriangle->SetPoint(i, PointType(testTrianglePointCoords[i]));
    }
 
  unsigned long trianglePoint2[] = {1,2,3};

  meshTriangle->SetCellsAllocationMethod( MeshType::CellsAllocatedDynamicallyCellByCell );
  CellAutoPointer testCell3; 
  testCell3.TakeOwnership(  new TriangleCellType ); 
  testCell3->SetPointIds(trianglePoint1);
  meshTriangle->SetCell(0, testCell3 );
 
  CellAutoPointer testCell4; 
  testCell4.TakeOwnership(  new TriangleCellType ); 
  testCell4->SetPointIds(trianglePoint2);
  meshTriangle->SetCell(1, testCell4 );

  // Create the mesh Spatial Object
  MeshSpatialObjectType::Pointer meshTriangleSO = MeshSpatialObjectType::New();
  meshTriangleSO->SetMesh(meshTriangle);

  itk::Point<double,3> pIn;
  pIn[0] = 60;
  pIn[1] = 60;
  pIn[2] = 64;
  itk::Point<double,3> pOut;
  pOut[0] = 60;
  pOut[1] = 102;
  pOut[2] = 64;
  if(!meshTriangleSO->IsInside(pIn) || meshTriangleSO->IsInside(pOut))
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  std::cout<<"[PASSED]"<<std::endl;

  std::cout<<"[TEST DONE]"<<std::endl;

  return EXIT_SUCCESS;

}
