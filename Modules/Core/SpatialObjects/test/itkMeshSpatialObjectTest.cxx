/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkDefaultDynamicMeshTraits.h"
#include "itkMeshSpatialObject.h"
#include "itkTetrahedronCell.h"
#include "itkMath.h"

int
itkMeshSpatialObjectTest(int, char *[])
{
  using MeshTrait = itk::DefaultDynamicMeshTraits<float, 3, 3>;
  using MeshType = itk::Mesh<float, 3, MeshTrait>;
  using CellTraits = MeshType::CellTraits;
  using CellInterfaceType = itk::CellInterface<float, CellTraits>;
  using TetraCellType = itk::TetrahedronCell<CellInterfaceType>;
  using MeshSpatialObjectType = itk::MeshSpatialObject<MeshType>;
  using PointType = MeshType::PointType;
  using CellType = MeshType::CellType;
  using CellAutoPointer = CellType::CellAutoPointer;

  // Create an itkMesh
  MeshType::Pointer mesh = MeshType::New();

  MeshType::CoordRepType testPointCoords[4][3] = { { 0, 0, 0 }, { 9, 0, 0 }, { 9, 9, 0 }, { 0, 0, 9 } };

  MeshType::PointIdentifier tetraPoints[4] = { 0, 1, 2, 3 };

  int i;
  for (i = 0; i < 4; ++i)
  {
    mesh->SetPoint(i, PointType(testPointCoords[i]));
  }

  mesh->SetCellsAllocationMethod(itk::MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedDynamicallyCellByCell);
  CellAutoPointer testCell1;
  testCell1.TakeOwnership(new TetraCellType);
  testCell1->SetPointIds(tetraPoints);
  mesh->SetCell(0, testCell1);

  // Create the mesh Spatial Object

  MeshSpatialObjectType::Pointer meshSO = MeshSpatialObjectType::New();
  meshSO->Print(std::cout);

  meshSO->SetMesh(mesh);
  meshSO->Update();

  std::cout << "Testing GetMesh(): ";

  if (mesh != meshSO->GetMesh())
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing Bounding Box: ";

  if ((itk::Math::NotExactlyEquals(meshSO->GetMyBoundingBoxInWorldSpace()->GetBounds()[0], 0)) ||
      (itk::Math::NotExactlyEquals(meshSO->GetMyBoundingBoxInWorldSpace()->GetBounds()[1], 9)) ||
      (itk::Math::NotExactlyEquals(meshSO->GetMyBoundingBoxInWorldSpace()->GetBounds()[2], 0)) ||
      (itk::Math::NotExactlyEquals(meshSO->GetMyBoundingBoxInWorldSpace()->GetBounds()[3], 9)) ||
      (itk::Math::NotExactlyEquals(meshSO->GetMyBoundingBoxInWorldSpace()->GetBounds()[4], 0)) ||
      (itk::Math::NotExactlyEquals(meshSO->GetMyBoundingBoxInWorldSpace()->GetBounds()[5], 9)))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;


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

  if (!meshSO->IsInsideInWorldSpace(inside) || meshSO->IsInsideInWorldSpace(outside))
  {
    std::cout << "[FAILED]" << std::endl;
    if (!meshSO->IsInsideInWorldSpace(inside))
    {
      std::cout << inside << " is not inside the mesh!" << std::endl;
    }
    if (meshSO->IsInsideInWorldSpace(outside))
    {
      std::cout << outside << " is inside the mesh!" << std::endl;
    }
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  // Testing is valueAt
  std::cout << "Testing ValueAt: ";
  double value;
  meshSO->ValueAtInWorldSpace(inside, value);
  if (!value)
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  meshSO->ValueAtInWorldSpace(outside, value);
  if (value)
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;


  // Testing IsInside() for triangle mesh
  std::cout << "Testing IsInside() Triangle: ";
  using TriangleCellType = itk::TriangleCell<CellInterfaceType>;

  // Create an itkMesh
  MeshType::Pointer meshTriangle = MeshType::New();

  MeshType::CoordRepType testTrianglePointCoords[4][3] = {
    { 50, 50, 64 }, { 50, 100, 64 }, { 100, 50, 64 }, { 100, 100, 64 }
  };

  MeshType::PointIdentifier trianglePoint1[3] = { 0, 1, 2 };

  for (i = 0; i < 4; ++i)
  {
    meshTriangle->SetPoint(i, PointType(testTrianglePointCoords[i]));
  }

  MeshType::PointIdentifier trianglePoint2[] = { 1, 2, 3 };

  meshTriangle->SetCellsAllocationMethod(
    itk::MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedDynamicallyCellByCell);
  CellAutoPointer testCell3;
  testCell3.TakeOwnership(new TriangleCellType);
  testCell3->SetPointIds(trianglePoint1);
  meshTriangle->SetCell(0, testCell3);

  CellAutoPointer testCell4;
  testCell4.TakeOwnership(new TriangleCellType);
  testCell4->SetPointIds(trianglePoint2);
  meshTriangle->SetCell(1, testCell4);

  // Create the mesh Spatial Object
  MeshSpatialObjectType::Pointer meshTriangleSO = MeshSpatialObjectType::New();
  meshTriangleSO->SetMesh(meshTriangle);
  meshTriangleSO->Update();

  itk::Point<double, 3> pIn;
  pIn[0] = 60;
  pIn[1] = 60;
  pIn[2] = 64;
  itk::Point<double, 3> pOut;
  pOut[0] = 60;
  pOut[1] = 102;
  pOut[2] = 64;
  if (!meshTriangleSO->IsInsideInWorldSpace(pIn) || meshTriangleSO->IsInsideInWorldSpace(pOut))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  meshSO->Print(std::cout);
  std::cout << "[PASSED]" << std::endl;

  std::cout << "[TEST DONE]" << std::endl;

  return EXIT_SUCCESS;
}
