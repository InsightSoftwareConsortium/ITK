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

#include "itkQuadEdgeMesh.h"

#include <iostream>


int itkQuadEdgeMeshPolygonCellTest(int, char* [] )
{


  /**
   * Define a mesh type that stores a PixelType of "int".  Use the defaults for
   * the other template parameters.
   */
  typedef itk::QuadEdgeMesh<int, 3>  MeshType;
  typedef MeshType::CellTraits       CellTraits;
  typedef MeshType::PointIdentifier  PointIdentifier;

  /**
   * Define a few cell types which uses a PixelType of "int".  Again,
   * use the defaults for the other parameters.  Note that a cell's template
   * parameters must match those of the mesh into which it is inserted.
   */
  typedef itk::CellInterface< int, CellTraits >           CellInterfaceType;
  typedef itk::QuadEdgeMeshPolygonCell<CellInterfaceType> PolygonCellType;


  /**
   * Typedef the generic cell type for the mesh.  It is an abstract class,
   * so we can only use information from it, like get its pointer type.
   */
  typedef MeshType::CellType              CellType;
  typedef CellType::CellAutoPointer       CellAutoPointer;

  /**
   * The type of point stored in the mesh. Because mesh was instantiated
   * with defaults (itkDefaultStaticMeshTraits), the point dimension is 3 and
   * the coordinate representation is float.
   */
  typedef MeshType::PointType  PointType;


  /**
   * Create the mesh through its object factory.
   */
  MeshType::Pointer mesh = MeshType::New();
  mesh->DebugOn();

  /**
   * Define the 3d geometric positions for 8 points in a cube.
   */
  MeshType::CoordRepType testPointCoords[8][3]
    = { {0,0,0}, {9,0,0}, {9,0,9}, {0,0,9},
        {0,9,0}, {9,9,0}, {9,9,9}, {0,9,9} };

  /**
   * Add our test points to the mesh.
   * mesh->SetPoint(pointId, point)
   * Note that the constructor for Point is public, and takes an array
   * of coordinates for the point.
   */
  for(int i=0; i < 8; ++i)
    {
    mesh->SetPoint(i, PointType(testPointCoords[i]));
    }

  /**
   * Specify the method used for allocating cells
   */
  mesh->SetCellsAllocationMethod(
     MeshType::CellsAllocatedDynamicallyCellByCell );

  /**
   * Create the test cell. Note that testCell is a generic auto
   * pointer to a cell; in this example it ends up pointing to
   * different types of cells.
   */
  CellAutoPointer testCell;
  PolygonCellType * newcell = new PolygonCellType( 4 );
  testCell.TakeOwnership( newcell ); // polymorphism

  /**
   * List the points that the polygon will use from the mesh.
   */
  PointIdentifier polygon1Points[4] = {0,1,2,3};

  /**
   * Assign the points to the tetrahedron through their identifiers.
   */
  testCell->SetPointIds(polygon1Points);
  if( newcell->GetPointId( 18 ) != PointIdentifier(-1) )
    {
    std::cerr << "Get Point should have failed !" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test MakeCopy" << std::endl;

  CellAutoPointer anotherCell;

  testCell->MakeCopy( anotherCell );

  if( anotherCell->GetNumberOfPoints() != testCell->GetNumberOfPoints() )
    {
    std::cerr << "Make Copy failed !" << std::endl;
    return EXIT_FAILURE;
    }

  /**
   * Add the test cell to the mesh.
   * mesh->SetCell(cellId, cell)
   *
   * Difference itk::Mesh and itk::QEMesh
   * the first take over the cell.
   * The cell address is still valid after a SetCell()
   * the second one create a new cell. The cell is deleted within SetCell()
   */
  mesh->SetCell(0, testCell ); // Transfer ownership to the mesh
  std::cout << "PolygonCell pointer = ";
  std::cout << (void const *)testCell.GetPointer() << std::endl;
  std::cout << "PolygonCell Owner   = " << testCell.IsOwner() << std::endl;

  return EXIT_SUCCESS;
}
