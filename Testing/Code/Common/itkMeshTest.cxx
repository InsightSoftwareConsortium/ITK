#include <iostream>
#include <string>

#include "itkMesh.h"
#include "itkTetrahedronCell.h"
#include "itkHexahedronCell.h"

/**
 * Some typedefs to make things easier.
 */

/**
 * Define a mesh type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
typedef itkMesh<int>             Mesh;
typedef Mesh::CellType           CellType;

/**
 * Define a few cell types which uses a PixelType of "int".  Again,
 * use the defaults for the other parameters.  Note that a cell's template
 * parameters must match those of the mesh into which it is inserted.
 */
typedef itkTetrahedronCell<int, CellType>    TetraCell;
typedef itkHexahedronCell<int, CellType>     HexaCell;

/**
 * Typedef the generic cell type for the mesh.  It is an abstract class,
 * so we can only use information from it, like get its pointer type.
 */
typedef Mesh::Cell               Cell;

/**
 * The type of point stored in the mesh.
 */
typedef Mesh::Point              Point;

int main(void)
{
  /**
   * Define the 3d geometric positions for 8 points in a cube.
   */
  double testPointCoords[8][3]
    = { {0,0,0}, {9,0,0}, {9,0,9}, {0,0,9},
        {0,9,0}, {9,9,0}, {9,9,9}, {0,9,9} };
  
  /**
   * List the points that the tetrahedron will use from the mesh.
   */
  unsigned long tetraPoints[4] = {0,1,2,4};
  
  /**
   * List the points that the hexahedron will use from the mesh.
   */
  unsigned long hexaPoints[8] = {0,1,2,3,4,5,6,7};
  
  
  /**
   * Create the mesh through its object factory.
   */
  Mesh::Pointer mesh(Mesh::New());  

  /**
   * Add our test points to the mesh.
   * mesh->SetPoint(pointId, point)
   * Note that the constructor for Point is public, and takes an array
   * of coordinates for the point.
   */
  for(int i=0; i < 8 ; ++i)
    {
    mesh->SetPoint(i, Point(testPointCoords[i]));
    }

  /**
   * Create the test cell.
   */
  Cell::Pointer testCell(TetraCell::New());

  /**
   * Assign the points to the tetrahedron through their identifiers.
   */
  testCell->SetCellPoints(tetraPoints);

  /**
   * Add the test cell to the mesh.
   * mesh->SetCell(cellId, cell)
   */
  mesh->SetCell(0, testCell);
  
  /**
   * Create another test cell.
   */
  testCell = HexaCell::New();

  testCell->SetCellPoints(hexaPoints);

  mesh->SetCell(1, testCell);

  /**
   * Try getting one of the hexahedron's faces.
   */
  testCell = mesh->GetCellBoundaryFeature(
    2,    // Topological dimension of boundary.
    1,    // CellIdentifier.
    0);   // CellFeatureIdentifier

  std::cout << testCell->GetClassName() << std::endl;

  /**
   * Try getting the hexahedron's neighbor through its first edge.
   * This should be the test tetrahedron.
   */
  std::list<Mesh::CellIdentifier>  neighborList;
  mesh->GetBoundaryFeatureNeighbors(
    1,              // Topological dimension of feature.
    1,              // CellIdentifier
    0,              // CellFeatureIdentifier
    &neighborList); // Where to put result.

  std::cout << "Neighbors:" << std::endl;
  for(std::list<Mesh::CellIdentifier>::iterator cell = neighborList.begin() ;
      cell != neighborList.end() ; ++cell)
    {
    std::cout << "Id " << *cell << ": ";
    if(mesh->GetCell(*cell, &testCell))
      {
      std::cout << testCell->GetClassName();
      }
    std::cout << std::endl;
    }
  
  return 0;  
}

