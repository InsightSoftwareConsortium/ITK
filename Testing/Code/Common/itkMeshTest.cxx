#include <iostream>
#include <string>

#include "itkMesh.h"
#include "itkTetrahedronCell.h"

/**
 * Some typedefs to make things easier.
 */

/**
 * Define a mesh type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
typedef itkMesh<int>             Mesh;

/**
 * Define a tetrahedron cell type which uses a PixelType of "int".  Again,'
 * use the defaults for the other parameters.  Note that a cell's template
 * parameters must match those of the mesh into which it is inserted.
 */
typedef itkTetrahedronCell<int>  TetraCell;

/**
 * The type of point stored in the mesh.
 */
typedef Mesh::Point              Point;

int main(void)
{
  /**
   * Define the 3d geometric positions for 4 points.
   * I just made these up, so there is no significance to relative position
   * of the points in this example.
   */
  double testPointCoords0[3] = {2,3,4};
  double testPointCoords1[3] = {3,3,3};
  double testPointCoords2[3] = {4,3,2};
  double testPointCoords3[3] = {5,6,4};
  
  /**
   * List the points that the tetrahedron will use from the mesh.
   */
  unsigned long testPointList[4] = {0,1,2,3};
  
  /**
   * Create the mesh through its object factory.
   */
  Mesh::Pointer mesh(Mesh::New());  

  /**
   * Add our 4 points to the mesh.
   * mesh->SetPoint(pointId, point)
   * Note that the constructor for Point is public, and takes an array
   * of coordinates for the point.
   */
  mesh->SetPoint(0, Point(testPointCoords0));
  mesh->SetPoint(1, Point(testPointCoords1));
  mesh->SetPoint(2, Point(testPointCoords2));
  mesh->SetPoint(3, Point(testPointCoords3));

  /**
   * Create the test cell.
   */
  TetraCell::Pointer testCell(TetraCell::New());

  /**
   * Assign the points to the tetrahedron through their identifiers.
   */
  testCell->SetCellPoints(testPointList);

  /**
   * Add the test cell to the mesh.
   * mesh->SetCell(cellId, cell)
   */
  mesh->SetCell(0, testCell);
  
  return 0;  
}

