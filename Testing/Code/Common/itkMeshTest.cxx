#include <iostream>
#include <string>

#include "itkMesh.h"
#include "itkTetrahedronCell.h"

typedef itkMesh<int>          Mesh;
typedef itkTetrahedronCell<int>  TetraCell;
typedef Mesh::Point           Point;

int main(void)
{
  double testPointCoords0[Point::PointDimension] = {2,3,4};
  double testPointCoords1[Point::PointDimension] = {3,3,3};
  double testPointCoords2[Point::PointDimension] = {4,3,2};
  double testPointCoords3[Point::PointDimension] = {5,6,4};
  unsigned long testPointList[4] = {0,1,2,3};
  
  Mesh::Pointer mesh(Mesh::New());  

  mesh->SetPoint(0, Point(testPointCoords0));
  mesh->SetPoint(1, Point(testPointCoords1));
  mesh->SetPoint(2, Point(testPointCoords2));
  mesh->SetPoint(3, Point(testPointCoords3));

  TetraCell::Pointer testCell(TetraCell::New());

  testCell->SetCellPoints(testPointList);
  
  mesh->SetCell(0, testCell);
  
  return 0;  
}

