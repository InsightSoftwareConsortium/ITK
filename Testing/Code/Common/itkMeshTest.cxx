#include <iostream>
#include <string>

#include "itkMesh.h"
#include "itkTriangleCell.h"

typedef itkMesh<int>          Mesh;
typedef itkTriangleCell<int>  TriangleCell;
typedef Mesh::Point           Point;

int main(void)
{
  double testPointCoords0[Point::PointDimension] = {2,3,4};
  double testPointCoords1[Point::PointDimension] = {3,3,3};
  double testPointCoords2[Point::PointDimension] = {4,4,2};
  unsigned long testPointList[3] = {0,1,2};  
  
  Mesh::Pointer mesh(Mesh::New());  

  mesh->SetPoint(0, Point(testPointCoords0));
  mesh->SetPoint(1, Point(testPointCoords1));
  mesh->SetPoint(2, Point(testPointCoords2));

  TriangleCell::Pointer testCell(TriangleCell::New());

  testCell->SetCellPoints(testPointList);
  
  mesh->SetCell(0, testCell);
  
  return 0;  
}

