#include "itkMesh.h"

int main()
{

  typedef itk::Mesh< unsigned short, 3 > MeshType;
  typedef MeshType::PointsContainer      PointsContainer;

  PointsContainer::Pointer points = PointsContainer::New();

  MeshType::PointType p1;
  MeshType::PointType p2;

  p1[0] = -1.0; p1[1] = 0.0; p1[2] = 0.0;
  p2[0] = -1.0; p2[1] = 0.0; p2[2] = 0.0;

  points->InsertElement( 0, p1 );
  points->InsertElement( 1, p2 );

  MeshType::Pointer  mesh = MeshType::New();

  mesh->SetPoints( points );

  return 0;

}

