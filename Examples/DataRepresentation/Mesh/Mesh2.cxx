#include "itkMesh.h"

int main()
{

  typedef itk::Mesh< unsigned short, 3 > MeshType;
  typedef MeshType::PointsContainer      PointsContainer;

  PointsContainer::Pointer points = PointsContainer::New();

  MeshType::PointType p;

  points->InsertElement( 0, p );

  MeshType::Pointer  mesh = MeshType::New();

  mesh->SetPoints( points );

  return 0;

}

