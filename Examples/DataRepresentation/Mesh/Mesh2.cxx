#include "itkMesh.h"
#include "itkLineCell.h"

int main()
{

  typedef float                             PixelType;
  typedef itk::Mesh< PixelType, 3 >         MeshType;

  typedef MeshType::CellType                CellType;
  typedef CellType::CellAutoPointer         CellAutoPointer;

  typedef itk::LineCell< CellType >         LineType;

  typedef MeshType::PointsContainer         PointsContainer;
  typedef MeshType::CellsContainer          CellsContainer;

  MeshType::Pointer  mesh = MeshType::New();

  MeshType::PointType p0;
  MeshType::PointType p1;
  MeshType::PointType p2;

  p0[0] = -1.0; p0[1] = 0.0; p0[2] = 0.0;
  p1[0] =  1.0; p1[1] = 0.0; p1[2] = 0.0;
  p2[0] =  1.0; p2[1] = 1.0; p2[2] = 0.0;

  PointsContainer::Pointer points = PointsContainer::New();

  points->InsertElement( 0, p0 );
  points->InsertElement( 1, p1 );
  points->InsertElement( 2, p2 );

  CellAutoPointer line0;
  CellAutoPointer line1;

  line0.TakeOwnership(  new LineType );
  line1.TakeOwnership(  new LineType );

  line0->SetPointId( 0, 0 ); // line between points 0 and 1
  line0->SetPointId( 1, 1 );

  line1->SetPointId( 0, 1 ); // line between points 1 and 2
  line1->SetPointId( 1, 2 );

  CellsContainer::Pointer cells = CellsContainer::New();

  cells->InsertElement( 0, line0.ReleaseOwnership() );
  cells->InsertElement( 1, line1.ReleaseOwnership() );

  mesh->SetPoints( points );
  mesh->SetCells( cells );

  std::cout << "Points = " << mesh->GetNumberOfPoints() << std::endl;
  std::cout << "Cells  = " << mesh->GetNumberOfCells()  << std::endl;

  return 0;

}

