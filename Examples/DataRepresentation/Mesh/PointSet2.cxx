#include "itkPointSet.h"

int main()
{

  typedef itk::PointSet< unsigned short, 3 > PointSetType;
  typedef PointSetType::PointsContainer      PointsContainer;

  PointsContainer::Pointer points = PointsContainer::New();

  PointSetType::PointType p1;
  PointSetType::PointType p2;

  p1[0] = -1.0; p1[1] = 0.0; p1[2] = 0.0; // Point 1 = {-1,0,0 }
  p2[0] =  1.0; p2[1] = 0.0; p2[2] = 0.0; // Point 2 = { 1,0,0 }

  points->InsertElement( 0, p1 );
  points->InsertElement( 1, p2 );

  PointSetType::Pointer  pointsSet = PointSetType::New();

  pointsSet->SetPoints( points );

  std::cout << pointsSet->GetNumberOfPoints() << std::endl;

  return 0;

}

