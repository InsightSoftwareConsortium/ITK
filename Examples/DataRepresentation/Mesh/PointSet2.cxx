#include "itkPointSet.h"

int main()
{

  typedef itk::PointSet< unsigned short, 3 > PointSetType;
  typedef PointSetType::PointsContainer      PointsContainer;

  PointsContainer::Pointer points = PointsContainer::New();

  PointSetType::PointType p0;
  PointSetType::PointType p1;

  p0[0] = -1.0; p0[1] = 0.0; p0[2] = 0.0; // Point 0 = {-1,0,0 }
  p1[0] =  1.0; p1[1] = 0.0; p1[2] = 0.0; // Point 1 = { 1,0,0 }

  points->InsertElement( 0, p0 );
  points->InsertElement( 1, p1 );

  PointSetType::Pointer  pointsSet = PointSetType::New();

  pointsSet->SetPoints( points );

  std::cout << pointsSet->GetNumberOfPoints() << std::endl;

  return 0;

}

