#include "itkPointSet.h"

int main()
{

  typedef itk::PointSet< unsigned short, 3 > PointSetType;

  PointSetType::PointType p1;
  PointSetType::PointType p2;
  PointSetType::PointType p3;

  p1[0] = -1.0; p1[1] = 0.0; p1[2] = 0.0; // Point 1 = {-1,0,0 }
  p2[0] =  1.0; p2[1] = 0.0; p2[2] = 0.0; // Point 2 = { 1,0,0 }
  p3[0] =  1.0; p3[1] = 1.0; p3[2] = 0.0; // Point 3 = { 1,1,0 }

  PointSetType::Pointer  pointsSet = PointSetType::New();

  pointsSet->SetPoint( 0, p1 );
  pointsSet->SetPoint( 1, p2 );
  pointsSet->SetPoint( 2, p3 );

  std::cout << pointsSet->GetNumberOfPoints() << std::endl;

  return 0;

}

