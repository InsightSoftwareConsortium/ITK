#include "itkImage.h"
#include "itkPoint.h"

int main()
{

  typedef itk::Image< unsigned short, 3 > ImageType;

  ImageType::Pointer image = ImageType::New();

  ImageType::IndexType start;
  ImageType::SizeType  size;

  size[0]  = 200;  // size along X
  size[1]  = 200;  // size along Y
  size[2]  = 200;  // size along Z

  start[0] =   0;  // first index on X
  start[1] =   0;  // first index on Y
  start[2] =   0;  // first index on Z

  ImageType::RegionType region;
  
  image->SetRegions( region );
  image->Allocate();

  image->FillBuffer( 0 );

  double spacing[ 3 ];

  spacing[0] = 0.33; // spacing in millimeters
  spacing[1] = 0.33; // spacing in millimeters
  spacing[2] = 1.20; // spacing in millimeters

  image->SetSpacing( spacing );

  double origin[ 3 ];

  origin[0] = 0.0;  // coordinates of the 
  origin[1] = 0.0;  // first pixel in N-D
  origin[2] = 0.0;

  image->SetOrigin( origin );

  typedef itk::Point<double,3> PointType;
 
  PointType point;

  point[0] = 1.45;
  point[1] = 7.21;
  point[2] = 9.28;

  ImageType::IndexType index;

  image->TransformPhysicalPointToIndex( point, index ); 

  return 0;

}

