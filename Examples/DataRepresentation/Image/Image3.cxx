#include "itkImage.h"

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

  ImageType::IndexType pixelIndex;
 
  pixelIndex[0] = 27;
  pixelIndex[1] = 29;
  pixelIndex[2] = 37;

  image->SetPixel( pixelIndex, 41 );

  ImageType::PixelType value = image->GetPixel( pixelIndex );

  return 0;

}

