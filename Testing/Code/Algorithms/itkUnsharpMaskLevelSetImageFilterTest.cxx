#include "itkImageRegionIterator.h"
#include "itkUnsharpMaskLevelSetImageFilter.h"
#include <iostream>

const unsigned int HEIGHT = (128);
const unsigned int WIDTH  = (128);

#define RADIUS (vnl_math_min(HEIGHT, WIDTH)/4)

// Distance transform function for square
float square(unsigned x, unsigned y)
{
    float X, Y;
    X = ::fabs(x - (float)WIDTH/2.0);
    Y = ::fabs(y - (float)HEIGHT/2.0);
    float dis;
    if (!((X > RADIUS)&&(Y > RADIUS)))
      dis = RADIUS - vnl_math_max(X, Y);
    else
      dis = -sqrt((X - RADIUS)*(X - RADIUS) +  (Y - RADIUS)*(Y - RADIUS));
    return(dis);
}

// Evaluates a function at each pixel in the itk image
void evaluate_function(itk::Image<float, 2> *im,
                       float (*f)(unsigned int, unsigned int) )
  
{
  itk::Image<float, 2>::IndexType idx;
  for (unsigned int x = 0; x < WIDTH; ++x)
    {
      idx[0] = x;
      for (unsigned int y = 0; y < HEIGHT; ++y)
        {
          idx[1] = y;
          im->SetPixel(idx, f(x, y) );
        }
    }
}

int itkUnsharpMaskLevelSetImageFilterTest(int, char* [] )
{
  typedef itk::Image<float, 2> ImageType;
  
  ImageType::Pointer im_init = ImageType::New();
  
  ImageType::RegionType r;
  ImageType::SizeType   sz = {{HEIGHT, WIDTH}};
  ImageType::IndexType  idx = {{0,0}};
  r.SetSize(sz);
  r.SetIndex(idx);

  im_init->SetLargestPossibleRegion(r);
  im_init->SetBufferedRegion(r);
  im_init->SetRequestedRegion(r);
  im_init->Allocate();

  evaluate_function(im_init, square);
  typedef itk::UnsharpMaskLevelSetImageFilter<ImageType,
    ImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetMaxFilterIteration (99);
  filter->SetNormalProcessUnsharpWeight(1);
  
  filter->SetInput(im_init);
  std::cout<<"Starting processing.\n";
  filter->Update();
  std::cout<<"Passed.\n";
  return 0;
}
