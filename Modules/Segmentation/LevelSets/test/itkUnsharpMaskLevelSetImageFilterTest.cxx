/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkUnsharpMaskLevelSetImageFilter.h"
#include <iostream>

const unsigned int HEIGHT = (128);
const unsigned int WIDTH  = (128);

#define RADIUS (std::min(HEIGHT, WIDTH)/4)

// Distance transform function for square
float square(unsigned x, unsigned y)
{
    float X, Y;
    X = itk::Math::abs(x - (float)WIDTH/2.0);
    Y = itk::Math::abs(y - (float)HEIGHT/2.0);
    float dis;
    if (!((X > RADIUS)&&(Y > RADIUS)))
      {
      dis = RADIUS - std::max(X, Y);
      }
    else
      {
      dis = -std::sqrt((X - RADIUS)*(X - RADIUS) +  (Y - RADIUS)*(Y - RADIUS));
      }
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
  std::cout<<"max iteration = "<<(filter->GetMaxFilterIteration())<<"\n";
  std::cout<<"Starting processing.\n";
  filter->Update();
  filter->Print(std::cout);
  std::cout<<"Passed.\n";
  return EXIT_SUCCESS;
}
