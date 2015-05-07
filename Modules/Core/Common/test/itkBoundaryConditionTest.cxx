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

#include "itkImageRegionIterator.h"
#include "itkConstantBoundaryCondition.h"
#include "itkNeighborhoodIterator.h"
namespace
{
  void println(const char *c) { std::cout << std::endl << c << std::endl; }
}

template <typename TPixel>
void printn(itk::NeighborhoodAllocator<TPixel> &n, const itk::Size<2> &sz)
{
  unsigned int i, j, k;
  k = 0;
  for (j = 0; j < sz[1]; ++j)
    {
      for (i = 0; i < sz[0]; ++i, ++k) std::cout << n[k] << " ";
      std::cout << std::endl;
    }
}

void filln(itk::Image<float, 2> *img)
{
  float i, j;
  i=j=0.0f;

  itk::ImageRegionIterator<itk::Image<float, 2> > it(img, img->GetRequestedRegion());

  while( ! it.IsAtEnd() )
    {
      it.Set(100.0 * j + i);
      ++it;
      i = i +1.0f;
      if ( (unsigned long)i % img->GetRequestedRegion().GetSize()[0] == 0 )
        {
          j = j +1.0f;
          i = 0.0f;
        }
    }
}

int itkBoundaryConditionTest(int, char* [] )
{

 typedef itk::Image<float, 2> ImageType2D;
 typedef itk::Image<float, 3> ImageType3D;
 typedef itk::Image<float, 4> ImageTypeND;

 println("Creating some images");

 // Create some images
 itk::ImageRegion<2> Region2D;
 itk::ImageRegion<3> Region3D;
 itk::ImageRegion<4> RegionND;

  itk::Size<2>  size2D;
   size2D[0] = 30;
   size2D[1] = 15;

  itk::Size<3>  size3D;
   size3D[0] = 100;
   size3D[1] = 100;
   size3D[2] = 10;

  itk::Size<4>  sizeND;
   sizeND[0] = 10;
   sizeND[1] = 10;
   sizeND[2] = 4;
   sizeND[3] = 2;

  itk::Index<2> orig2D;
   orig2D[0] = 0;
   orig2D[1] = 0;

  itk::Index<3> orig3D;
   orig3D[0] = 0;
   orig3D[1] = 0;
   orig3D[2] = 0;

  itk::Index<4> origND;
   origND[0] = 0;
   origND[1] = 0;
   origND[2] = 0;
   origND[3] = 0;

  Region2D.SetSize(size2D);
  Region3D.SetSize(size3D);
  RegionND.SetSize(sizeND);

  Region2D.SetIndex(orig2D);
  Region3D.SetIndex(orig3D);
  RegionND.SetIndex(origND);

  ImageType2D::Pointer image2D = ImageType2D::New();
  ImageType3D::Pointer image3D = ImageType3D::New();
  ImageTypeND::Pointer imageND = ImageTypeND::New();

  image2D->SetLargestPossibleRegion(Region2D);
  image3D->SetLargestPossibleRegion(Region3D);
  imageND->SetLargestPossibleRegion(RegionND);

  image2D->SetBufferedRegion(Region2D);
  image3D->SetBufferedRegion(Region3D);
  imageND->SetBufferedRegion(RegionND);

  image2D->SetRequestedRegion(Region2D);
  image3D->SetRequestedRegion(Region3D);
  imageND->SetRequestedRegion(RegionND);

  image2D->Allocate();
  image3D->Allocate();
  imageND->Allocate();

  itk::ImageRegionIterator<ImageType2D> it2D(image2D,
                                          image2D->GetRequestedRegion());
  itk::ImageRegionIterator<ImageType3D> it3D(image3D,
                                          image3D->GetRequestedRegion());
  itk::ImageRegionIterator<ImageTypeND> itND(imageND,
                                          imageND->GetRequestedRegion());

  println("Initializing some images");

  //  for (; !it2D.IsAtEnd(); ++it2D) *it2D = 1.0f;
  filln(image2D);
  for (; !it3D.IsAtEnd(); ++it3D) it3D.Set(1.0f);
  for (; !itND.IsAtEnd(); ++itND) itND.Set(1.0f);

  println("Initializing smart neighborhood iterators");
  itk::Size<2> sz2;
   sz2[0] = 2;
   sz2[1] = 1;

  itk::Size<3> sz3;
   sz3[0] = 2;
   sz3[1] = 3;
   sz3[2] = 1;

  itk::Size<4> szN;
   szN[0] = 1;
   szN[1] = 3;
   szN[2] = 1;
   szN[3] = 1;

   typedef itk::NeighborhoodIterator<ImageType2D,
     itk::ConstantBoundaryCondition<ImageType2D> > SmartIteratorType;

   SmartIteratorType it2d(sz2, image2D, image2D->GetRequestedRegion());

   itk::ConstantBoundaryCondition<ImageType2D> cbc;
   cbc.SetConstant(itk::NumericTraits<float>::ZeroValue());
   it2d.OverrideBoundaryCondition(&cbc);

   SmartIteratorType::NeighborhoodType tempN;
   SmartIteratorType::NeighborhoodType temp2N;
   temp2N = it2d.GetNeighborhood(); // initialize

   it2d.GoToEnd();
   --it2d;
   tempN = it2d.GetNeighborhood();


   printn(tempN.GetBufferReference(), tempN.GetSize());


   std::cout << " ________________________________________ " << std::endl;

   itk::ZeroFluxNeumannBoundaryCondition<ImageType2D> neumann;
   for (int yak = 0; yak < 2; ++yak)
     {
       for (it2d.GoToBegin(); !it2d.IsAtEnd(); ++it2d)
         {
           for (unsigned int ii = 0; ii < temp2N.Size(); ++ii)
             {
               temp2N[ii] = it2d.GetPixel(ii);
             }
         }

       it2d.OverrideBoundaryCondition(&neumann);
     }

  return EXIT_SUCCESS;
}
