/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkNeighborhoodIteratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkImage.h"
#include "itkOffset.h"
#include "vnl/vnl_vector.h"
#include "itkNeighborhoodAllocator.h"
#include "itkNeighborhood.h"
#include "itkNeighborhoodIterator.h"
#include "itkRegionNeighborhoodIterator.h"
#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include "itkRandomAccessNeighborhoodIterator.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkSmartRegionNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include <iostream>

inline void println(char *s) { std::cout << s << std::endl; }

int main()
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
   size2D[0] = 26;
   size2D[1] = 26;
  
  itk::Size<3>  size3D;
   size3D[0] = 26;
   size3D[1] = 26;
   size3D[2] = 3;

  itk::Size<4>  sizeND;
   sizeND[0] = 26;
   sizeND[1] = 26;
   sizeND[2] = 3;
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
  
  for (it2D = it2D.Begin(); it2D != it2D.End(); ++it2D) it2D.Set( 1.0f );
  for (it3D = it3D.Begin(); it3D != it3D.End(); ++it3D) it3D.Set( 1.0f );
  for (itND = itND.Begin(); itND != itND.End(); ++itND) itND.Set( 1.0f );

  // Set up some neighborhood iterators
  println("Setting up some neighborhood iterators");
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

  itk::RegionNeighborhoodIterator<ImageType2D> rni2D(sz2, image2D,
                                 image2D->GetRequestedRegion());
  itk::SmartRegionNeighborhoodIterator<ImageType3D> rni3D(sz3, image3D,
                                            image3D->GetRequestedRegion());
  itk::RegionNeighborhoodIterator<ImageTypeND> rniND(szN, imageND,
                                            imageND->GetRequestedRegion());

  println("Done setting up neighborhood iterators");
  
  rni2D.Print(std::cout);
  std::cout << std::endl;
  rni2D.Begin().Print(std::cout);
  std::cout << std::endl;
  rni2D.End().Print(std::cout);

  println("Testing ComputeInternalIndex");
  rni3D.Print(std::cout);
  for (unsigned int ia_1 = 0; ia_1 < rni3D.Size(); ++ia_1)
    {
      std::cout << ia_1 << "--> " << rni3D.ComputeInternalIndex(ia_1);
    }

  int i=0;
  println("2d forward iteration");
  for (rni2D = rni2D.Begin(); rni2D < rni2D.End(); ++rni2D)
    {
      std::cout << rni2D.GetIndex() << std::endl;
      ++i;
    }
  std::cout << i << std::endl;

  println("2d backward iteration");
  i = 0;
  rni2D = rni2D.End();
  --rni2D;
  for (; rni2D >= rni2D.Begin(); --rni2D)
    {
      std::cout << rni2D.GetIndex() << std::endl;
      ++i;
    }
  std::cout << i << std::endl;

  i=0;
  for (rni3D = rni3D.Begin(); rni3D < rni3D.End(); ++rni3D)
    { ++i; }
  std::cout << i << std::endl;

  i=0;
  for (rniND = rniND.Begin(); rniND < rniND.End(); ++rniND)
    { ++i; }
  std::cout << i << std::endl;


  println("Testing RandomAccessNeighborhoodIterator");
  itk::RandomAccessNeighborhoodIterator<ImageType3D>
    rri3D(sz3, image3D, image3D->GetRequestedRegion());

  println("Testing forward iteration");
  i =0; 
  for (rri3D = rri3D.Begin(); rri3D < rri3D.End(); ++rri3D)
    {++i;}
  std::cout << i << std::endl;

  println("Testing backward iteration");
  i=0;
  rri3D = rri3D.End();
  --rri3D;
  for (; rri3D >= rri3D.Begin(); --rri3D)    {++i;}
  std::cout << i << std::endl;

  println("Testing random access");
  itk::Offset<3> offset;
  offset[0] = 12;
  offset[1] = 11;
  offset[2] = 2;
  rri3D = rri3D.Begin() + offset;
  rri3D.Print(std::cout);

  (rri3D - offset).Print(std::cout);

  rri3D = offset + rri3D.Begin();
  rri3D.Print(std::cout);

  println("Testing iterator subtraction (distance between iterators)");
  std::cout << (rri3D - rri3D.Begin()) << std::endl;
 
  println("Testing RegionNonBoundaryNeighborhoodIterator");
  itk::RegionNonBoundaryNeighborhoodIterator<ImageType3D>
    rnbi3D(sz3, image3D, image3D->GetRequestedRegion());

  rnbi3D.Print(std::cout);
 
  println("Testing SmartRegionNeighborhoodIterator");
  itk::SmartRegionNeighborhoodIterator<ImageType3D> rsbi3D(sz3, image3D,
                               image3D->GetRequestedRegion());
  rsbi3D.Print(std::cout);
  
  println("Testing RegionBoundaryNeighborhoodIterator");
   itk::RegionBoundaryNeighborhoodIterator<ImageType3D> rsbni3D
     (sz3, image3D, image3D->GetRequestedRegion() );
   rsbni3D.Print(std::cout);
  
  return 0;
}
