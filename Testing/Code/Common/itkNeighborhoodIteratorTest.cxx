/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkNeighborhoodIteratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImage.h"
#include "vnl/vnl_vector.h"
#include "itkNeighborhoodAllocator.h"
#include "itkNeighborhood.h"
#include "itkNeighborhoodIterator.h"
#include "itkRegionNeighborhoodIterator.h"
#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include "itkRandomAccessNeighborhoodIterator.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
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
   size2D[0] = 123;
   size2D[1] = 241;
  
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

  itk::ImageRegionIterator<float, 2> it2D(image2D,
                                          image2D->GetRequestedRegion());
  itk::ImageRegionIterator<float, 3> it3D(image3D,
                                          image3D->GetRequestedRegion());
  itk::ImageRegionIterator<float, 4> itND(imageND,
                                          imageND->GetRequestedRegion());

  println("Initializing some images");
  
  for (it2D = it2D.Begin(); it2D != it2D.End(); ++it2D) *it2D = 1.0f;
  for (it3D = it3D.Begin(); it3D != it3D.End(); ++it3D) *it3D = 1.0f;
  for (itND = itND.Begin(); itND != itND.End(); ++itND) *itND = 1.0f;

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

  itk::RegionNeighborhoodIterator<float, 2,itk::NeighborhoodAllocator<float *>,
    vnl_vector<float> > rni2D(sz2, image2D, image2D->GetRequestedRegion());
  itk::RegionNeighborhoodIterator<float, 3> rni3D(sz3, image3D,
                                            image3D->GetRequestedRegion());
  itk::RegionNeighborhoodIterator<float, 4> rniND(szN, imageND,
                                            imageND->GetRequestedRegion());

  rni2D.Print(std::cout);
  std::cout << std::endl;
  rni2D.Begin().Print(std::cout);
  std::cout << std::endl;
  rni2D.End().Print(std::cout);

  int i=0;
  for (rni2D = rni2D.Begin(); rni2D < rni2D.End(); ++rni2D)
    { ++i; }
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
  itk::RandomAccessNeighborhoodIterator<float, 3,
    itk::NeighborhoodAllocator<float *>,
    vnl_vector<float> > rri3D(sz3, image3D, image3D->GetRequestedRegion());


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
  itk::Index<3> offset = { 12, 11, 2};
  rri3D = rri3D.Begin() + offset;
  rri3D.Print(std::cout);

  (rri3D - offset).Print(std::cout);

  rri3D = offset + rri3D.Begin();
  rri3D.Print(std::cout);

  println("Testing iterator subtraction (distance between iterators)");
  std::cout << (rri3D - rri3D.Begin()) << std::endl;


  println("Testing RegionNonBoundaryNeighborhoodIterator");
  itk::RegionNonBoundaryNeighborhoodIterator<float, 3,
    itk::NeighborhoodAllocator<float *>,
    vnl_vector<float> > rnbi3D(sz3, image3D, image3D->GetRequestedRegion());

  rnbi3D.Print(std::cout);

  println("Testing SmartRegionNeighborhoodIterator");
  itk::SmartRegionNeighborhoodIterator<float, 3,
        itk::NeighborhoodAllocator<float *>,
    vnl_vector<float> > rsbi3D(sz3, image3D,
                               image3D->GetRequestedRegion());
  rsbi3D.Print(std::cout);


  println("Testing RegionBoundaryNeighborhoodIterator");
  itk::RegionBoundaryNeighborhoodIterator<float, 3,
    itk::NeighborhoodAllocator<float *>,
    vnl_vector<float> > rbni3D
    (sz3, image3D, image3D->GetRequestedRegion() );
  rbni3D.Print(std::cout);
  
  
  return 0;
}
