/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkNeighborhoodAlgorithmTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImage.h"
#include "itkNeighborhood.h"
#include "itkRegionNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkSmartRegionNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkDerivativeOperator.h"
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
  size3D[0] = 123;
  size3D[1] = 241;
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
  itk::Index<2> orig2Dm;
  orig2Dm[0] = 55;
  orig2Dm[1] = 55;
  
  itk::Index<3> orig3Dm;
  orig3Dm[0] = 55;
  orig3Dm[1] = 55;
  orig3Dm[2] = 5;
  
  itk::Index<4> origNDm;
  origNDm[0] = 55;
  origNDm[1] = 55;
  origNDm[2] = 5;
  origNDm[3] = 2;

  itk::Size<2> sz2;
  sz2[0] = 2;
  sz2[1] = 3;
  
  itk::Size<3> sz3;
  sz3[0] = 2;
  sz3[1] = 3;
  sz3[2] = 1;
  
  itk::Size<4> szN;
  szN[0] = 1;
  szN[1] = 3;
  szN[2] = 1;
  szN[3] = 1;

  itk::RegionNeighborhoodIterator<float, 2> rni2D(sz2, image2D,
                                           image2D->GetRequestedRegion());
  itk::RegionNeighborhoodIterator<float, 3> rni3D(sz3, image3D,
                                           image3D->GetRequestedRegion());
  itk::RegionNeighborhoodIterator<float, 4> rniND(szN, imageND,
                                           imageND->GetRequestedRegion());

  // Test convolution
  println("Testing convolution");
  rni2D.SetLocation(orig2Dm);
  itk::Neighborhood<float, 2> n2 = rni2D.GetNeighborhood();
  n2 = itk::NeighborhoodAlgorithm::Convolve2D(n2, n2, 1);
  n2 = itk::NeighborhoodAlgorithm::Convolve2D(n2, n2, 0);

  rni3D.SetLocation(orig3Dm);
  itk::Neighborhood<float, 3> n3 = rni3D.GetNeighborhood();
  n3 = itk::NeighborhoodAlgorithm::Convolve3D(n3, n3, 1);
  n3 = itk::NeighborhoodAlgorithm::Convolve3D(n3, n3, 0);

  rniND.SetLocation(origNDm);
  itk::Neighborhood<float, 4> nN = rniND.GetNeighborhood();
  nN = itk::NeighborhoodAlgorithm::ConvolveND(nN, nN, 1);
  nN = itk::NeighborhoodAlgorithm::ConvolveND(nN, nN, 0);
  
  // Test inner product
  println("Testing inner product variations");
  itk::DerivativeOperator<float, 2> d2D;
  itk::DerivativeOperator<float, 3> d3D;
  itk::DerivativeOperator<float, 4> dND;

  d2D.SetDirection(0);
  d3D.SetDirection(0);
  dND.SetDirection(0);
  
  d2D.SetOrder(1);
  d3D.SetOrder(1);
  dND.SetOrder(1);

  d2D.CreateDirectional();
  d3D.CreateDirectional();
  dND.CreateDirectional();
  
  itk::SmartRegionNeighborhoodIterator<float, 2>
    rni2Dd(d2D.GetRadius(), image2D, image2D->GetRequestedRegion());
  itk::SmartRegionNeighborhoodIterator<float, 3>
    rni3Dd(d3D.GetRadius(), image3D, image3D->GetRequestedRegion());
  itk::SmartRegionNeighborhoodIterator<float, 4>
    rniNDd(dND.GetRadius(), imageND, imageND->GetRequestedRegion());

  println("A");
  itk::NeighborhoodAlgorithm
    ::IteratorInnerProduct<itk::NeighborhoodIterator<float, 2>,
    itk::Neighborhood<float, 2> > ipi2D;
  ipi2D(rni2Dd, d2D);

  println("B");
  itk::NeighborhoodAlgorithm
    ::InnerProduct<itk::Neighborhood<float, 2>, itk::Neighborhood<float,2> >
    ipn2D;
  n2 = rni2Dd.GetNeighborhood();
  ipn2D(n2, d2D);

  println("C");
  itk::NeighborhoodAlgorithm
    ::IteratorInnerProduct<itk::NeighborhoodIterator<float, 3>,
    itk::Neighborhood<float, 3> > ipi3D;
  ipi3D(rni3Dd, d3D);

  println("D");
  itk::NeighborhoodAlgorithm
    ::InnerProduct<itk::Neighborhood<float, 3>,
    itk::Neighborhood<float, 3> > ipn3D;
  n3 = rni3Dd.GetNeighborhood();
  ipn3D(n3, d3D);

  println("E");
  itk::NeighborhoodAlgorithm
    ::IteratorInnerProduct<itk::NeighborhoodIterator<float, 4>,
    itk::Neighborhood<float, 4> > ipiND;
  ipiND(rniNDd, dND);

  println("F");
  itk::NeighborhoodAlgorithm
    ::InnerProduct<itk::Neighborhood<float, 4>,
    itk::Neighborhood<float, 4> > ipnND;
  nN = rniNDd.GetNeighborhood();
  ipnND(nN, dND);
  
  println("G");
  itk::NeighborhoodAlgorithm
    ::BoundsCheckingIteratorInnerProduct<itk::
    SmartRegionNeighborhoodIterator<float, 4>,
    itk::Neighborhood<float, 4> > ipibND;
  ipibND(rniNDd, dND);

  return 0;
}
