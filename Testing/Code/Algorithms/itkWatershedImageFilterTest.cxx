/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkWatershedImageFilter.h"

inline void println(char *s) { std::cout << s << std::endl; }

int itkWatershedImageFilterTest(int, char* [] )
{
  typedef itk::Image<float, 2> ImageType2D;
  
  println("Creating some images");
  itk::ImageRegion<2> Region2D;
  
  itk::Size<2>  size2D;
   size2D[0] = 314;
   size2D[1] = 314;
  
  itk::Index<2> orig2D;
   orig2D[0] = 0;
   orig2D[1] = 0;
   
  Region2D.SetSize(size2D);
  Region2D.SetIndex(orig2D);

  ImageType2D::Pointer image2D = ImageType2D::New();
   image2D->SetLargestPossibleRegion(Region2D);
   image2D->SetBufferedRegion(Region2D);
   image2D->SetRequestedRegion(Region2D);
   image2D->Allocate();

 itk::ImageRegionIterator<ImageType2D>
     it2D(image2D, image2D->GetRequestedRegion());  
  println("Initializing an image");
  float q = 0.00f;
  for (; !it2D.IsAtEnd(); ++it2D)
    {
      it2D.Value() = ::sin(q);
      q = q + 0.10f;
    }

  println("Creating the watershed filter");
  itk::WatershedImageFilter<ImageType2D>::Pointer ws_filter =
                  itk::WatershedImageFilter<ImageType2D>::New();
  ws_filter->SetInput(image2D);
  ws_filter->SetThreshold(.05f);
  ws_filter->SetLevel(1.0f);

  println("Executing the filter");
  try {
  ws_filter->Update();
  }
  catch (...) {
  std::cerr << "WatershedImageFilter exception thrown" << std::endl;
  return 1;
  }

  return 0;
}
