/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkWatershedImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkWatershedImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkRelabelWatershedImageFilter.h"

inline void println(char *s) { std::cout << s << std::endl; }

int main()
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
  for (it2D = it2D.Begin(); it2D != it2D.End(); ++it2D)
    {
      it2D.Value() = ::sin(q);
      q = q + 0.10f;
    }

  println("Creating the watershed filter");
  itk::WatershedImageFilter<ImageType2D, ImageType2D>::Pointer ws_filter =
                  itk::WatershedImageFilter<ImageType2D, ImageType2D>::New();
  ws_filter->SetInput(image2D);
  ws_filter->SetThreshold(.05f);
  ws_filter->SetLevel(1.0f);

  println("Creating the output relabeler");
  itk::RelabelWatershedImageFilter<ImageType2D, ImageType2D>::Pointer
    ws_merger = itk::RelabelWatershedImageFilter<ImageType2D,
    ImageType2D>::New();
  ws_merger->SetLevel(0.5f);
  ws_merger->SetInput(ws_filter->GetBasicOutput());
  
  println("Executing the filter");
  ws_merger->Update();

  return 0;
}
