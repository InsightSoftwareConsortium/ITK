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
#include "itkWatershedEquivalenceRelabeler.h"
#include "itkWatershedBoundaryResolver.h"

inline void println(char *s) { std::cout << s << std::endl; }

int itkWatershedImageFilterTest(int, char* [] )
{
  typedef itk::Image<float, 2> ImageType2D;
  typedef itk::Image<unsigned long, 2> LongImageType2D;
  
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

  LongImageType2D::Pointer longimage2D = LongImageType2D::New();
   longimage2D->SetRegions(Region2D);
   longimage2D->Allocate();

  
 itk::ImageRegionIterator<ImageType2D>
     it2D(image2D, image2D->GetRequestedRegion());  
  println("Initializing an image");
  float q = 0.00f;
  for (; !it2D.IsAtEnd(); ++it2D)
    {
      it2D.Value() = ::sin(q);
      q = q + 0.10f;
    }


  println("Testing various associated objects");  
  println("Testing EquivalenceRelabeler");
  itk::EquivalencyTable::Pointer t= itk::EquivalencyTable::New();

  itk::watershed::EquivalenceRelabeler<long, 2>::Pointer eq
    = itk::watershed::EquivalenceRelabeler<long, 2>::New();
  eq->SetInputImage(longimage2D);
  eq->SetEquivalencyTable(t);
  eq->GetEquivalencyTable();
  eq->Update();

  println("Testing WatershedMiniPipelineProgressCommand.  Forcing the execution of the const Execute method which is not normally called.");
  itk::WatershedMiniPipelineProgressCommand::Pointer wmppc
    = itk::WatershedMiniPipelineProgressCommand::New();
  wmppc->SetCount(2);
  wmppc->GetCount();
  wmppc->SetNumberOfFilters(2);
  wmppc->GetNumberOfFilters();
  wmppc->SetFilter(eq);
  wmppc->GetFilter();
  const itk::ProcessObject *constp = eq.GetPointer();
  wmppc->Execute(constp, itk::ProgressEvent());
  wmppc->Execute(eq.GetPointer(), itk::ProgressEvent());
  

  println("Testing watershed::BoundaryResolver");
  itk::watershed::BoundaryResolver<float, 2>::Pointer br
    = itk::watershed::BoundaryResolver<float, 2>::New();
  itk::watershed::Boundary<float, 1>::Pointer boundaryA
    = itk::watershed::Boundary<float, 1>::New();
  itk::watershed::Boundary<float, 1>::Pointer boundaryB
    = itk::watershed::Boundary<float, 1>::New();
  

  
  println("Creating the watershed filter");
  itk::WatershedImageFilter<ImageType2D>::Pointer ws_filter =
                  itk::WatershedImageFilter<ImageType2D>::New();
  ws_filter->SetInput(image2D);
  ws_filter->SetThreshold(.05f);
  ws_filter->GetThreshold();
  ws_filter->SetLevel(1.0f);
  ws_filter->GetLevel();

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
