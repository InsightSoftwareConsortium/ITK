/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkShrinkImageTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkScalar.h"
#include "itkShrinkImage.h"

int main()
{
  // typedefs to simplify the syntax
  typedef itk::Image<itk::Scalar<short>, 2>   ShortImage;

  // Test the creation of an image with native type
  ShortImage::Pointer if2 = ShortImage::New();

  // fill in an image
  ShortImage::IndexType  index = {0, 0};
  ShortImage::SizeType   size = {8, 12};
  ShortImage::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  if2->SetLargestPossibleRegion( region );
  if2->SetBufferedRegion( region );
  if2->Allocate();

  itk::ImageRegionIterator<itk::Scalar<short>, 2> iterator(if2, region);

  short i=0;
  for (; !iterator.IsAtEnd(); ++iterator, ++i)
    {
    *iterator = i;
    }
  
  // Create a filter
  itk::ShrinkImage< ShortImage, ShortImage >::Pointer shrink;
  shrink = itk::ShrinkImage< ShortImage, ShortImage >::New();
  shrink->SetInput( if2 );
  shrink->SetShrinkFactor(2);
  shrink->Update();

  //
  // The rest of this code determines whether the shrink code produced
  // the image we expected.
  //
  ShortImage::RegionType requestedRegion;
  requestedRegion = shrink->GetOutput()->GetRequestedRegion();
  
  itk::ImageRegionIterator<itk::Scalar<short>, 2>
    iterator2(shrink->GetOutput(), requestedRegion);

  bool passed = true;
  for (; !iterator2.IsAtEnd(); ++iterator2)
    {
    std::cout << "Pixel " << iterator2.GetIndex() << " = " << *iterator2 << std::endl;
    if ( *iterator2 != (shrink->GetShrinkFactor() * iterator2.GetIndex()[0]
          + region.GetSize()[0]
          *shrink->GetShrinkFactor()*iterator2.GetIndex()[1]))
      {
      passed = false;
      }
    }

  if (passed)
    {
    std::cout << "ShrinkImage test passed." << std::endl;
    exit(EXIT_SUCCESS);
    }
  else
    {
    std::cout << "ShrinkImage test failed." << std::endl;
    exit(EXIT_FAILURE);
    }

}



