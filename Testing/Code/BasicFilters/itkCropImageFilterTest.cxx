/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCropImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>
#include "itkImage.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkCropImageFilter.h"
#include "itkFileOutputWindow.h"
#include "itkStreamingImageFilter.h"

int itkCropImageFilterTest(int, char**)
{
  itk::FileOutputWindow::Pointer fow = itk::FileOutputWindow::New();
  fow->SetInstance(fow);

  int nextVal;

  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   SimpleImage;
  SimpleImage::Pointer simpleImage = SimpleImage::New();
  std::cout << "Simple image spacing: " << simpleImage->GetSpacing()[0] << ", "
            << simpleImage->GetSpacing()[1] << std::endl;
  
  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   ShortImage;
  
  // Test the creation of an image with native type
  ShortImage::Pointer if2 = ShortImage::New();
  
  // fill in an image
  ShortImage::IndexType  index = {{0, 0}};
  ShortImage::SizeType   size = {{8, 12}};
  ShortImage::RegionType region;
  int row, column;
  region.SetSize( size );
  region.SetIndex( index );
  if2->SetLargestPossibleRegion( region );
  if2->SetBufferedRegion( region );
  if2->Allocate();
  
  itk::ImageRegionIterator<ShortImage> iterator(if2, region);
  
  short i=0;
  for (; !iterator.IsAtEnd(); ++iterator, ++i)
    {
      iterator.Set( i );
    }

  std::cout << "Input Image: " << if2 << std::endl;

  // Create a filter
  itk::CropImageFilter< ShortImage, ShortImage >::Pointer extract;
  extract = itk::CropImageFilter< ShortImage, ShortImage >::New();
  extract->SetInput( if2 );
  
  // fill in an image
  ShortImage::SizeType   extractSize = {{8, 12}};

  extract->SetUpperBoundaryCropSize(extractSize);
  extract->SetLowerBoundaryCropSize(extractSize);
  extract->UpdateLargestPossibleRegion();

  std::cout << extract << std::endl;
  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", "
            << if2->GetSpacing()[1] << std::endl;
  std::cout << "Output spacing: " << extract->GetOutput()->GetSpacing()[0]
            << ", "
            << extract->GetOutput()->GetSpacing()[1] << std::endl;
  
  
  ShortImage::RegionType requestedRegion;
  bool passed;
  
  extractSize[0] = 1; extractSize[1] = 1;
  extract->SetUpperBoundaryCropSize(extractSize);
  extract->SetLowerBoundaryCropSize(extractSize);
  extract->UpdateLargestPossibleRegion();
  requestedRegion = extract->GetOutput()->GetRequestedRegion();

  if (extract->GetOutput()->GetLargestPossibleRegion().GetSize()[0] != 6
      || extract->GetOutput()->GetLargestPossibleRegion().GetSize()[0] != 10)
    {
      return EXIT_FAILURE;
    }

  if (extract->GetOutput()->GetLargestPossibleRegion().GetIndex()[0] != 1
      || extract->GetOutput()->GetLargestPossibleRegion().GetIndex()[0] != 1)
    {
      return EXIT_FAILURE;
    }
    
  return EXIT_SUCCESS;
}
