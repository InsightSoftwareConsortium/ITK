/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImportImageTest.cxx
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
#include "itkImageRegionIterator.h"
#include "itkShrinkImageFilter.h"
#include "itkImportImageFilter.h"

int itkImportImageTest(int, char* [] )
{
  // Create a C-array to hold an image
  short *rawImage = new short[8*12];
  for (int i=0; i < 8*12; i++)
    {
    rawImage[i] = i;
    }

  // typdefs to simplify the syntax
  typedef itk::ImportImageFilter<short, 2>          ImportImageFilter;
  typedef itk::Image<short, 2>   ShortImage;
    
  // Create an ImportImageFilter filter
  ImportImageFilter::Pointer import;
  import = ImportImageFilter::New();

  itk::ImageRegion<2>         region;
  itk::ImageRegion<2>::IndexType  index = {{0, 0}};
  itk::ImageRegion<2>::SizeType   size = {{8, 12}};

  region.SetSize( size );
  region.SetIndex( index );

  import->SetRegion( region );
  import->SetImportPointer( rawImage, 8*12, true);
  
  // Create another filter
  itk::ShrinkImageFilter<ImportImageFilter::OutputImageType, ShortImage >::Pointer shrink;
  shrink = itk::ShrinkImageFilter<ImportImageFilter::OutputImageType, ShortImage>::New();
  shrink->SetInput( import->GetOutput() );
  shrink->SetShrinkFactors(2);
  shrink->Update();

  //
  // The rest of this code determines whether the shrink code produced
  // the image we expected.
  //
  ShortImage::RegionType requestedRegion;
  requestedRegion = shrink->GetOutput()->GetRequestedRegion();
  
  itk::ImageRegionIterator<ShortImage>
    iterator2(shrink->GetOutput(), requestedRegion);

  bool passed = true;
  for (; !iterator2.IsAtEnd(); ++iterator2)
    {
    std::cout << "Pixel " << iterator2.GetIndex() << " = " << iterator2.Get() << std::endl;
    if ( iterator2.Get() != 
         static_cast<long>( (shrink->GetShrinkFactors()[0] * iterator2.GetIndex()[0])
                          +(region.GetSize()[0]
                          * shrink->GetShrinkFactors()[0] * iterator2.GetIndex()[1])))
      {
      passed = false;
      }
    }

  if (passed)
    {
    std::cout << "ImportImageFilter test passed." << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "ImportImageFilter test failed." << std::endl;
    return EXIT_FAILURE;
    }

}
