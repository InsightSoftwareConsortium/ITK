/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShrinkImageTest.cxx
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
#include "itkFileOutputWindow.h"


int itkShrinkImageTest(int, char* [] )
{
  itk::FileOutputWindow::Pointer fow = itk::FileOutputWindow::New();
  fow->SetInstance(fow);

  std::cout << "Shrink an image by (2,3)" << std::endl;
  
  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   SimpleImage;
  SimpleImage::Pointer simpleImage = SimpleImage::New();
  std::cout << "Simple image spacing: " << simpleImage->GetSpacing()[0] << ", "
            << simpleImage->GetSpacing()[1] << std::endl;
  std::cout << std::flush;
  
  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   ShortImage;

  // Test the creation of an image with native type
  ShortImage::Pointer if2 = ShortImage::New();

  // fill in an image
  ShortImage::IndexType  index = {{0, 0}};
  ShortImage::SizeType   size = {{8, 12}};
  ShortImage::RegionType region;
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
  
  // Create a filter, shrink by 2,3
  itk::ShrinkImageFilter< ShortImage, ShortImage >::Pointer shrink;
  shrink = itk::ShrinkImageFilter< ShortImage, ShortImage >::New();
  shrink->SetInput( if2 );
  shrink->SetNumberOfThreads(4);
  
  unsigned int factors[2] = { 2, 3 };
  shrink->SetShrinkFactors(factors);
  shrink->UpdateLargestPossibleRegion();

  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", "
            << if2->GetSpacing()[1] << std::endl;
  std::cout << std::flush;
  std::cout << "Output spacing: " << shrink->GetOutput()->GetSpacing()[0]
            << ", "
            << shrink->GetOutput()->GetSpacing()[1] << std::endl;
  std::cout << std::flush;
  std::cout << "Input Requested region: " << shrink->GetInput()->GetRequestedRegion() << std::endl;
  std::cout << std::flush;
  std::cout << "Output Requested region: " << shrink->GetOutput()->GetRequestedRegion() << std::endl;  
  std::cout << std::flush;
  
  //
  // This code determines whether the shrink code produced
  // the image we expected.
  //
  ShortImage::RegionType requestedRegion;
  requestedRegion = shrink->GetOutput()->GetRequestedRegion();
  
  itk::ImageRegionIterator<ShortImage>
    iterator2(shrink->GetOutput(), requestedRegion);

  bool passed = true;
  std::cout << "Output image" << std::endl;
  std::cout << std::flush;
  for (; !iterator2.IsAtEnd(); ++iterator2)
    {
    std::cout << "Pixel " << iterator2.GetIndex() << " = " << iterator2.Get()
              << std::endl;
    std::cout << std::flush;

    short trueValue = static_cast<short>((shrink->GetShrinkFactors()[0] * iterator2.GetIndex()[0])
              + (region.GetSize()[0]
                * shrink->GetShrinkFactors()[1] * iterator2.GetIndex()[1]));

    if ( iterator2.Get() != trueValue )
      {
      passed = false;
      }
    }

  // Now test shrinking by 2x2
  std::cout << "Shrink the image by (2,2) instead." << std::endl;
  std::cout << std::flush;
  
  factors[1] = 2;
  shrink->SetShrinkFactors(factors);

  // ask for an invalid requested region to force an exception
  std::cout << "Assign an invalid requested region which should throw an exception." << std::endl;
  std::cout << std::flush;
  itk::Index<2> foo;
  itk::ImageRegion<2>::IndexValueType fooindex[] = {100, 100};
  foo.SetIndex(fooindex);
  itk::ImageRegion<2> fooregion;
  fooregion = shrink->GetOutput()->GetRequestedRegion();
  fooregion.SetIndex(foo);
  shrink->GetOutput()->SetRequestedRegion( fooregion );
  
  try
    {
    // this should fail due to a bad requested region
    shrink->Update();
    }
  catch (itk::InvalidRequestedRegionError & e)
    {
    std::cout << e << std::endl;
    std::cout << std::endl << std::endl
              << "Exception caught, updating largest possible region instead."
              << std::endl;
    std::cout << std::flush;
    shrink->ResetPipeline();
    shrink->UpdateLargestPossibleRegion();
    }
  catch (...)
    {
    std::cout << "Exception missed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << std::endl << std::endl;
  std::cout << std::flush;
  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", "
            << if2->GetSpacing()[1] << std::endl;
  std::cout << std::flush;
  std::cout << "Shrink filter: " << shrink;
  std::cout << "Shrink filter output: " << shrink->GetOutput();
  std::cout << "Output spacing: " << std::flush << shrink->GetOutput()->GetSpacing()[0]
            << ", "
            << shrink->GetOutput()->GetSpacing()[1] << std::endl;
  std::cout << std::flush;
  std::cout << "Input Requested region: " << std::flush << shrink->GetInput()->GetRequestedRegion() << std::endl;
  std::cout << std::flush;
  std::cout << "Output Requested region: " << shrink->GetOutput()->GetRequestedRegion() << std::endl;  
  std::cout << std::flush;

  std::cout << shrink << std::endl;
  std::cout << std::flush;
  std::cout << "Input" << std::endl << shrink->GetInput() << std::endl;
  std::cout << std::flush;
  std::cout << "Output" << std::endl << shrink->GetOutput() << std::endl;
  std::cout << std::flush;
  
  requestedRegion = shrink->GetOutput()->GetRequestedRegion();
  iterator2 = itk::ImageRegionIterator<ShortImage>(shrink->GetOutput(), requestedRegion);

  std::cout << "Output image" << std::endl;
  std::cout << std::flush;
  for (; !iterator2.IsAtEnd(); ++iterator2)
    {
    std::cout << "Pixel " << iterator2.GetIndex() << " = " << iterator2.Get()
              << std::endl;
    std::cout << std::flush;

    short trueValue = static_cast<short>((shrink->GetShrinkFactors()[0] * iterator2.GetIndex()[0])
              + (region.GetSize()[0]
                * shrink->GetShrinkFactors()[1] * iterator2.GetIndex()[1]));

    if ( iterator2.Get() != trueValue )
      {
      passed = false;
      }
    }
  
  std::cout << std::endl;
  std::cout << std::flush;
  if (passed)
    {
    std::cout << "Recovered from the exception." << std::endl;
    std::cout << "ShrinkImageFilter test passed." << std::endl;
    std::cout << std::flush;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "ShrinkImageFilter test failed." << std::endl;
    std::cout << std::flush;
    return EXIT_FAILURE;
    }

}
