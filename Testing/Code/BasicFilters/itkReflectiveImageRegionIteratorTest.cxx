/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReflectiveImageRegionIteratorTest.cxx
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
#include "itkNumericTraits.h"
#include "itkReflectiveImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"


int main()
{
  std::cout << "Creating an image" << std::endl;
  typedef itk::Image<unsigned short,4> ImageType;

  ImageType::Pointer myImage = ImageType::New();
  
  ImageType::SizeType size = {{4,4,4,4}};

  ImageType::IndexType start;
  start = ImageType::IndexType::ZeroIndex;

  ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();

  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  IteratorType nit(myImage, region );

  ImageType::PixelType value = itk::NumericTraits<ImageType::PixelType>::Zero;

  // Store information on the Image
  std::cout << "Storing data in the image ... " << std::endl;
  while( !nit.IsAtEnd() )
    {
    nit.Set( 0 );
    ++nit;
    } 
  
  typedef itk::ReflectiveImageRegionIterator< ImageType > ReflectiveIteratorType;
  ReflectiveIteratorType rit( myImage, region );

  // Verification 
  std::cout << "Verifying the reflective iterator... " << std::endl;;

  value = itk::NumericTraits< ImageType::PixelType >::Zero;
  while( !rit.IsAtEnd() )
    {
    value = rit.Get();
    value++;
    rit.Set(value);
    std::cout << rit.GetIndex() << std::endl;
    ++rit;
    }

  // Each element should be visited 2 * # of dimensions
  int visits = ImageType::ImageDimension * 2;
  int failed = 0;

  nit.GoToBegin();
  while( !nit.IsAtEnd() )
    {
    if (nit.Get() != visits)
      {
      std::cout << nit.GetIndex() << " should not = " << nit.Get() << std::endl;
      failed++;
      }
    ++nit;
    }

  std::cout << std::endl;
  if (!failed)
    {
    std::cout << "      PASSED !" << std::endl;
    }
  else
    {
    std::cout << "      FAILED !" << std::endl;
    }
  return failed;
}



