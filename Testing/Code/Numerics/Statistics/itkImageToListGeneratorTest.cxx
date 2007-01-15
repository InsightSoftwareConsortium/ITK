/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToListGeneratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// The example tests the class itk::Statistics::ImageToListGenerator.
// The class is capable of generating an itk::ListSample from an image
// confined to a mask (if specified). This test exercises that.

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImageToListGenerator.h"
#include "itkImageRegionIteratorWithIndex.h"

typedef itk::Image< unsigned int , 2 > ImageType;
typedef itk::Image< unsigned char, 2 > MaskImageType;

//------------------------------------------------------------------------
// Creates a 10 x 10 image of unsigned chars with pixel at location 
// (x,y) being yx. ie Pixel at (6,4) = 46.
//
static ImageType::Pointer CreateImage()
{
  ImageType::Pointer image = ImageType::New();
  ImageType::IndexType start = {0,0};
  ImageType::SizeType  size = {10,10};
  ImageType::RegionType region( start, size );
  image->SetRegions( region );
  image->Allocate();
  typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;
  IteratorType it( image, region );
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    it.Set( it.GetIndex()[1] * 10 + it.GetIndex()[0]);
    ++it; 
    }
  return image;
}

//------------------------------------------------------------------------
// Creates a 10 x 10 image of unsigned chars with pixel from (2,3) - (8,5) as 
// 255 and rest as 0 
static MaskImageType::Pointer CreateMaskImage()
{
  MaskImageType::Pointer image = MaskImageType::New();
  MaskImageType::IndexType start = {0,0};
  MaskImageType::SizeType  size = {10, 10};
  MaskImageType::RegionType region( start, size );
  image->SetRegions( region );
  image->Allocate();
  image->FillBuffer(0);
  MaskImageType::IndexType startMask = {2,3};
  MaskImageType::SizeType sizeMask = {7, 3};
  MaskImageType::RegionType regionMask( startMask, sizeMask);
  typedef itk::ImageRegionIteratorWithIndex< MaskImageType > IteratorType;
  IteratorType it( image, regionMask );
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    it.Set((unsigned char)255);    
    ++it; 
    }
  return image;
}

int itkImageToListGeneratorTest(int, char* [] ) 
{
  ImageType::Pointer     image     = CreateImage();
  MaskImageType::Pointer maskImage = CreateMaskImage();
  
  // Generate a list sampel from "image" confined to the mask, "maskImage".
  typedef itk::Statistics::ImageToListGenerator< 
    ImageType, MaskImageType > ImageToListGeneratorType;
  ImageToListGeneratorType::Pointer listGenerator 
                              = ImageToListGeneratorType::New();
  listGenerator->SetInput( image );
  listGenerator->SetMaskImage( maskImage );
  listGenerator->SetMaskValue( 255 );
  listGenerator->Update();

  typedef ImageToListGeneratorType::ListSampleType ListSampleType;
  ListSampleType * list = listGenerator->GetListSample();

  // Check the sum of the pixels in the list sample. This should
  // be 945
  ListSampleType::Iterator lit = list->Begin();
  unsigned int sum = 0;
  while (lit != list->End())
    {
    sum += lit.GetMeasurementVector()[0];
    ++lit;
    }

  if (sum != 945) 
    {
    std::cerr << "[FAILED]" << std::endl;
    std::cerr << "  Sum of pixels in the list sample (masked) is : " << sum
              << " but should be 945.";
    return EXIT_FAILURE;
    }
  
  std::cerr << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}

