/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkResampleImageTest.cxx
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

#include "itkAffineTransform.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkResampleImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"


enum {NDimensions = 2};

typedef float                  PixelType;
typedef itk::Image<PixelType, NDimensions>     ImageType;
typedef ImageType::IndexType                ImageIndexType;
typedef ImageType::Pointer                  ImagePointerType;
typedef ImageType::RegionType               ImageRegionType;
typedef ImageType::SizeType                 ImageSizeType;
typedef double                  CoordRepType;
typedef itk::AffineTransform<CoordRepType,NDimensions>   AffineTransformType;
typedef itk::LinearInterpolateImageFunction<ImageType,CoordRepType>  InterpolatorType;


int itkResampleImageTest(int, char* [] )
{
  // Create and configure an image
  ImagePointerType image = ImageType::New();
  ImageIndexType  index = {{0,  0}};
  ImageSizeType   size  = {{18, 12}};
  ImageRegionType region;
  region.SetSize ( size );
  region.SetIndex( index );
  image->SetLargestPossibleRegion( region );
  image->SetBufferedRegion( region );
  image->Allocate();

  // Fill image with a ramp
  itk::ImageRegionIteratorWithIndex<ImageType> iter(image, region);
  PixelType value;
  for (; !iter.IsAtEnd(); ++iter) {
    index = iter.GetIndex();
    value = index[0] + index[1];
    iter.Set(value);
  }

  // Create an affine transformation
  AffineTransformType::Pointer aff = AffineTransformType::New();
  aff->Scale(0.5);

  // Create a linear interpolation image function
  InterpolatorType::Pointer interp = InterpolatorType::New();
  interp->SetInputImage(image);
  
  // Create and configure a resampling filter
  itk::ResampleImageFilter< ImageType, ImageType >::Pointer resample;
  resample = itk::ResampleImageFilter< ImageType, ImageType >::New();
  resample->SetInput(image);
  resample->SetSize(size);
  resample->SetTransform(aff);
  resample->SetInterpolator(interp);

  // Run the resampling filter
  resample->Update();


  // Check if desired results were obtained
  bool passed = true;
  ImageType::RegionType region2;
  region2 = resample->GetOutput()->GetRequestedRegion();
  itk::ImageRegionIteratorWithIndex<ImageType>
      iter2(resample->GetOutput(), region2);
  PixelType pixval;
  for (; !iter2.IsAtEnd(); ++iter2) {
    index  = iter2.GetIndex();
    value  = iter2.Get();
    pixval = value;
    if ( static_cast<PixelType>( (index[0] + index[1]) / 2.0 ) != pixval ) {
      std::cout << "Error in resampled image: Pixel " << index
                << " = " << value << std::endl;
      passed = false;
    }
  }

  // Report success or failure
  if (!passed) {
    std::cout << "Resampling test failed" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise other member functions
  resample->Print( std::cout );
  std::cout << "Transform: " << resample->GetTransform() << std::endl;
  std::cout << "Interpolator: " << resample->GetInterpolator() << std::endl;
  std::cout << "Size: " << resample->GetSize() << std::endl;
  std::cout << "DefaultPixelValue: " << resample->GetDefaultPixelValue() << std::endl;

  // Exercise error handling
  
  try
    {
    std::cout << "Setting interpolator to NULL" << std::endl;
    passed = false;
    resample->SetInterpolator( NULL );
    resample->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    passed = true;
    resample->ResetPipeline();
    resample->SetInterpolator( interp );
    }

  if (!passed) {
    std::cout << "Resampling test failed" << std::endl;
    return EXIT_FAILURE;
  }

 std::cout << "Test passed." << std::endl;
 return EXIT_SUCCESS;

}
