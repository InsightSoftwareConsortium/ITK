/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBToVectorAdaptImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
/**
 *  
 *  This program illustrates the AdaptImageFilter
 *
 *  The example shows how an Accessor can be used to 
 *  convert an RGBPixel image to an image that has
 *  vector pixel type.
 *
 *  This allows to access an RGB image a an image of vectors.
 *
 */


#include <itkAdaptImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkRGBPixel.h>
#include <itkRGBToVectorPixelAccessor.h>

#include <vnl/vnl_sample.h>


//-------------------------
//
//   Main code
//
//-------------------------
int itkRGBToVectorAdaptImageFilterTest(int, char* [] ) {


  //-------------------------------------
  //     Typedefs for convenience
  //-------------------------------------
  typedef itk::RGBPixel< float >                  RGBPixelType;
  typedef itk::Image< RGBPixelType,   2 >         RGBImageType;

  typedef itk::ImageRegionIteratorWithIndex< RGBImageType >  myRGBIteratorType;

  typedef itk::Accessor::RGBToVectorPixelAccessor<float>    AccessorType;

  typedef AccessorType::ExternalType              VectorPixelType;

  typedef itk::Image< VectorPixelType,   2 >      myImageType;

  typedef itk::ImageRegionIteratorWithIndex< myImageType >  myVectorIteratorType;



  RGBImageType::SizeType size;
  size[0] = 2;
  size[1] = 2;

  RGBImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  RGBImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize(  size  );

  RGBImageType::Pointer myImage = RGBImageType::New();


  myImage->SetRegions( region );
  myImage->Allocate();
  
  myRGBIteratorType  it1( myImage, myImage->GetRequestedRegion() );
  
  // Value to initialize the pixels
  RGBImageType::PixelType color;
  
  // Initializing all the pixel in the image
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
    {
    color.Set( (float) vnl_sample_uniform(0.0, 1.0),
               (float) vnl_sample_uniform(0.0, 1.0),
               (float) vnl_sample_uniform(0.0, 1.0) );
    it1.Set(color);
    ++it1;
    }

  // Reading the values to verify the image content
  std::cout << "--- Initial image --- " << std::endl;
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
    {
    const RGBImageType::PixelType c( it1.Get() );
    std::cout << c.GetRed()   << "  ";
    std::cout << c.GetGreen() << "  ";
    std::cout << c.GetBlue()  << std::endl;
    ++it1;
    }


  bool passed = true;

  // Convert to a Vector image
  typedef itk::AdaptImageFilter< RGBImageType, 
                                 myImageType, 
                                 AccessorType   >  AdaptFilterType;

  AdaptFilterType::Pointer  adaptImageToVector = AdaptFilterType::New();

  adaptImageToVector->SetInput(myImage);
  adaptImageToVector->UpdateLargestPossibleRegion();
 
  myVectorIteratorType  it( 
            adaptImageToVector->GetOutput(), 
            adaptImageToVector->GetOutput()->GetRequestedRegion() );

  std::cout << "--- Read Vector values --- " << std::endl;

  it.GoToBegin();
  it1.GoToBegin();
  while( !it.IsAtEnd() )
  {
  std::cout << it.Get()   << std::endl;
  VectorPixelType v =   it.Get();
  RGBPixelType    c =  it1.Get();
  
  if ( v[0] != c.GetRed()   ||
       v[1] != c.GetGreen() ||
       v[2] != c.GetBlue()     )
    {
    std::cerr << "Vector pixel = " << v << std::endl;
    std::cerr << "does not match " << std::endl; 
    std::cerr << "RGB    pixel = " << c << std::endl;
    passed = false;
    break;
    }
    
  ++it;
  ++it1;
  }

  std::cout << std::endl;
  if (passed)
    {
    std::cout << "AdaptImageFilterTest passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "AdaptImageFilterTest passed" << std::endl;
    return EXIT_FAILURE;
    }
}



