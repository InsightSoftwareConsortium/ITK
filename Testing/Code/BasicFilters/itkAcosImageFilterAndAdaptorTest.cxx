/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAcosImageFilterAndAdaptorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/



#include <itkImage.h>
#include <itkAcosImageFilter.h>
#include <itkAcosImageAdaptor.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkSubtractImageFilter.h>
#include "itkFilterWatcher.h"

int itkAcosImageFilterAndAdaptorTest(int, char* [] ) 
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  // Declare the types of the images
  typedef itk::Image<float, ImageDimension>  InputImageType;
  typedef itk::Image<float, ImageDimension>  OutputImageType;

  
  
  // Declare Iterator types apropriated for each image 
  typedef itk::ImageRegionIteratorWithIndex<
                                  InputImageType>  InputIteratorType;

  typedef itk::ImageRegionIteratorWithIndex<
                                  OutputImageType>  OutputIteratorType;



  // Declare the type of the index to access images
  typedef itk::Index<ImageDimension>         IndexType;

  // Declare the type of the size 
  typedef itk::Size<ImageDimension>          SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<ImageDimension>   RegionType;

  // Create two images
  InputImageType::Pointer inputImage  = InputImageType::New();
  
  // Define their size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetRegions( region );
  inputImage->Allocate();
  // Create one iterator for the Input Image (this is a light object)
  InputIteratorType it( inputImage, inputImage->GetBufferedRegion() );

  // Initialize the content of Image A
  const double pi    = atan( 1.0 ) * 4.0;
  const double value = pi / 6.0;
  std::cout << "Content of the Input " << std::endl;
  it.GoToBegin();
  while( !it.IsAtEnd() ) 
  {
    it.Set( value );
    std::cout << it.Get() << std::endl;
    ++it;
  }

  // Declare the type for the Acos filter
  typedef itk::AcosImageFilter< InputImageType,
                               OutputImageType  >  FilterType;
            

  // Create an Acos Filter                                
  FilterType::Pointer filter = FilterType::New();
  FilterWatcher watch(filter);

  // Connect the input images
  filter->SetInput( inputImage ); 

  // Get the Smart Pointer to the Filter Output 
  OutputImageType::Pointer outputImage = filter->GetOutput();

  
  // Execute the filter
  filter->Update();

  // Create an iterator for going through the image output
  OutputIteratorType ot(outputImage, outputImage->GetRequestedRegion());
  
  //  Check the content of the result image
  std::cout << "Verification of the output " << std::endl;
  const OutputImageType::PixelType epsilon = 1e-6;
  ot.GoToBegin();
  it.GoToBegin();
  while( !ot.IsAtEnd() ) 
  {
    std::cout <<  ot.Get() << " = ";
    std::cout <<  acos( it.Get() )  << std::endl; 
    const InputImageType::PixelType  input  = it.Get();
    const OutputImageType::PixelType output = ot.Get();
    const OutputImageType::PixelType arccosinus  = acos(input);
    if( fabs( arccosinus - output ) > epsilon )
    {
      std::cerr << "Error in itkAcosImageFilterTest " << std::endl;
      std::cerr << " acos( " << input << ") = " << arccosinus << std::endl;
      std::cerr << " differs from " << output;
      std::cerr << " by more than " << epsilon << std::endl;
      return 1;
    }
    ++ot;
    ++it;
  }



  //---------------------------------------
  // This section tests for AcosImageAdaptor
  //---------------------------------------

  typedef itk::AcosImageAdaptor<InputImageType,
                          OutputImageType::PixelType>  AdaptorType;

  AdaptorType::Pointer acosAdaptor = AdaptorType::New();

  acosAdaptor->SetImage( inputImage );

  typedef itk::SubtractImageFilter<
                        OutputImageType,
                        AdaptorType,
                        OutputImageType   > DiffFilterType;

  DiffFilterType::Pointer diffFilter = DiffFilterType::New();

  diffFilter->SetInput1( outputImage );
  diffFilter->SetInput2( acosAdaptor  );

  diffFilter->Update();

  // Get the Smart Pointer to the Diff filter Output
  OutputImageType::Pointer diffImage = diffFilter->GetOutput();

  //  Check the content of the diff image
  std::cout << "Comparing the results with those of an Adaptor" << std::endl;
  std::cout << "Verification of the output " << std::endl;

  // Create an iterator for going through the image output
  OutputIteratorType dt(diffImage, diffImage->GetRequestedRegion());
  
  dt.GoToBegin();
  while( !dt.IsAtEnd() ) 
  {
    std::cout <<  dt.Get() << std::endl;
    const OutputImageType::PixelType diff = dt.Get();
    if( fabs( diff ) > epsilon )
    {
      std::cerr << "Error in itkAcosImageFilterTest " << std::endl;
      std::cerr << "Comparing results with Adaptors" << std::endl;
      std::cerr << " difference = " << diff << std::endl;
      std::cerr << " differs from 0 ";
      std::cerr << " by more than " << epsilon << std::endl;
      return 1;
    }
    ++dt;
  }



  return 0;

}




