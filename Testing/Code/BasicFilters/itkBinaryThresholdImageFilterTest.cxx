/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/



#include "itkImage.h"
#include "itkRandomImageSource.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"


int itkBinaryThresholdImageFilterTest(int, char* [] ) 
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  // Declare the types of the images
  typedef itk::Image<unsigned char, ImageDimension>  InputImageType;
  typedef itk::Image<float, ImageDimension>  OutputImageType;
  typedef InputImageType::PixelType InputPixelType;
  typedef OutputImageType::PixelType OutputPixelType;

  // Declare iterator type
  typedef itk::ImageRegionIteratorWithIndex<
                                  InputImageType>  InputIteratorType;

  typedef itk::ImageRegionIteratorWithIndex<
                                  OutputImageType>  OutputIteratorType;

  // Use a random image source as input
  typedef itk::RandomImageSource<InputImageType> SourceType;
  SourceType::Pointer source = SourceType::New();

  unsigned long sizeArray[ImageDimension] = { 3,3,3 };

  source->SetMin( itk::NumericTraits<InputPixelType>::Zero );
  source->SetMax( itk::NumericTraits<InputPixelType>::max() );
  source->SetSize( sizeArray );

  // Declare the type for the binary threshold filter
  typedef itk::BinaryThresholdImageFilter< InputImageType,
                               OutputImageType  >  FilterType;
            

  // Create a filter                                
  FilterType::Pointer filter = FilterType::New();

  // Setup ivars
  InputPixelType lower = 64;
  InputPixelType upper = 128;
  filter->SetUpperThreshold( upper );
  filter->SetLowerThreshold( lower );

  OutputPixelType inside = -0.5;
  OutputPixelType outside = 0.5;
  filter->SetInsideValue( inside );
  filter->SetOutsideValue( outside );

  filter->Print( std::cout );

  // exercise Get methods
  std::cout << "OutsideValue: " << filter->GetOutsideValue() << std::endl;
  std::cout << "InsideValue: " << filter->GetInsideValue() << std::endl;
  std::cout << "UpperThreshold: " << filter->GetUpperThreshold() << std::endl;
  std::cout << "LowerThreshold: " << filter->GetLowerThreshold() << std::endl;

  // Connect the input images
  filter->SetInput( source->GetOutput() ); 

  // Get the Smart Pointer to the Filter Output 
  OutputImageType::Pointer outputImage = filter->GetOutput();
  
  // Execute the filter
  try
    {
    filter->Update();
    }
  catch(...)
    {
    std::cerr << "Caught an unexpected exception. " << std::endl;
    std::cerr << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  // Create an iterator for going through the image output
  InputIteratorType  it( source->GetOutput(), source->GetOutput()->GetRequestedRegion() ); 
  OutputIteratorType ot(outputImage, outputImage->GetRequestedRegion());
  
  //  Check the content of the result image
  std::cout << "Verification of the output " << std::endl;
  ot.GoToBegin();
  it.GoToBegin();
  while( !ot.IsAtEnd() ) 
  {

    const InputPixelType  input  = it.Get();
    const OutputPixelType output = ot.Get();
    std::cout <<  (double) input  << " " << (double) output << std::endl; 

    bool pass = true;
    if( lower <= input && input <= upper )
      {
      if ( output != inside )
        {
        pass = false;
        }
      }
    else if ( output != outside )
      {
      pass = false;
      }

    if ( !pass )
      {
      std::cerr << "Error in itkBinaryThresholdImageFilterTest " << std::endl;
      std::cerr << " lower = " << (double)lower;
      std::cerr << " upper = " << (double)upper;
      std::cerr << " inside = " << (double)inside;
      std::cerr << " outside = " << (double) outside;
      std::cerr << std::endl;
      std::cerr << " input = " << (double) input;
      std::cerr << " output = " << (double) output;
      std::cerr << std::endl;
      return EXIT_FAILURE;
      }

    ++ot;
    ++it;
  }


  // Deliberately cause an exception by setting lower threshold to be
  // greater than the upper threshold
  filter->SetUpperThreshold( lower );
  filter->SetLowerThreshold( upper );
  bool pass = false;
  try
    {
    filter->Update();
    }
  catch(itk::ExceptionObject &err)
    {
    pass = true;
    std::cout << "Caught an expected exception. " << std::endl;
    std::cout << err << std::endl;
    }

  if ( pass )
    {
    std::cout << "Test passsed. " << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }
}




