/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChangeLabelImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif



#include "itkImage.h"
#include "itkRandomImageSource.h"
#include "itkChangeLabelImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"


int itkChangeLabelImageFilterTest(int, char* [] ) 
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  // Declare the types of the images
  typedef itk::Image<unsigned char, ImageDimension>  InputImageType;
  typedef itk::Image<unsigned char, ImageDimension>  OutputImageType;
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

  // limit to a few labels
  InputPixelType upper = 10;
  source->SetMin( itk::NumericTraits<InputPixelType>::Zero );
  source->SetMax( upper );
  source->SetSize( sizeArray );

  // Declare the type for the binary threshold filter
  typedef itk::ChangeLabelImageFilter< InputImageType,
                               OutputImageType  >  FilterType;
            

  // Create a filter                                
  FilterType::Pointer filter = FilterType::New();

  // Eleiminate most labels
  InputPixelType background = 0;
  InputPixelType maxRemainingLabel = 2;
  for (InputPixelType i = maxRemainingLabel; i <= upper; i++) {
    filter->SetChange( i, background );
  }

  filter->Print( std::cout );

  // exercise Get methods


  // Connect the input images
  filter->SetInput( source->GetOutput() ); 

  // Get the Smart Pointer to the Filter Output 
  OutputImageType::Pointer outputImage = filter->GetOutput();
  
  // Execute the filter
  try
    {
    filter->Update();
    filter->SetFunctor(filter->GetFunctor());
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
  
  bool pass = true;
 
  //  Check the content of the result image
  std::cout << "Verification of the output " << std::endl;
  ot.GoToBegin();
  it.GoToBegin();
  while( !ot.IsAtEnd() ) 
  {

    const InputPixelType  input  = it.Get();
    const OutputPixelType output = ot.Get();
    std::cout <<  (double) input  << " " << (double) output << std::endl; 

    if( output > maxRemainingLabel )
      {
        pass = false;
      }
    if ( !pass )
      {
      std::cerr << "Error in itkChangeLaelImageFilterTest " << std::endl;
      std::cerr << " input = " << input;
      std::cerr << " output = " << output;
      std::cerr << std::endl;
      return EXIT_FAILURE;
      }

    ++ot;
    ++it;
  }


  // Test to see if clearing the changemap works
  filter->ClearChangeMap();

  // reexecute the filter
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
  InputIteratorType  ita( source->GetOutput(), source->GetOutput()->GetRequestedRegion() ); 
  OutputIteratorType ota(outputImage, outputImage->GetRequestedRegion());
  

  //  Check the content of the result image
  //  Since the change map is clear, input is expected to be the same as output
  std::cout << "Verification of the output " << std::endl;
  ota.GoToBegin();
  ita.GoToBegin();
  while( !ota.IsAtEnd() ) 
  {

    const InputPixelType  input  = ita.Get();
    const OutputPixelType output = ota.Get();
    std::cout <<  (double) input  << " " << (double) output << std::endl; 

    if( input != output )
      {
        pass = false;
      }
    if ( !pass )
      {
      std::cerr << "Error in itkChangeLaelImageFilterTest " << std::endl;
      std::cerr << " input = " << input;
      std::cerr << " output = " << output;
      std::cerr << std::endl;
      return EXIT_FAILURE;
      }

    ++ota;
    ++ita;
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
