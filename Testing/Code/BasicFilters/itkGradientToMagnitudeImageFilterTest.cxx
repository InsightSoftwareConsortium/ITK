/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientToMagnitudeImageFilterTest.cxx
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
#include <itkGradientRecursiveGaussianImageFilter.h>
#include <itkImageRegionIterator.h>
#include <itkCovariantVector.h>
#include <itkGradientToMagnitudeImageFilter.h>


int itkGradientToMagnitudeImageFilterTest(int, char**) 
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<float, myDimension>           myImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>             myIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>              mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();

  
  // Define their size, and start index
  mySizeType size;
  size[0] = 8;
  size[1] = 8;
  size[2] = 8;

  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  // Declare Iterator type for the input image
  typedef itk::ImageRegionIterator<myImageType>  myIteratorType;

  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the content of Image A
  while( !it.IsAtEnd() ) 
  {
    it.Set( 0.0 );
    ++it;
  }

  size[0] = 4;
  size[1] = 4;
  size[2] = 4;

  start[0] = 2;
  start[1] = 2;
  start[2] = 2;

  // Create one iterator for an internal region
  region.SetSize( size );
  region.SetIndex( start );
  myIteratorType itb( inputImage, region );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() ) 
  {
    itb.Set( 100.0 );
    ++itb;
  }

  // Declare the type for the gradient filter
  typedef itk::GradientRecursiveGaussianImageFilter< 
                                            myImageType >  myFilterType;
            
  typedef myFilterType::OutputImageType myGradientImageType;


  // Create a  Filter                                
  myFilterType::Pointer gradient = myFilterType::New();


  // Connect the input images
  gradient->SetInput( inputImage ); 

  // Select the value of Sigma
  gradient->SetSigma( 2.5 ); 

  
  // Declare the gradient to Magnitude image filter
  typedef itk::GradientToMagnitudeImageFilter< 
                                  myGradientImageType, 
                                  myImageType >         myMagnitudeFilterType;

  // Create the filter
  myMagnitudeFilterType::Pointer magnitude = myMagnitudeFilterType::New();

  magnitude->SetInput( gradient->GetOutput() );

  // Now compute the magnitude of the gradient
  try
    {
    magnitude->Update();
    }
  catch(...)
    {
    std::cerr << "Exception thrown during Update() " << std::endl;
    return EXIT_FAILURE;
    }

  // Get the Smart Pointer to the Filter Output 
  // It is important to do it AFTER the gradient filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myImageType::Pointer outputImage = magnitude->GetOutput();

  // Declare Iterator type for the output image
  typedef itk::ImageRegionIterator<
                                 myImageType>  myOutputIteratorType;

  // Create an iterator for going through the output image
  myOutputIteratorType itg( outputImage, 
                            outputImage->GetBufferedRegion() );
  
  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  itg.GoToBegin();
  while( !itg.IsAtEnd() ) 
  {
    std::cout << itg.Get() << std::endl;
    ++itg;
  }


  // All objects should be automatically destroyed at this point
  std::cout << std::endl << "Test PASSED !! " << std::endl;

  return EXIT_SUCCESS;

}




