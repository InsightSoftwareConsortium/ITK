/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientRecursiveGaussianFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/




#include <itkPhysicalImage.h>
#include <itkGradientRecursiveGaussianImageFilter.h>
#include <itkSimpleImageRegionIterator.h>
#include <itkCovariantVector.h>


int main() 
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare gradient type
  typedef itk::CovariantVector<float, myDimension> myGradientType;

  // Declare the types of the images
  typedef itk::PhysicalImage<float, myDimension>           myImageType;
  typedef itk::PhysicalImage<myGradientType, myDimension>  myGradientImageType;

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
  start = myIndexType::ZeroIndex;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  // Declare Iterator types apropriated for each image 
  typedef itk::SimpleImageRegionIterator<myImageType>  myIteratorType;

  typedef itk::SimpleImageRegionIterator<
                                 myGradientImageType>  myOutputIteratorType;

  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the content of Image A
  std::cout << "Input Image initialization " << std::endl;
  it.Begin();
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
  itb.Begin();
  while( !itb.IsAtEnd() ) 
  {
    itb.Set( 100.0 );
    ++itb;
  }

  // Declare the type for the 
  typedef itk::GradientRecursiveGaussianImageFilter<
                                            myImageType,
                                            myGradientImageType,
                                            float       >  myFilterType;
            

  // Create a  Filter                                
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput( inputImage ); 

  
  // Execute the filter
  filter->Update();


  // Get the Smart Pointer to the Filter Output 
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myGradientImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the output image
  myOutputIteratorType itg( outputImage, 
                            outputImage->GetRequestedRegion() );
  
  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  itg.Begin();
  while( !itg.IsAtEnd() ) 
  {
    std::cout << itg.Get() << std::endl;
    ++itg;
  }


  // All objects should be automatically destroyed at this point
  return 0;

}




