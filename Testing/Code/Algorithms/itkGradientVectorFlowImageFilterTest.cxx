/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientVectorFlowImageFilterTest.cxx
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
#include <itkGradientVectorFlowImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkCovariantVector.h>


int itkGradientVectorFlowImageFilterTest(int, char**) 
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare gradient type
  typedef itk::CovariantVector<float, myDimension> myGradientType;

  // Declare the types of the images
  typedef itk::Image<myGradientType, myDimension>  myGradientImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>             myIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>              mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Create the image
  myGradientImageType::Pointer inputImage  = myGradientImageType::New();

  
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
  typedef itk::ImageRegionIterator<myGradientImageType>  myIteratorType;

  typedef itk::ImageRegionIterator<
                                 myGradientImageType>  myOutputIteratorType;

  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  myGradientType gradientValueA;
  gradientValueA.Fill( 1.5 );

  // Initialize the content of Image A
  while( !it.IsAtEnd() ) 
  {
    it.Set( gradientValueA );
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

  myGradientType gradientValueB;
  gradientValueB.Fill( 2.5 );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() ) 
  {
    itb.Set( gradientValueB );
    ++itb;
  }

  // Declare the type for the 
  typedef itk::GradientVectorFlowImageFilter<
                                      myGradientImageType,
                                      myGradientImageType
                                                   >  myFilterType;
            

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
  itg.GoToBegin();
  while( !itg.IsAtEnd() ) 
  {
    std::cout << itg.Get();
    ++itg;
  }


  // All objects should be automatically destroyed at this point
  return 0;

}




