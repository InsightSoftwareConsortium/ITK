/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveGaussianImageFiltersTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Disable warning for long symbol names in this file only
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


#include <itkImage.h>
#include <itkRecursiveGaussianImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>


int itkRecursiveGaussianImageFiltersTest(int, char**) 
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare gradient type
  typedef itk::CovariantVector<float, myDimension> myGradientType;

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
  size[0] = 100;
  size[1] = 100;
  size[2] = 100;

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

  // Declare Iterator types apropriated for each image 
  typedef itk::ImageRegionIteratorWithIndex<myImageType>  myIteratorType;


  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the content of Image A
  std::cout << "Input Image initialization " << std::endl;
  while( !it.IsAtEnd() ) 
  {
    it.Set( 0.0 );
    ++it;
  }

  size[0] = 60;
  size[1] = 60;
  size[2] = 60;

  start[0] = 20;
  start[1] = 20;
  start[2] = 20;

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




  // Declare the type for the  Gaussian  filter
  typedef itk::RecursiveGaussianImageFilter<
                                              myImageType,
                                              myImageType
                                                        >  myGaussianFilterType;
            

  // Create a  Filter                                
  myGaussianFilterType::Pointer filter = myGaussianFilterType::New();


  // Connect the input images
  filter->SetInput( inputImage ); 
  filter->SetDirection( 2 );  // apply along Z
  filter->SetOrder( myGaussianFilterType::ZeroOrder );

  
  // Execute the filter
  std::cout << "Executing Smoothing filter...";
  filter->Update();
  std::cout << " Done !" << std::endl;


  // Create a  Filter                                
  myGaussianFilterType::Pointer filter1 = myGaussianFilterType::New();


  // Connect the input images
  filter1->SetInput( inputImage ); 
  filter1->SetDirection( 2 );  // apply along Z
  filter1->SetOrder( myGaussianFilterType::FirstOrder );

  
  // Execute the filter1
  std::cout << "Executing First Derivative filter...";
  filter1->Update();
  std::cout << " Done !" << std::endl;



  // Create a  Filter                                
  myGaussianFilterType::Pointer filter2 = myGaussianFilterType::New();


  // Connect the input images
  filter2->SetInput( inputImage ); 
  filter2->SetDirection( 2 );  // apply along Z
  filter2->SetOrder( myGaussianFilterType::SecondOrder );
  
  // Execute the filter2
  std::cout << "Executing Second Derivative filter...";
  filter2->Update();
  std::cout << " Done !" << std::endl;



  
  // All objects should be automatically destroyed at this point
  return 0;

}




