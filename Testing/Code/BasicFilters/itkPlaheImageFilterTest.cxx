/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaheImageFilterTest.cxx
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


#include "itkImage.h"
#include "itkPlaheImageFilter.h"
#include "itkGaussianImageSource.h"


int itkPlaheImageFilterTest(int, char* [] ) 
{

  // Define the dimension of the images
  const unsigned int myDimension = 2;

  // Define the pixel type of the images
  typedef   float           myPixelType;

  // Declare the types of the images
  typedef itk::Image<myPixelType, myDimension>  myImageType;

  // Declare the type of the index to access images
  typedef myImageType::IndexType          myIndexType;

  // Declare the type of the size 
  typedef myImageType::SizeType           mySizeType;

  // Declare the type of the Region
  typedef myImageType::RegionType         myRegionType;

  // Declare the type for the filter
  typedef itk::PlaheImageFilter< myImageType > myFilterType;
 
  // Declare the type for the image source
  typedef itk::GaussianImageSource< myImageType > mySourceType;
 
  // Declare the pointers to images
  typedef myImageType::Pointer    myImageTypePointer;
  
  // Declare the pointers to the filter
  typedef myFilterType::Pointer   myFilterTypePointer;

  // Declare the pointers to the image source
  typedef mySourceType::Pointer   mySourceTypePointer;

  // Create the image source
  mySourceTypePointer imageSource = mySourceType::New();
  
  // Define their size, and start index
  unsigned long usize[ myDimension ];
  usize[0] = 20;
  usize[1] = 20;

  mySizeType size;
  size[0] = usize[0];
  size[1] = usize[1];

  myIndexType start;
  start[0] = 0;
  start[1] = 0;

  float spacing[ myDimension ];
  spacing[0] = 1.0;
  spacing[1] = 1.0;

  float origin[ myDimension ];
  origin[0] = 1.0;
  origin[1] = 1.0;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  imageSource->SetSize( usize );
  imageSource->SetOrigin( origin );
  imageSource->SetSpacing( spacing );

  // Create the Filter                                
  myFilterTypePointer filter = myFilterType::New();

  // window[] is a neighborhood of a evaluated pixel
  unsigned int pixelWindow[ myDimension ];
  pixelWindow[0] = 9;
  pixelWindow[1] = 9;

  filter->SetWindow( pixelWindow );
  filter->SetAlpha( 0.3 );
  filter->SetBeta(  0.3 );

  // Connect the input images
  filter->SetInput( imageSource->GetOutput() ); 

  // Get the Smart Pointer to the Filter Output 
  myImageTypePointer outputImage = filter->GetOutput();
  
  // Execute the filter
  filter->Update();

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}




