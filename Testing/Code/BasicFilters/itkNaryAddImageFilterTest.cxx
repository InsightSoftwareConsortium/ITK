/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNaryAddImageFilterTest.cxx
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

#include <itkImage.h>
#include <itkNaryAddImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <iostream>



// Define the dimension of the images
const unsigned int myDimension = 3;

// Declare the types of the images
typedef itk::Image<float, myDimension>  myInputImageType;
typedef itk::Image<float, myDimension>  myOutputImageType;

// Declare the type of the index to access images
typedef itk::Index<myDimension>         myIndexType;

// Declare the type of the size 
typedef itk::Size<myDimension>          mySizeType;

// Declare the type of the Region
typedef itk::ImageRegion<myDimension>        myRegionType;

// Declare the type of the Region
typedef itk::ImageRegionIteratorWithIndex<myInputImageType>  myImageIteratorType;

// Declare the type for the ADD filter
typedef itk::NaryAddImageFilter<
                              myInputImageType,
                              myOutputImageType  >  myFilterType;
 



// Function for image initialization
void InitializeImage( myInputImageType * image, double value   )
{

  myInputImageType::Pointer inputImage( image );

  // Define their size, and start index
  mySizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );
  
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  myImageIteratorType it( inputImage, 
                     inputImage->GetRequestedRegion() );
  
  it.GoToBegin();
  while( !it.IsAtEnd() ) 
    {
    it.Set( value );
    ++it;
    }


}



// Function for image printing
void PrintImage( myInputImageType * image, const char * text)
{
  // Create an iterator for going through the image
  myImageIteratorType it( image, 
                          image->GetRequestedRegion() );
  
  it.GoToBegin();
  //  Print the content of the image
  std::cout << text << std::endl;
  while( !it.IsAtEnd() ) 
  {
    std::cout << it.Get() << std::endl;
    ++it;
  }

}






int itkNaryAddImageFilterTest(int, char* [] ) 
{

  // Create two images
  myInputImageType::Pointer inputImageA  = myInputImageType::New();
  myInputImageType::Pointer inputImageB  = myInputImageType::New();
  myInputImageType::Pointer inputImageC  = myInputImageType::New();
  
  InitializeImage( inputImageA, 12 );
  InitializeImage( inputImageB, 17 );
  InitializeImage( inputImageC, -4 );
  
  PrintImage( inputImageA, "Input image A" ); 
  PrintImage( inputImageB, "Input image B" ); 
  PrintImage( inputImageC, "Input image C" ); 

  // Create an ADD Filter                                
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput( 0, inputImageA ); 
  filter->SetInput( 1, inputImageB );
  filter->SetInput( 2, inputImageC );

  // Get the Smart Pointer to the Filter Output 
  myOutputImageType::Pointer outputImage = filter->GetOutput();

  
  // Execute the filter
  filter->Update();
  filter->SetFunctor(filter->GetFunctor());

  PrintImage( outputImage, "Resulting image" );

  
  // Test the validity of the output
  myRegionType region = inputImageA->GetRequestedRegion();
  
  typedef itk::ImageRegionConstIterator<myInputImageType>  IteratorIn;
  typedef itk::ImageRegionConstIterator<myOutputImageType> IteratorOut;
  
  IteratorIn  iterA( inputImageA, region );
  IteratorIn  iterB( inputImageB, region );
  IteratorIn  iterC( inputImageC, region );
  IteratorOut iterO( outputImage, region );

  const double epsilon = 1e-9;
  unsigned int failures = 0;
  while ( !iterO.IsAtEnd() )
    {
    if ( vcl_abs( iterO.Get() - (iterA.Get() + iterB.Get() + iterC.Get()) ) > epsilon ) ++failures;
    ++iterA;
    ++iterB;
    ++iterC;
    ++iterO;
    }

  if ( failures > 0 )
    {
    std::cout << "Got " << failures << " different pixels." << std::endl;
    return EXIT_FAILURE;
    }

  
  // Execute the filter in place
  filter->InPlaceOn();
  filter->Update();

  PrintImage( outputImage, "Resulting image" );

  
  // Test the validity of the output
  IteratorOut iterO2( outputImage, region );
  failures = 0;
  while ( !iterO2.IsAtEnd() )
    {
    // Here we cannot test using the input iterators anymore since
    // inputImageA should have been overwritten
    if ( vcl_abs( iterO2.Get() - (12+17-4) ) > epsilon ) ++failures;
    ++iterO2;
    }

  if ( failures > 0 )
    {
    std::cout << "Got " << failures << " different pixels." << std::endl;
    return EXIT_FAILURE;
    }
  
  // Testing with vector Images
  typedef itk::Image< itk::Vector<double,2>, 2> VectorImageType;
  typedef itk::NaryAddImageFilter< VectorImageType, VectorImageType > NaryAdderType;
  NaryAdderType::Pointer adder = NaryAdderType::New();

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}




