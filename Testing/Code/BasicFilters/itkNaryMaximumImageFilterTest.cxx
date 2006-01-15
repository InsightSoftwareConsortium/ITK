/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNaryMaximumImageFilterTest.cxx
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
#include "itkNaryMaximumImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"


// Create a namespace in order to avoid conflicts with other tests.

namespace NaryMaximumImageFilterTest
{

// Define the dimension of the images
const unsigned int Dimension = 3;

// Declare the types of the images
typedef itk::Image<float, Dimension>  InputImageType;
typedef itk::Image<float, Dimension>  OutputImageType;

// Declare the type of the index to access images
typedef itk::Index<Dimension>         IndexType;

// Declare the type of the size 
typedef itk::Size<Dimension>          SizeType;

// Declare the type of the Region
typedef itk::ImageRegion<Dimension>        RegionType;

// Declare the type of the Iterators
typedef itk::ImageRegionIteratorWithIndex< InputImageType >  InImageIteratorType;
typedef itk::ImageRegionIteratorWithIndex< OutputImageType > OutImageIteratorType;

// Declare the type for the ADD filter
typedef itk::NaryMaximumImageFilter<
                              InputImageType,
                              OutputImageType  >  FilterType;
 



// Function for image initialization
void InitializeImage( InputImageType * image, double value   )
{

  InputImageType::Pointer inputImage( image );

  // Define their size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start.Fill(0);

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );
  
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  InImageIteratorType it( inputImage, 
                     inputImage->GetRequestedRegion() );
  
  it.GoToBegin();
  while( !it.IsAtEnd() ) 
    {
    it.Set( value );
    ++it;
    }


}



// Function for image printing
void PrintImage( InputImageType * image, const char *)
{
  // Create an iterator for going through the image
  InImageIteratorType it( image, 
                          image->GetRequestedRegion() );
  
  it.GoToBegin();
  //  Print the content of the image
  //std::cout << text << std::endl;
  while( !it.IsAtEnd() ) 
  {
    std::cout << it.Get() << std::endl;
    ++it;
  }

}


} // end namespace NaryMaximumImageFilterTest


int itkNaryMaximumImageFilterTest(int, char* [] ) 
{

  // It is safe to open the namespace here because 
  // the symbols will not be exposed outside this function
  using namespace NaryMaximumImageFilterTest; 


  // Create two images
  InputImageType::Pointer inputImageA  = InputImageType::New();
  InputImageType::Pointer inputImageB  = InputImageType::New();
  
  static const int minValue = 12;
  static const int maxValue = 13;
  InitializeImage( inputImageA, minValue );
  InitializeImage( inputImageB, maxValue );

  PrintImage( inputImageA, "Input image A" ); 
  PrintImage( inputImageB, "Input image B" ); 

  // Create an ADD Filter                                
  FilterType::Pointer filter = FilterType::New();


  // Connect the input images
  filter->SetInput( 0, inputImageA ); 
  filter->SetInput( 1, inputImageB );

  // Get the Smart Pointer to the Filter Output 
  OutputImageType::Pointer outputImage = filter->GetOutput();

  
  // Execute the filter
  filter->Update();
  filter->SetFunctor(filter->GetFunctor());

  PrintImage( outputImage, "Resulting image 1" ); 

  OutImageIteratorType it( outputImage, 
              outputImage->GetRequestedRegion() );
  it.GoToBegin();
  while( !it.IsAtEnd() ) 
  {
    if (it.Get() != maxValue)
      {
      std::cerr << "Test Failed!" << std::endl;
      return -1;
      }
    ++it;
  }
  
  // now try it the other way
  InitializeImage( inputImageA, minValue );
  InitializeImage( inputImageB, maxValue );
  filter->SetInput( 1, inputImageA ); 
  filter->SetInput( 0, inputImageB );
  filter->InPlaceOff(); // let's make sure this works too, while we're at it...
  filter->Update();
  
  PrintImage( outputImage, "Resulting image 2" ); 
  
  OutImageIteratorType it2( outputImage, 
               outputImage->GetRequestedRegion() );
  it2.GoToBegin();
  while( !it2.IsAtEnd() ) 
  {
    if (it2.Get() != maxValue)
    {
      std::cerr << "Test Failed!" << std::endl;
      return -1;
    }
    ++it2;
  }
  
  std::cerr << "Test Passed!" << std::endl;
  // All objects should be automatically destroyed at this point
  return 0;

}




