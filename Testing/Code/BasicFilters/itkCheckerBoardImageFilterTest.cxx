/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCheckerBoardImageFilterTest.cxx
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
#include <itkCheckerBoardImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>


int itkCheckerBoardImageFilterTest(int, char* [] ) 
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<unsigned int, myDimension>  myImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>         myIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>          mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Declare the type for the filter
  typedef itk::CheckerBoardImageFilter<
                               myImageType >   myFilterType;
 
  // Declare the type of the arrays that define how many 
  // checkers to have along every dimension.
  typedef myFilterType::PatternArrayType       myPatternArrayType;
  
  // Declare the pointers to images
  typedef myImageType::Pointer   myImageTypePointer;
  typedef myFilterType::Pointer  myFilterTypePointer;

  // Create two images
  myImageTypePointer inputImageA  = myImageType::New();
  myImageTypePointer inputImageB  = myImageType::New();
  
  // Define their size, and start index
  mySizeType size;
  size[0] = 40;
  size[1] = 40;
  size[2] = 40;

  myIndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImageA->SetLargestPossibleRegion( region );
  inputImageA->SetBufferedRegion( region );
  inputImageA->SetRequestedRegion( region );
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetLargestPossibleRegion( region );
  inputImageB->SetBufferedRegion( region );
  inputImageB->SetRequestedRegion( region );
  inputImageB->Allocate();


  // Declare Iterator types apropriated for each image 
  typedef itk::ImageRegionIteratorWithIndex<myImageType>  myIteratorType;

  // Create one iterator for Image A (this is a light object)
  myIteratorType it1( inputImageA, inputImageA->GetBufferedRegion() );

  // Initialize the content of Image A
  std::cout << "First operand " << std::endl;
  while( !it1.IsAtEnd() ) 
    {
    it1.Set( 2 );
    std::cout << it1.Get() << std::endl;
    ++it1;
    }

  // Create one iterator for Image B (this is a light object)
  myIteratorType it2( inputImageB, inputImageB->GetBufferedRegion() );

  // Initialize the content of Image B
  std::cout << "Second operand " << std::endl;
  while( !it2.IsAtEnd() ) 
    {
    it2.Set( 3 );
    std::cout << it2.Get() << std::endl;
    ++it2;
    }
           

  // Create the Filter                                
  myFilterTypePointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput1( inputImageA ); 
  filter->SetInput2( inputImageB );

  myPatternArrayType pattern;
  pattern[0] =  4; // number of checkers along X
  pattern[1] =  8; // number of checkers along Y
  pattern[2] = 10; // number of checkers along Z

  filter->SetCheckerPattern( pattern );

  // Get the Smart Pointer to the Filter Output 
  myImageTypePointer outputImage = filter->GetOutput();

  
  // Execute the filter
  filter->Update();

  // Create an iterator for going through the image output
  myIteratorType it3(outputImage, outputImage->GetBufferedRegion());
  
  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  while( !it3.IsAtEnd() ) 
    {
    std::cout << it3.Get() << std::endl;
    ++it3;
    }

  // Exerciset the GetCheckerPattern() method
  myPatternArrayType pattern2 = filter->GetCheckerPattern();

  for(unsigned int k=0; k<3; k++)
    {
    if( pattern2[k] != pattern[k] )
      {
      std::cerr << "error in SetCheckerPattern()/GetCheckerPattern() " << std::endl;
      std::cerr << "Expected = " << pattern  << std::endl;
      std::cerr << "Received = " << pattern2 << std::endl;
      return EXIT_FAILURE;
      }
    }
  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}




