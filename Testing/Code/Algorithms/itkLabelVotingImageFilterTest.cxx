/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelVotingImageFilterTest.cxx
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
#include <itkLabelVotingImageFilter.h>
#include <itkImageRegionIterator.h>


int itkLabelVotingImageFilterTest(int, char* [] ) 
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<unsigned int, myDimension>  myImageType;

  // Input data arrays for test images
  const unsigned int dataImageA[8] = 
    { 0, 1, 3, 3, 4, 6, 6, 0 };
  const unsigned int dataImageB[8] = 
    { 1, 1, 2, 4, 4, 5, 7, 1 };
  const unsigned int dataImageC[8] = 
    { 0, 2, 2, 3, 5, 5, 6, 8 };

  // Correct combinations of input images
  const unsigned int combinationABC[8] = 
    { 0, 1, 2, 3, 4, 5, 6, 9 };
  const unsigned int combinationAB[8] = 
    { 8, 1, 8, 8, 4, 8, 8, 8 };
  const unsigned int combinationABundecided255[8] = 
    { 255, 1, 255, 255, 4, 255, 255, 255 };

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>         myIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>          mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Declare Iterator type apropriate for image 
  typedef itk::ImageRegionIterator<myImageType>  myIteratorType;

  // Declare the type for the ADD filter
  typedef itk::LabelVotingImageFilter<myImageType> myFilterType;
  typedef myFilterType::Pointer myFilterTypePointer;
 
  // Declare the pointers to images
  typedef myImageType::Pointer   myImageTypePointer;

  // Create two images
  myImageTypePointer inputImageA  = myImageType::New();
  myImageTypePointer inputImageB  = myImageType::New();
  myImageTypePointer inputImageC  = myImageType::New();
  
  // Define their size, and start index
  mySizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

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

  myIteratorType it = 
    myIteratorType( inputImageA, inputImageA->GetBufferedRegion() );
  
  for( int i = 0; i < 8; ++i, ++it )
    {
    it.Set( dataImageA[i] );
    }

  // Initialize Image B
  inputImageB->SetLargestPossibleRegion( region );
  inputImageB->SetBufferedRegion( region );
  inputImageB->SetRequestedRegion( region );
  inputImageB->Allocate();

  it = myIteratorType( inputImageB, inputImageB->GetBufferedRegion() );
  for( int i = 0; i < 8; ++i, ++it )
    {
    it.Set( dataImageB[i] );
    }

  // Initialize Image C
  inputImageC->SetLargestPossibleRegion( region );
  inputImageC->SetBufferedRegion( region );
  inputImageC->SetRequestedRegion( region );
  inputImageC->Allocate();

  it = myIteratorType( inputImageC, inputImageC->GetBufferedRegion() );
  for( int i = 0; i < 8; ++i, ++it )
    {
    it.Set( dataImageC[i] );
    }

  // Create an LabelVoting Filter                                
  myFilterTypePointer filter = myFilterType::New();

  // Get the Smart Pointer to the Filter Output 
  myImageTypePointer outputImage = filter->GetOutput();
  
  // = test first two input images with undecided label set to 255 = //

  // Connect the first two input images
  filter->SetInput( 0, inputImageA ); 
  filter->SetInput( 1, inputImageB ); 

  // Set label for undecided pixels
  filter->SetLabelForUndecidedPixels( 255 );

  // Execute the filter
  filter->Update();

  // compare to correct results
  it = myIteratorType( outputImage, outputImage->GetBufferedRegion() );
  for( unsigned int i = 0; i < 8; ++i, ++it )
    {
    if( combinationABundecided255[i] != it.Get() )
      {
      std::cout << "Incorrect result using images A,B and undecided=255: "
                << "i = " << i 
                << ", correct = " << combinationABundecided255[i]
                << ", got = " << it.Get() << "\n";
      return EXIT_FAILURE;
      }
    }

  // =========== test first two input images ============ //

  // unset undecided pixel label; reinstate automatic selection
  filter->UnsetLabelForUndecidedPixels();

  // Execute the filter
  filter->Update();

  // compare to correct results
  it = myIteratorType( outputImage, outputImage->GetBufferedRegion() );
  for(unsigned int i = 0; i < 8; ++i, ++it )
    {
    if( combinationAB[i] != it.Get() )
      {
      std::cout << "Incorrect result using images A,B: i = " << i 
                << ", correct = " << combinationAB[i]
                << ", got = " << it.Get() << "\n";
      return EXIT_FAILURE;
      }
    }

  // =========== test all three input images ============ //

  // connect third input image
  filter->SetInput( 2, inputImageC ); 

  // Execute the filter
  filter->Update();

  // compare to correct results
  it = myIteratorType( outputImage, outputImage->GetBufferedRegion() );
  for( unsigned int i = 0; i < 8; ++i, ++it )
    {
    if( combinationABC[i] != it.Get() )
      {
      std::cout << "Incorrect result using images A,B,C: i = " << i 
                << ", correct = " << combinationABC[i]
                << ", got = " << it.Get() << "\n";
      return EXIT_FAILURE;
      }
    }
  
  std::cout << "Success!\n";

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}

