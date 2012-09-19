/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkMultiLabelSTAPLEImageFilter.h"

int itkMultiLabelSTAPLEImageFilterTest(int, char* [] )
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<unsigned int, myDimension>  myImageType;

  // Input data arrays for test images
  const unsigned int dataImageA[8] = { 0, 1, 3, 3, 4, 6, 6, 0 };
  const unsigned int dataImageB[8] = { 1, 1, 2, 4, 4, 5, 7, 1 };
  const unsigned int dataImageC[8] = { 0, 2, 2, 3, 5, 5, 6, 8 };

  // Correct combinations of input images
  const unsigned int combinationABC[8] =            { 0, 1, 2, 3, 4, 5, 6, 9 };
  const unsigned int combinationAB[8] =             { 8, 1, 8, 8, 4, 8, 8, 8 };
  const unsigned int combinationABundecided255[8] = { 8, 1, 8, 8, 4, 8, 8, 8 };

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>                  myIndexType;

  // Declare the type of the size
  typedef itk::Size<myDimension>                   mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>            myRegionType;

  // Declare Iterator type appropriate for image
  typedef itk::ImageRegionIterator<myImageType>    myIteratorType;

  // Declare the type for the ADD filter
  typedef itk::MultiLabelSTAPLEImageFilter<myImageType> myFilterType;
  typedef myFilterType::Pointer                         myFilterTypePointer;

  // Declare the pointers to images
  typedef myImageType::Pointer   myImageTypePointer;

  // Create two images
  myImageTypePointer inputImageA = myImageType::New();
  myImageTypePointer inputImageB = myImageType::New();
  myImageTypePointer inputImageC = myImageType::New();

  myRegionType region;
    {
    // Define their size, and start index
    mySizeType size;
    size[0] = 2;
    size[1] = 2;
    size[2] = 2;

    myIndexType start;
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;

    region.SetIndex( start );
    region.SetSize( size );
    }

  // Initialize Image A
  inputImageA->SetLargestPossibleRegion( region );
  inputImageA->SetBufferedRegion( region );
  inputImageA->SetRequestedRegion( region );
  inputImageA->Allocate();

  myIteratorType it =
    myIteratorType( inputImageA, inputImageA->GetBufferedRegion() );

  for( unsigned int i = 0; i < 8; ++i, ++it )
    {
    it.Set( dataImageA[i] );
    }

  // Initialize Image B
  inputImageB->SetLargestPossibleRegion( region );
  inputImageB->SetBufferedRegion( region );
  inputImageB->SetRequestedRegion( region );
  inputImageB->Allocate();

  it = myIteratorType( inputImageB, inputImageB->GetBufferedRegion() );
  for( unsigned int i = 0; i < 8; ++i, ++it )
    {
    it.Set( dataImageB[i] );
    }

  // Initialize Image C
  inputImageC->SetLargestPossibleRegion( region );
  inputImageC->SetBufferedRegion( region );
  inputImageC->SetRequestedRegion( region );
  inputImageC->Allocate();

  it = myIteratorType( inputImageC, inputImageC->GetBufferedRegion() );
  for( unsigned int i = 0; i < 8; ++i, ++it )
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
  for( unsigned int i = 0; i < 8; ++i, ++it )
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

  filter->Print( std::cout, 3 );

  std::cout << "Prior probabilities: " << filter->GetPriorProbabilities() << std::endl;
  std::cout << "Confusion matrix 0 " << std::endl << filter->GetConfusionMatrix( 0 ) << std::endl;
  std::cout << "Confusion matrix 1 " << std::endl << filter->GetConfusionMatrix( 1 ) << std::endl;
  std::cout << "Confusion matrix 2 " << std::endl << filter->GetConfusionMatrix( 2 ) << std::endl;

  std::cout << "Success!\n";

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
