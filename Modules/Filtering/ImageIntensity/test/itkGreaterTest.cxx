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
#include "itkLogicOpsFunctors.h"
#include "itkBinaryFunctorImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkLogicTestSupport.h"

int itkGreaterTest(int, char* [] )
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<float, myDimension>  myImageType1;
  typedef itk::Image<float, myDimension>  myImageType2;
  typedef itk::Image<float, myDimension>  myImageType3;
  typedef myImageType1::PixelType         PixelType;
  // Declare the type of the index to access images
  typedef itk::Index<myDimension>         myIndexType;

  // Declare the type of the size
  typedef itk::Size<myDimension>          mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Declare the type for the ADD filter
  typedef itk::BinaryFunctorImageFilter<
    myImageType1,
    myImageType2,
    myImageType3,
    itk::Functor::Greater<myImageType1::PixelType,
                          myImageType2::PixelType,
                          myImageType3::PixelType>
    >       myFilterType;

  // Declare the pointers to images
  typedef myImageType1::Pointer   myImageType1Pointer;
  typedef myImageType2::Pointer   myImageType2Pointer;
  typedef myImageType3::Pointer   myImageType3Pointer;
  typedef myFilterType::Pointer   myFilterTypePointer;

  // Create two images
  myImageType1Pointer inputImageA  = myImageType1::New();
  myImageType2Pointer inputImageB  = myImageType2::New();

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

  // Initialize Image B
  inputImageB->SetLargestPossibleRegion( region );
  inputImageB->SetBufferedRegion( region );
  inputImageB->SetRequestedRegion( region );
  inputImageB->Allocate();


  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIteratorWithIndex<myImageType1>  myIteratorType1;
  typedef itk::ImageRegionIteratorWithIndex<myImageType2>  myIteratorType2;

  // Create one iterator for Image A (this is a light object)
  myIteratorType1 it1( inputImageA, inputImageA->GetBufferedRegion() );

  // Initialize the content of Image A
  it1.Set( 3.0 );
  ++it1;
  while( !it1.IsAtEnd() )
  {
    it1.Set( 2.0 );
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  myIteratorType2 it2( inputImageB, inputImageB->GetBufferedRegion() );

  // Initialize the content of Image B
  while( !it2.IsAtEnd() )
  {
    it2.Set( 3.0 );
    ++it2;
  }

  {
  // Create a logic Filter
  myFilterTypePointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput1( inputImageA );
  filter->SetInput2( inputImageB );

  filter->SetFunctor(filter->GetFunctor());

  // Get the Smart Pointer to the Filter Output
  myImageType3Pointer outputImage = filter->GetOutput();


  // Execute the filter
  filter->Update();
  filter->SetFunctor(filter->GetFunctor());
  PixelType FG = filter->GetFunctor().GetForegroundValue();
  PixelType BG = filter->GetFunctor().GetBackgroundValue();

  int status1 = checkImOnImRes < myImageType1, myImageType2, myImageType3, std::greater<myImageType1::PixelType> >
                             (inputImageA, inputImageB, outputImage, FG, BG);
  if (status1 == EXIT_FAILURE)
    {
    return(EXIT_FAILURE);
    }
  else
    {
    std::cout << "Step 1 passed" << std::endl;
    }
  }

  {
  // Create a logic Filter
  myFilterTypePointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput1( inputImageA );

  filter->SetFunctor(filter->GetFunctor());

  // Get the Smart Pointer to the Filter Output
  myImageType3Pointer outputImage = filter->GetOutput();

  // Now try testing with constant : Im1 > 2
  filter->SetConstant(2.0);
  filter->Update();
  PixelType FG = filter->GetFunctor().GetForegroundValue();
  PixelType BG = filter->GetFunctor().GetBackgroundValue();
  PixelType C = filter->GetConstant2();
  int status2 = checkImOnConstRes < myImageType1, PixelType, myImageType3, std::greater<PixelType> >
                                (inputImageA, C, outputImage, FG, BG);
  if (status2 == EXIT_FAILURE)
    {
    return(EXIT_FAILURE);
    }
  else
    {
    std::cout << "Step 2 passed " << std::endl;
    }
  }
  // Now try testing with constant : 3 != Im2
  {
  // Create a logic Filter
  myFilterTypePointer filter = myFilterType::New();

  // Connect the input images
  filter->SetFunctor(filter->GetFunctor());

  // Get the Smart Pointer to the Filter Output
  myImageType3Pointer outputImage = filter->GetOutput();
  filter->SetConstant1(3.0);
  filter->SetInput2(inputImageB);
  filter->Update();
  PixelType FG = filter->GetFunctor().GetForegroundValue();
  PixelType BG = filter->GetFunctor().GetBackgroundValue();

  int status3 = checkConstOnImRes < PixelType, myImageType2, myImageType3, std::greater<PixelType> >
                                (filter->GetConstant1(),  inputImageB, outputImage, FG, BG);
  if (status3 == EXIT_FAILURE)
    {
    return(EXIT_FAILURE);
    }
  else
    {
    std::cout << "Step 3 passed" << std::endl;
    }
  }
  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
