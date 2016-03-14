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

#include "itkEigenAnalysis2DImageFilter.h"
#include "itkFilterWatcher.h"


// Define the dimension of the images
const unsigned int myDimension = 2;

// Declare type for Eigen Vectors
typedef itk::Vector<double, myDimension> myVectorType;

// Declare the types of the images
typedef itk::Image<double, myDimension>           myImageType;
typedef itk::Image<myVectorType, myDimension>     myVectorImageType;

// Declare the type of the index to access images
typedef itk::Index<myDimension>             myIndexType;

// Declare the type of the size
typedef itk::Size<myDimension>              mySizeType;

// Declare the type of the Region
typedef itk::ImageRegion<myDimension>        myRegionType;


// Declare Iterator types apropriated for each image
typedef itk::ImageRegionIteratorWithIndex<myImageType>       myIteratorType;
typedef itk::ImageRegionIteratorWithIndex<myVectorImageType> myVectorIteratorType;


// Declare the Filter
typedef itk::EigenAnalysis2DImageFilter< myImageType,
                                         myImageType,
                                         myVectorImageType >  myFilterType;

// Function for image initialization
void InitializeImage( myImageType * image, double value   )
{

  myImageType::Pointer inputImage( image );

  // Define their size, and start index
  mySizeType size;
  size[0] = 2;
  size[1] = 2;

  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  myIteratorType it( inputImage,
                     inputImage->GetRequestedRegion() );

  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( value );
    ++it;
    }

}

// Function for image printing
void PrintImage( myImageType * image, const char *text )
{

  myImageType::Pointer imagePtr( image );

  // Create an iterator for going through the image
  myIteratorType it( imagePtr,
                     imagePtr->GetRequestedRegion() );

  it.GoToBegin();

  //  Print the content of the image
  std::cout << text << std::endl;
  while( !it.IsAtEnd() )
  {
    std::cout << it.Get() << std::endl;
    ++it;
  }

}


// Function for image printing
void PrintImage( myVectorImageType * image, const char *text )
{

  myVectorImageType::Pointer imagePtr( image );

  // Create an iterator for going through the image
  myVectorIteratorType it( imagePtr,
                           imagePtr->GetRequestedRegion() );

  it.GoToBegin();

  //  Print the content of the image
  std::cout << text << std::endl;
  while( !it.IsAtEnd() )
  {
    std::cout << it.Get() << std::endl;
    ++it;
  }

}

int itkEigenAnalysis2DImageFilterTest(int, char* [] )
{
  // Create the images
  myImageType::Pointer inputImageXX  = myImageType::New();
  myImageType::Pointer inputImageXY  = myImageType::New();
  myImageType::Pointer inputImageYY  = myImageType::New();


  InitializeImage( inputImageXX, std::cos( itk::Math::pi / 6.0 ) );
  InitializeImage( inputImageXY, std::sin( itk::Math::pi / 6.0 ) );
  InitializeImage( inputImageYY, std::cos( itk::Math::pi / 6.0 ) );


  // Create a  Filter
  myFilterType::Pointer filter = myFilterType::New();
  FilterWatcher watcher(filter);

  // Connect the input images
  filter->SetInput1( inputImageXX );
  filter->SetInput2( inputImageXY );
  filter->SetInput3( inputImageYY );


  // Execute the filter
  filter->Update();

  // Get
  myImageType::Pointer maxEigenValue = filter->GetMaxEigenValue();
  myImageType::Pointer minEigenValue = filter->GetMinEigenValue();

  myVectorImageType::Pointer maxEigenVector = filter->GetMaxEigenVector();

  PrintImage( maxEigenValue, "Max Eigen Value");
  PrintImage( minEigenValue, "Min Eigen Value");
  PrintImage( maxEigenVector, "Max Eigen Vector");


  // All objects should be automatically destroyed at this point

  return EXIT_SUCCESS;

}
