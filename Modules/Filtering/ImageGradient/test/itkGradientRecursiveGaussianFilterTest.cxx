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

#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkSimpleFilterWatcher.h"

int itkGradientRecursiveGaussianFilterTest(int, char* [] )
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

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
  size[0] = 8;
  size[1] = 8;
  size[2] = 8;

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

  // Declare Iterator type for the input image
  typedef itk::ImageRegionIteratorWithIndex<myImageType>  myIteratorType;

  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the content of Image A
  while( !it.IsAtEnd() )
    {
    it.Set( 0.0 );
    ++it;
    }

  size[0] = 4;
  size[1] = 4;
  size[2] = 4;

  start[0] = 2;
  start[1] = 2;
  start[2] = 2;

  // Create one iterator for an internal region
  myRegionType innerRegion;
  innerRegion.SetSize( size );
  innerRegion.SetIndex( start );
  myIteratorType itb( inputImage, innerRegion );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() )
    {
    itb.Set( 100.0 );
    ++itb;
    }

  // Declare the type for the
  typedef itk::GradientRecursiveGaussianImageFilter< myImageType >  myFilterType;

  typedef myFilterType::OutputImageType myGradientImageType;


  // Create a  Filter
  myFilterType::Pointer filter = myFilterType::New();
  itk::SimpleFilterWatcher watcher(filter);

  // Connect the input images
  filter->SetInput( inputImage );

  // Select the value of Sigma
  filter->SetSigma( 2.5 );


  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myGradientImageType::Pointer outputImage = filter->GetOutput();

  // Declare Iterator type for the output image
  typedef itk::ImageRegionIteratorWithIndex<myGradientImageType>  myOutputIteratorType;

  // Create an iterator for going through the output image
  myOutputIteratorType itg( outputImage, outputImage->GetRequestedRegion() );

  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  itg.GoToBegin();
  while( !itg.IsAtEnd() )
    {
    std::cout << itg.Get();
    ++itg;
    }
  std::cout << std::endl;

  //
  // Test with a change in image direction
  //
  myImageType::DirectionType direction;
  direction.Fill( 0.0 );
  direction[0][0] = -1.0;
  direction[1][1] = -1.0;
  direction[2][2] = -1.0;
  inputImage->SetDirection( direction );

  // Create a  Filter
  myFilterType::Pointer filter2 = myFilterType::New();
  filter2->SetInput( inputImage );
  filter2->SetSigma( 2.5 );
  filter2->Update();
  myGradientImageType::Pointer outputFlippedImage = filter2->GetOutput();

  // compare the output between identity direction and flipped direction
  std::cout << " Result of flipped image " << std::endl;
  myOutputIteratorType itf( outputFlippedImage, outputFlippedImage->GetRequestedRegion() );
  itf.GoToBegin();
  bool passed = true;
  while( !itf.IsAtEnd() )
    {
    std::cout << itf.Get();
    myImageType::IndexType index;
    for( unsigned int d = 0; d < myDimension; d++ )
      {
      index[d] = region.GetSize()[d] - 1 - itf.GetIndex()[d];
      }
    if( itf.Value() != outputImage->GetPixel( index ) )
      {
      passed = false;
      }
    ++itf;
    }
  std::cout << std::endl;
  if( ! passed )
    {
    std::cerr << "Flipped image gradient does not match regular image as expected." << std::endl;
    return EXIT_FAILURE;
    }

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
