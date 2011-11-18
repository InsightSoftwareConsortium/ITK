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

#include "itkReflectImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"


int itkReflectImageFilterTest(int, char* [] )
{

  // Define the dimension of the image
  const unsigned int myDimension = 3;

  // Declare the types of the image
  typedef itk::Image<unsigned short, myDimension>  myImageType;

  // Declare the type of the Region
  typedef myImageType::RegionType       myRegionType;

  // Declare the type of the index to access images
  typedef myRegionType::IndexType        myIndexType;

  // Declare the type of the size
  typedef myRegionType::SizeType         mySizeType;

  // Declare the type for the ADD filter
  typedef itk::ReflectImageFilter<
                        myImageType,
                        myImageType  >  myFilterType;

  // Declare the pointers to images
  typedef myImageType::Pointer   myImagePointer;

  // Create an image
  myImagePointer inputImage = myImageType::New();

  // Define their size, and start index
  mySizeType size;
  size[0] = 15;
  size[1] = 2;
  size[2] = 1;

  myIndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();


  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIteratorWithIndex<myImageType>  myIteratorType;

  // Create one iterator for the input Image  (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the content of the input Image
  // with a ramp along the 0 direction
  std::cout << "Input Image " << std::endl;
  unsigned long counter = 0;
  while( !it.IsAtEnd() )
  {
    it.Set( counter );
    std::cout << it.Get() << std::endl;
    ++counter;
    ++it;
  }

  // Create the Filter
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input image
  filter->SetInput( inputImage );

  // Get the Smart Pointer to the Filter Output
  myImagePointer outputImage = filter->GetOutput();


  // Execute the filter
  filter->Update();

  // Create an iterator for going through the image output
  myIteratorType ot( outputImage, outputImage->GetRequestedRegion() );

  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  while( !ot.IsAtEnd() )
  {
    std::cout << ot.GetIndex() << " = " << ot.Get() << std::endl;
    ++ot;
  }


  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}
