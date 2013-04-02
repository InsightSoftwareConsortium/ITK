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


#include "itkImageRegionIteratorWithIndex.h"
#include "itkMaskNegatedImageFilter.h"

int itkMaskNegatedImageFilterTest(int, char* [] )
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<float, myDimension>           InputImageType;
  typedef itk::Image<unsigned short, myDimension>  MaskImageType;
  typedef itk::Image<float, myDimension>           OutputImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>         myIndexType;

  // Declare the type of the size
  typedef itk::Size<myDimension>          mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Create two images
  InputImageType::Pointer inputImage  = InputImageType::New();
  MaskImageType::Pointer inputMask    = MaskImageType::New();

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

  // Initialize the image
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  // Initialize the mask
  inputMask->SetLargestPossibleRegion( region );
  inputMask->SetBufferedRegion( region );
  inputMask->SetRequestedRegion( region );
  inputMask->Allocate();


  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIteratorWithIndex<InputImageType>  InputIteratorType;
  typedef itk::ImageRegionIteratorWithIndex<MaskImageType>   MaskIteratorType;
  typedef itk::ImageRegionIteratorWithIndex<OutputImageType> OutputIteratorType;

  // Create one iterator for Image A (this is a light object)
  InputIteratorType inputIterator( inputImage, inputImage->GetBufferedRegion() );

  // Initialize the content of Image A
  std::cout << "First operand " << std::endl;
  while( !inputIterator.IsAtEnd() )
  {
    inputIterator.Set( 255.0 );
    std::cout << inputIterator.Get() << std::endl;
    ++inputIterator;
  }

  // Create one iterator for Image B (this is a light object)
  MaskIteratorType maskIterator( inputMask, inputMask->GetBufferedRegion() );

  // Initialize the content of Image B
  // Set to mask first 2 pixels and last 2 pixels and leave the rest as is
  std::cout << "Second operand " << std::endl;
  for(unsigned int i = 0; i<2; ++i, ++maskIterator) maskIterator.Set( 0 );

  while( !maskIterator.IsAtEnd() )
  {
    maskIterator.Set( 3 );
    ++maskIterator;
  }

  for(unsigned int i = 0; i< 3; ++i, --maskIterator) maskIterator.Set( 0 );

  maskIterator.GoToBegin();
  while( !maskIterator.IsAtEnd() )
    {
    std::cout << maskIterator.Get() << std::endl;
    ++maskIterator;
    }

  // Declare the type for the MaskNegated filter
  typedef itk::MaskNegatedImageFilter<
                           InputImageType,
                           MaskImageType,
                           OutputImageType  >       myFilterType;


  // Create an MaskNegated Filter
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput1( inputImage );
  filter->SetInput2( inputMask );
  filter->SetOutsideValue( 50 );

  // Get the Smart Pointer to the Filter Output
  OutputImageType::Pointer outputImage = filter->GetOutput();


  // Execute the filter
  filter->Update();
  filter->SetFunctor(filter->GetFunctor());

  // Create an iterator for going through the image output
  OutputIteratorType outputIterator(outputImage, outputImage->GetBufferedRegion());

  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  while( !outputIterator.IsAtEnd() )
    {
    std::cout << outputIterator.Get() << std::endl;
    ++outputIterator;
    }


  filter->Print( std::cout );

  // Test named mutator/accessors
  {
  filter->SetMaskImage( inputMask );
  myFilterType::MaskImageType::ConstPointer retrievedMask = filter->GetMaskImage();
  if(retrievedMask != inputMask)
    {
    std::cerr << "Mask not retrieved successfully!" << std::endl;
    return EXIT_FAILURE;
    }
  }

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}
