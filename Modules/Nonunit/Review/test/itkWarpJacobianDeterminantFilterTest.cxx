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

#include "itkDisplacementFieldJacobianDeterminantFilter.h"


int itkWarpJacobianDeterminantFilterTest(int, char* [] )
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  typedef itk::Vector< double, ImageDimension >   DeformationPixelType;
  typedef unsigned char                           OutputPixelType;

  // Declare the types of the images
  typedef itk::Image<DeformationPixelType, ImageDimension>  DisplacementFieldType;
  typedef itk::Image<OutputPixelType, ImageDimension>       OutputImageType;

  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIteratorWithIndex< DisplacementFieldType >  DeformationIteratorType;
  typedef itk::ImageRegionIteratorWithIndex< OutputImageType >  OutputIteratorType;


  // Declare the type of the index to access images
  typedef itk::Index<ImageDimension>         IndexType;

  // Declare the type of the size
  typedef itk::Size<ImageDimension>          SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<ImageDimension>   RegionType;

  // Create two images
  DisplacementFieldType ::Pointer inputDisplacementField  = DisplacementFieldType ::New();

  // Define their size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputDisplacementField->SetLargestPossibleRegion( region );
  inputDisplacementField->SetBufferedRegion( region );
  inputDisplacementField->SetRequestedRegion( region );
  inputDisplacementField->Allocate();

  // Create one iterator for the Input Image (this is a light object)
  DeformationIteratorType it( inputDisplacementField, inputDisplacementField->GetBufferedRegion() );

  // Initialize the content of Image A
  DeformationPixelType vectorValue;
  vectorValue.Fill( 5.0 ); // FIXME: replace with something more interesting...

  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( vectorValue );
    std::cout << it.Get() << std::endl;
    ++it;
    }

  // Declare the type for the Log filter
  typedef itk::DisplacementFieldJacobianDeterminantFilter<
    DisplacementFieldType, float, OutputImageType  >   FilterType;


  // Create one Filter
  FilterType::Pointer filter = FilterType::New();


  // Connect the input images
  filter->SetInput( inputDisplacementField );

  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the image output
  OutputIteratorType ot(outputImage, outputImage->GetRequestedRegion());

  //  Check the content of the result image
  std::cout << "Verification of the output " << std::endl;

  ot.GoToBegin();
  it.GoToBegin();
  while( !ot.IsAtEnd() )
    {
    DeformationPixelType input  = it.Get();
    OutputPixelType output = ot.Get();
    std::cout << input << " => ";
    std::cout << output  << std::endl;
    ++ot;
    ++it;
    }

  return EXIT_SUCCESS;
}
