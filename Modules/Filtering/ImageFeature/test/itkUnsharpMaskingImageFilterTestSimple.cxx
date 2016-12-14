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

#include "itkUnsharpMaskingImageFilter.h"
#include "itkFilterWatcher.h"
#include "itkTestingMacros.h"

int itkUnsharpMaskingImageFilterTestSimple(int, char* [] )
{
  // Define the dimension of the images
  const unsigned int myDimension = 2;

  // Declare the types of the images
  typedef itk::Image<float, myDimension> myImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension> myIndexType;

  // Declare the type of the size
  typedef itk::Size<myDimension> mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension> myRegionType;

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();


  // Define their size, and start index
  mySizeType size;
  size[0] = 20;
  size[1] = 4;

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
      if (it.GetIndex()[0] > itk::IndexValueType(size[0] / 2))
        {
        it.Set(1.0);
        }
      else
        {
        it.Set(0.0);
        }

    ++it;
    }

  // Declare the type for the
  typedef itk::UnsharpMaskingImageFilter< myImageType >  myFilterType;

  typedef myFilterType::OutputImageType myGradientImageType;

  // Create a  Filter
  myFilterType::Pointer filter = myFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS(filter, UnsharpMaskingImageFilter, ImageToImageFilter);

  FilterWatcher watchit(filter);

  // Connect the input images
  filter->SetInput( inputImage );

  // Select the value of Sigma
  filter->SetSigma( 2.5 );
  filter->SetAmount(0.8);
  filter->SetThreshold(0.01);

  // Execute the filter
  TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Get the Smart Pointer to the Filter Output
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myGradientImageType::Pointer outputImage = filter->GetOutput();

  // check that output is correct near the step
  start[0] = 9;
  float mins[4] = { -0.21f, -0.33f, 1.32f, 1.20f };
  float maxs[4] = { -0.20f, -0.32f, 1.33f, 1.21f };
  for (unsigned int i = 0; i < 4; i++)
    {
    if (outputImage->GetPixel(start) < mins[i]
        || outputImage->GetPixel(start) > maxs[i])
      {
      std::cerr << "Test FAILED! Unexpected value: ";
      std::cerr << outputImage->GetPixel(start) << std::endl;
      std::cerr << "Expected value between " << mins[i];
      std::cerr << " and " << maxs[i] << std::endl;
      return EXIT_FAILURE;
      }
    ++start[0];
    }

  // All objects should be automatically destroyed at this point
  std::cout << std::endl << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
