/*=========================================================================
 *
 *  Copyright NumFOCUS
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


int
itkGradientRecursiveGaussianFilterSpeedTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "usage: size reps" << std::endl;
    return EXIT_FAILURE;
  }

  int imageSize = std::stoi(argv[1]);
  int reps = std::stoi(argv[2]);

  std::cout << "imageSize: " << imageSize << " reps: " << reps << std::endl;

  // Define the dimension of the images
  constexpr unsigned int myDimension = 3;

  // Declare the types of the images
  using myImageType = itk::Image<float, myDimension>;

  // Declare the type of the index to access images
  using myIndexType = itk::Index<myDimension>;

  // Declare the type of the size
  using mySizeType = itk::Size<myDimension>;

  // Declare the type of the Region
  using myRegionType = itk::ImageRegion<myDimension>;

  // Create the image
  myImageType::Pointer inputImage = myImageType::New();


  // Define their size, and start index
  mySizeType size;
  size[0] = imageSize;
  size[1] = imageSize;
  size[2] = imageSize;

  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex(start);
  region.SetSize(size);

  // Initialize Image A
  inputImage->SetLargestPossibleRegion(region);
  inputImage->SetBufferedRegion(region);
  inputImage->SetRequestedRegion(region);
  inputImage->Allocate();

  // Declare Iterator type for the input image
  using myIteratorType = itk::ImageRegionIteratorWithIndex<myImageType>;

  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it(inputImage, inputImage->GetRequestedRegion());

  // Initialize the content of Image A
  while (!it.IsAtEnd())
  {
    it.Set(0.0);
    ++it;
  }

  size.Fill(imageSize - 4);

  start[0] = 2;
  start[1] = 2;
  start[2] = 2;

  // Create one iterator for an internal region
  myRegionType innerRegion;
  innerRegion.SetSize(size);
  innerRegion.SetIndex(start);
  myIteratorType itb(inputImage, innerRegion);

  // Initialize the content the internal region
  while (!itb.IsAtEnd())
  {
    itb.Set(100.0);
    ++itb;
  }

  // Declare the type for the
  using myFilterType = itk::GradientRecursiveGaussianImageFilter<myImageType>;

  using myGradientImageType = myFilterType::OutputImageType;


  // Create a  Filter
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput(inputImage);


  // loop
  for (int i = 0; i < reps; i++)
  {
    // Select the value of Sigma
    filter->SetSigma(2.5 + reps / 100.0);

    // Execute the filter
    filter->Update();
  }

  if (reps > 0)
  {
    // Get the Smart Pointer to the Filter Output
    // It is important to do it AFTER the filter is Updated
    // Because the object connected to the output may be changed
    // by another during GenerateData() call
    myGradientImageType::Pointer outputImage = filter->GetOutput();

    // Declare Iterator type for the output image
    using myOutputIteratorType = itk::ImageRegionIteratorWithIndex<myGradientImageType>;

    // Create an iterator for going through the output image
    myOutputIteratorType itg(outputImage, outputImage->GetRequestedRegion());

    //  Print the content of the result image
    std::cout << " Result " << std::endl;
    itg.GoToBegin();
    std::cout << itg.Get();
    std::cout << std::endl;
  }

  return EXIT_SUCCESS;
}
