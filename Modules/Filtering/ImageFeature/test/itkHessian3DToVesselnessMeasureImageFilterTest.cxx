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

#include "itkHessianRecursiveGaussianImageFilter.h"
#include "itkHessian3DToVesselnessMeasureImageFilter.h"


int
itkHessian3DToVesselnessMeasureImageFilterTest(int, char *[])
{

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
  size[0] = 8;
  size[1] = 8;
  size[2] = 8;

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

  size[0] = 1;
  size[1] = 8;
  size[2] = 1;

  start[0] = 3;
  start[1] = 0;
  start[2] = 3;

  // Create one iterator for an internal region
  region.SetSize(size);
  region.SetIndex(start);
  myIteratorType itb(inputImage, region);

  // Initialize the content the internal region
  while (!itb.IsAtEnd())
  {
    itb.Set(100.0);
    ++itb;
  }

  // Declare the type for the Hessian filter
  using myHessianFilterType = itk::HessianRecursiveGaussianImageFilter<myImageType>;

  // Declare the type for the vesselness filter
  using myVesselnessFilterType = itk::Hessian3DToVesselnessMeasureImageFilter<float>;

  using myVesselnessImageType = myVesselnessFilterType::OutputImageType;


  // Create a Hessian Filter
  myHessianFilterType::Pointer filterHessian = myHessianFilterType::New();

  // Create a vesselness Filter
  myVesselnessFilterType::Pointer filterVesselness = myVesselnessFilterType::New();


  // Connect the input images
  filterHessian->SetInput(inputImage);
  filterVesselness->SetInput(filterHessian->GetOutput());

  // Select the value of Sigma
  filterHessian->SetSigma(0.5);


  // Execute the filter
  filterVesselness->Update();

  // Get the Smart Pointer to the Filter Output
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myVesselnessImageType::Pointer outputImage = filterVesselness->GetOutput();

  // Declare Iterator type for the output image
  using myOutputIteratorType = itk::ImageRegionIteratorWithIndex<myVesselnessImageType>;

  // Create an iterator for going through the output image
  myOutputIteratorType itg(outputImage, outputImage->GetRequestedRegion());

  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  itg.GoToBegin();
  while (!itg.IsAtEnd())
  {
    std::cout << itg.Get() << " ";
    ++itg;
  }


  return EXIT_SUCCESS;
}
