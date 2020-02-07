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

#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkRGBPixel.h"
#include "itkRGBToLuminanceImageAdaptor.h"

int
itkSmoothingRecursiveGaussianImageFilterOnImageAdaptorTest(int, char *[])
{

  // Define the dimension of the images
  constexpr unsigned int myDimension = 3;

  using RGBPixelType = itk::RGBPixel<float>;

  // Declare the types of the images
  using myImageType = itk::Image<RGBPixelType, myDimension>;

  using FloatImageType = itk::Image<float, myDimension>;

  // Declare the type of the index to access images
  using myIndexType = itk::Index<myDimension>;

  // Declare the type of the size
  using mySizeType = itk::Size<myDimension>;

  // Declare the type of the Region
  using myRegionType = itk::ImageRegion<myDimension>;

  constexpr unsigned int numberOfComponents = 3;

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
  inputImage->SetNumberOfComponentsPerPixel(numberOfComponents);
  inputImage->Allocate();

  // Declare Iterator type for the input image
  using myIteratorType = itk::ImageRegionIteratorWithIndex<myImageType>;

  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it(inputImage, inputImage->GetRequestedRegion());

  // Initialize the content of Image A
  while (!it.IsAtEnd())
  {
    myImageType::PixelType p(numberOfComponents);
    p.Fill(0.0);
    it.Set(p);
    ++it;
  }

  size[0] = 4;
  size[1] = 4;
  size[2] = 4;

  start[0] = 2;
  start[1] = 2;
  start[2] = 2;

  // Create one iterator for an internal region
  region.SetSize(size);
  region.SetIndex(start);
  myIteratorType itb(inputImage, region);

  // Initialize the content the internal region
  while (!itb.IsAtEnd())
  {
    myImageType::PixelType p = itb.Get();
    p.Fill(100);
    itb.Set(p);
    ++itb;
  }

  // Create Image adaptor on the RGB Image
  using myAdaptorType = itk::RGBToLuminanceImageAdaptor<myImageType, float>;
  myAdaptorType::Pointer adaptor = myAdaptorType::New();
  adaptor->SetImage(inputImage);


  // Declare the type for the
  using myFilterType = itk::SmoothingRecursiveGaussianImageFilter<myAdaptorType, FloatImageType>;

  using myGradientImageType = myFilterType::OutputImageType;


  // Create a  Filter
  myFilterType::Pointer    filter = myFilterType::New();
  itk::SimpleFilterWatcher watchit(filter);

  // Connect the input images
  filter->SetInput(adaptor);

  // Select the value of Sigma
  filter->SetSigma(2.5);


  // Execute the filter
  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    (&err)->Print(std::cerr);
    return EXIT_FAILURE;
  }


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
  while (!itg.IsAtEnd())
  {
    std::cout << itg.Get() << std::endl;
    ++itg;
  }

  // All objects should be automatically destroyed at this point
  std::cout << std::endl << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
