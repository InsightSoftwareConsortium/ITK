/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkHessianRecursiveGaussianImageFilter.h"
#include "itkTensorFractionalAnisotropyImageFilter.h"
#include "itkDiffusionTensor3D.h"
#include "itkTestingMacros.h"


int
itkTensorFractionalAnisotropyImageFilterTest(int, char *[])
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
  auto inputImage = myImageType::New();


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
  inputImage->SetRegions(region);
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
    itb.Set(100.0);
    ++itb;
  }

  // Declare the type for the tensor pixel and tensor image.
  using myTensorPixelType = itk::DiffusionTensor3D<double>;
  using myDTIImageType = itk::Image<myTensorPixelType, myDimension>;
  using myRealValueType = myTensorPixelType::RealValueType;

  // Declare the type for the image generator
  using myFilterType = itk::HessianRecursiveGaussianImageFilter<myImageType, myDTIImageType>;

  using myFaImageType = itk::Image<myRealValueType, myDimension>;


  // Create a  Filter
  auto filter = myFilterType::New();


  // Connect the input images
  filter->SetInput(inputImage);

  // Select the value of Sigma
  filter->SetSigma(2.5);


  using FAFilterType = itk::TensorFractionalAnisotropyImageFilter<myDTIImageType, myFaImageType>;

  auto fractionalAnisotropyFilter = FAFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    fractionalAnisotropyFilter, TensorFractionalAnisotropyImageFilter, UnaryFunctorImageFilter);

  fractionalAnisotropyFilter->SetInput(filter->GetOutput());

  // Execute the filter
  fractionalAnisotropyFilter->Update();
  fractionalAnisotropyFilter->SetFunctor(fractionalAnisotropyFilter->GetFunctor());

  // Get the Smart Pointer to the Filter Output
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myFaImageType::Pointer outputImage = fractionalAnisotropyFilter->GetOutput();

  // Declare Iterator type for the output image
  using myOutputIteratorType = itk::ImageRegionIteratorWithIndex<myFaImageType>;

  // Create an iterator for going through the output image
  myOutputIteratorType itg(outputImage, outputImage->GetRequestedRegion());

  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  itg.GoToBegin();
  while (!itg.IsAtEnd())
  {
    std::cout << itg.GetIndex() << " = " << itg.Get() << std::endl;
    ++itg;
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
