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

#include "itkGaussianSpatialFunction.h"
#include "itkSpatialFunctionImageEvaluatorFilter.h"
#include "itkTestingMacros.h"

int
itkSpatialFunctionImageEvaluatorFilterTest(int, char *[])
{
  constexpr unsigned int dim = 3;

  // Image typedef
  using ImageType = itk::Image<unsigned char, dim>;

  // Create a new input image
  // Image size and spacing parameters
  ImageType::SizeValueType    sourceImageSize[] = { 20, 20, 20 };
  ImageType::SpacingValueType sourceImageSpacing[] = { 1.0, 1.0, 1.0 };
  ImageType::PointValueType   sourceImageOrigin[] = { 0, 0, 0 };

  // Create the sourceImage
  ImageType::Pointer sourceImage = ImageType::New();
  sourceImage->SetOrigin(sourceImageOrigin);
  sourceImage->SetSpacing(sourceImageSpacing);

  // Create a size object native to the sourceImage type
  ImageType::SizeType sourceImageSizeObject;
  // Set the size object to the array defined earlier
  sourceImageSizeObject.SetSize(sourceImageSize);
  // Create a region object native to the sourceImage type
  ImageType::RegionType largestPossibleRegion;
  // Resize the region
  largestPossibleRegion.SetSize(sourceImageSizeObject);
  // Set the largest legal region size (i.e. the size of the whole sourceImage) to what we just defined
  sourceImage->SetLargestPossibleRegion(largestPossibleRegion);
  // Set the buffered region
  sourceImage->SetBufferedRegion(largestPossibleRegion);
  // Set the requested region
  sourceImage->SetRequestedRegion(largestPossibleRegion);
  // Now allocate memory for the sourceImage
  sourceImage->Allocate();

  // Create and initialize a new Gaussian function
  using FunctionType = itk::GaussianSpatialFunction<char, dim>;
  FunctionType::Pointer func = FunctionType::New();

  // Run the image evaluator filter
  using TFilter = itk::SpatialFunctionImageEvaluatorFilter<FunctionType, ImageType, ImageType>;
  TFilter::Pointer filter = TFilter::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, SpatialFunctionImageEvaluatorFilter, ImageToImageFilter);


  filter->SetInput(sourceImage);
  filter->SetFunction(func);
  ImageType::Pointer outputImage = filter->GetOutput();

  filter->Update();

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
