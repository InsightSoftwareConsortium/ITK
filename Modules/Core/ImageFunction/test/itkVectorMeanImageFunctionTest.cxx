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


#include "itkVectorMeanImageFunction.h"
#include "itkTestingMacros.h"

int
itkVectorMeanImageFunctionTest(int, char *[])
{

  constexpr unsigned int Dimension{ 3 };
  using PixelComponentType = unsigned char;
  constexpr unsigned int VectorDimension{ 4 };

  using PixelType = itk::FixedArray<PixelComponentType, VectorDimension>;

  using ImageType = itk::Image<PixelType, Dimension>;
  using FunctionType = itk::VectorMeanImageFunction<ImageType>;

  // Create and allocate the image
  auto                          image = ImageType::New();
  constexpr ImageType::SizeType size{ 20, 20, 20 };
  ImageType::RegionType         region = { size };

  image->SetRegions(region);
  image->Allocate();

  ImageType::PixelType initialValue{ { 11, 13, 17, 19 } };
  image->FillBuffer(initialValue);

  auto function = FunctionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(function, VectorMeanImageFunction, ImageFunction);


  function->SetInputImage(image);

  function->SetNeighborhoodRadius(5);

  ImageType::IndexType     index{ 10, 10, 10 };
  FunctionType::OutputType mean = function->EvaluateAtIndex(index);
  std::cout << "function->EvaluateAtIndex( index ): " << mean << std::endl;

  // Test Evaluate
  FunctionType::PointType        point{ { 25, 25, 25 } };
  const FunctionType::OutputType mean2 = function->Evaluate(point);
  std::cout << "function->Evaluate(point): " << mean2 << std::endl;

  // Test EvaluateAtContinuousIndex
  FunctionType::ContinuousIndexType cindex{ { 25, 25, 25 } };
  const FunctionType::OutputType    mean3 = function->EvaluateAtContinuousIndex(cindex);
  std::cout << "function->EvaluateAtContinuousIndex(cindex): " << mean3 << std::endl;

  // Test GetConstReferenceMacro
  const unsigned int & neighborhoodRadius = function->GetNeighborhoodRadius();
  std::cout << "function->GetNeighborhoodRadius(): " << neighborhoodRadius << std::endl;


  // since the input image is constant
  // the should be equal to the initial value
  for (unsigned int ii = 0; ii < VectorDimension; ++ii)
  {
    if (itk::Math::abs(initialValue[ii] - mean[ii]) > 10e-7)
    {
      std::cerr << "Error in mean computation" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
