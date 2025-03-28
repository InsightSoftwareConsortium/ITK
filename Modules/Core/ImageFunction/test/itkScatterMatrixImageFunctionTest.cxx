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


#include "itkScatterMatrixImageFunction.h"
#include "itkTestingMacros.h"

int
itkScatterMatrixImageFunctionTest(int, char *[])
{

  constexpr unsigned int Dimension = 3;
  using PixelComponentType = unsigned char;
  constexpr unsigned int VectorDimension = 4;

  using PixelType = itk::FixedArray<PixelComponentType, VectorDimension>;
  using ImageType = itk::Image<PixelType, Dimension>;
  using FunctionType = itk::ScatterMatrixImageFunction<ImageType>;

  // Create and allocate the image
  auto                  image = ImageType::New();
  ImageType::SizeType   size;
  ImageType::IndexType  start;
  ImageType::RegionType region;

  size[0] = 20;
  size[1] = 20;
  size[2] = 20;

  start.Fill(0);

  region.SetIndex(start);
  region.SetSize(size);

  image->SetRegions(region);
  image->Allocate();

  ImageType::PixelType initialValue;

  initialValue[0] = 11;
  initialValue[1] = 13;
  initialValue[2] = 17;
  initialValue[3] = 19;

  image->FillBuffer(initialValue);

  auto function = FunctionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(function, ScatterMatrixImageFunction, ImageFunction);


  constexpr unsigned int neighborhoodRadius = 5;
  function->SetNeighborhoodRadius(neighborhoodRadius);
  ITK_TEST_SET_GET_VALUE(neighborhoodRadius, function->GetNeighborhoodRadius());

  function->SetInputImage(image);

  ImageType::IndexType index;

  index[0] = 10;
  index[1] = 10;
  index[2] = 10;

  FunctionType::OutputType scatterMatrix = function->EvaluateAtIndex(index);
  std::cout << "function->EvaluateAtIndex( index ): " << scatterMatrix << std::endl;

  // Test Evaluate
  FunctionType::PointType point;
  point[0] = 25;
  point[1] = 25;
  point[2] = 25;
  const FunctionType::OutputType covariance2 = function->Evaluate(point);
  std::cout << "function->Evaluate(point): " << covariance2 << std::endl;

  // Test EvaluateAtContinuousIndex
  FunctionType::ContinuousIndexType cindex;
  cindex[0] = 25;
  cindex[1] = 25;
  cindex[2] = 25;
  const FunctionType::OutputType covariance3 = function->EvaluateAtContinuousIndex(cindex);
  std::cout << "function->EvaluateAtContinuousIndex(cindex): " << covariance3 << std::endl;

  // Test GetConstReferenceMacro
  const unsigned int & neighborhoodRadiusConst = function->GetNeighborhoodRadius();
  std::cout << "function->GetNeighborhoodRadius(): " << neighborhoodRadiusConst << std::endl;

  std::cout << "Scatter Matrix = " << std::endl;
  std::cout << scatterMatrix << std::endl;

  // since the input image is constant
  // the should be equal to the initial value
  for (unsigned int ix = 0; ix < VectorDimension; ++ix)
  {
    for (unsigned int iy = 0; iy < VectorDimension; ++iy)
    {
      if (itk::Math::abs(initialValue[ix] * initialValue[iy] - scatterMatrix[ix][iy]) > 10e-7)
      {
        std::cerr << "Error in scatterMatrix computation" << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
