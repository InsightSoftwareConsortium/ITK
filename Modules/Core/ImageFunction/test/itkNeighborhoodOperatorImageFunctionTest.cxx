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


#include "itkNeighborhoodOperatorImageFunction.h"

#include "itkGaussianOperator.h"

int
itkNeighborhoodOperatorImageFunctionTest(int, char *[])
{

  constexpr unsigned int Dimension = 3;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;
  using NeighborhoodOperatorType = itk::GaussianOperator<PixelType, 3>;
  using FunctionType = itk::NeighborhoodOperatorImageFunction<ImageType, PixelType>;

  // Create and allocate the image
  auto                  image = ImageType::New();
  ImageType::SizeType   size;
  ImageType::IndexType  start;
  ImageType::RegionType region;

  size[0] = 50;
  size[1] = 50;
  size[2] = 50;

  start.Fill(0);

  region.SetIndex(start);
  region.SetSize(size);

  image->SetRegions(region);
  image->Allocate();

  ImageType::PixelType initialValue = 27;
  image->FillBuffer(initialValue);


  auto function = FunctionType::New();
  function->SetInputImage(image);

  auto * oper = new NeighborhoodOperatorType;
  oper->CreateToRadius(3);

  function->SetOperator(*oper);
  delete oper;

  itk::Index<3> index;
  index.Fill(25);

  FunctionType::OutputType Blur;

  std::cout << "EvaluateAtIndex: ";
  Blur = function->EvaluateAtIndex(index);

  // since the input image is constant
  // the should be equal to the initial value
  if (itk::Math::abs(initialValue - Blur) > 10e-7)
  {
    std::cerr << "[FAILED] : Error in Blur computation" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] " << std::endl;

  std::cout << "EvaluateAtContinuousIndex: ";

  FunctionType::ContinuousIndexType continuousIndex;
  continuousIndex.Fill(25);

  function->EvaluateAtContinuousIndex(continuousIndex);

  std::cout << "[PASSED] " << std::endl;

  std::cout << "EvaluateAtPoint: ";

  FunctionType::PointType point;
  point[0] = 25;
  point[1] = 25;
  point[2] = 25;

  function->Evaluate(point);

  std::cout << "[PASSED] " << std::endl;


  std::cout << function << std::endl;

  std::cout << "[TEST DONE] " << std::endl;
  return EXIT_SUCCESS;
}
