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

#include "itkMath.h"
#include "itkNearestNeighborExtrapolateImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkTestingMacros.h"

/**
 * This module tests the functionality of the
 * NearestNeighborExtrapolateImageFunction class.
 *
 */
int
itkNearestNeighborExtrapolateImageFunctionTest(int, char *[])
{
  using CoordRep = double;
  constexpr unsigned int ImageDimension = 2;
  using PixelType = unsigned char;
  constexpr unsigned int VectorDimension = 4;
  using VectorPixelType = itk::Vector<PixelType, VectorDimension>;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using VectorImageType = itk::Image<VectorPixelType, ImageDimension>;

  ImageType::SizeType imageSize;
  imageSize[0] = 5;
  imageSize[1] = 7;
  const ImageType::RegionType imageRegion(imageSize);

  auto image = ImageType::New();
  image->SetRegions(imageRegion);
  image->Allocate();

  auto vectorimage = VectorImageType::New();
  vectorimage->SetRegions(imageRegion);
  vectorimage->Allocate();

  using Iterator = itk::ImageRegionIterator<ImageType>;
  Iterator iter(image, imageRegion);
  iter.GoToBegin();
  unsigned char counter = 0;

  while (!iter.IsAtEnd())
  {
    iter.Set(counter++);
    ++iter;
  }

  using VectorIterator = itk::ImageRegionIterator<VectorImageType>;
  VectorIterator vectoriter(vectorimage, imageRegion);
  vectoriter.GoToBegin();
  counter = 0;

  while (!vectoriter.IsAtEnd())
  {
    VectorPixelType & vectorpixel = vectoriter.Value();
    vectorpixel.Fill(counter++);
    ++vectoriter;
  }

  // set up the extrapolator
  using FunctionType = itk::NearestNeighborExtrapolateImageFunction<ImageType, CoordRep>;
  auto function = FunctionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(function, NearestNeighborExtrapolateImageFunction, ExtrapolateImageFunction);


  using VectorFunctionType = itk::NearestNeighborExtrapolateImageFunction<VectorImageType, CoordRep>;
  auto vectorfunction = VectorFunctionType::New();

  function->SetInputImage(image);

  vectorfunction->SetInputImage(vectorimage);

  // evaluate at point inside the image
  FunctionType::PointType point;
  point[0] = 2.25;
  point[1] = 3.25;
  FunctionType::OutputType value = function->Evaluate(point);

  const VectorFunctionType::OutputType vectorvalue = vectorfunction->Evaluate(point);

  FunctionType::OutputType trueValue =
    itk::Math::Round<int>(point[0]) + (itk::Math::Round<int>(point[1])) * static_cast<double>(imageSize[0]);
  auto trueVectorValue = itk::MakeFilled<VectorFunctionType::OutputType>(trueValue);

  std::cout << "Point: " << point << " Value: " << value << " Vector Value: " << vectorvalue << std::endl;
  if (itk::Math::NotAlmostEquals(value, trueValue))
  {
    std::cout << "Value not the same as trueValue: " << trueValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
  }

  if (vectorvalue != trueVectorValue)
  {
    std::cout << "Vector Value not the same as trueVectorValue: " << trueVectorValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
  }

  // evaluate at point outside the image
  point[0] = 2.25;
  point[1] = 8.0;
  value = function->Evaluate(point);

  trueValue = itk::Math::Round<int>(point[0]) + (6.0) * static_cast<double>(imageSize[0]);

  std::cout << "Point: " << point << " Value: " << value << std::endl;
  if (itk::Math::NotAlmostEquals(value, trueValue))
  {
    std::cout << "Value not the same as trueValue: " << trueValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
  }

  // evaluate at index inside the image
  FunctionType::IndexType index;
  index[0] = 4;
  index[1] = 5;
  value = function->EvaluateAtIndex(index);

  trueValue = static_cast<double>(index[0]) + static_cast<double>(index[1]) * static_cast<double>(imageSize[0]);

  std::cout << "Index: " << index << " Value: " << value << std::endl;
  if (itk::Math::NotAlmostEquals(value, trueValue))
  {
    std::cout << "Value not the same as trueValue: " << trueValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
  }


  // evaluate at index outside the image
  index[0] = 8;
  index[1] = -1;
  value = function->EvaluateAtIndex(index);

  trueValue = static_cast<double>(4) + static_cast<double>(0) * static_cast<double>(imageSize[0]);

  std::cout << "Index: " << index << " Value: " << value << std::endl;
  if (itk::Math::NotAlmostEquals(value, trueValue))
  {
    std::cout << "Value not the same as trueValue: " << trueValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
