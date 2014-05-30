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

#include <iostream>

#include "itkImageRegionIterator.h"

#include "itkBinaryThresholdImageFunction.h"

int itkBinaryThresholdImageFunctionTest(int, char* [] )
{
  std::cout << "itkBinaryThresholdImageFunctionTest Start" << std::endl;

  typedef itk::Image<float,3> FloatImage;

  FloatImage::Pointer    image  = FloatImage::New();
  FloatImage::RegionType region;
  FloatImage::SizeType   size; size.Fill(64);
  FloatImage::IndexType  index; index.Fill(0);

  region.SetIndex (index);
  region.SetSize (size);

  image->SetRegions( region );
  image->Allocate(true); // initialize buffer to zero

  for (unsigned int i = 0; i < FloatImage::ImageDimension; i++)
    {
    size[i] -= 20;
    index[i] += 10;
    }
  region.SetIndex (index);
  region.SetSize (size);

  itk::ImageRegionIterator<FloatImage> it(image,region);
  while (!it.IsAtEnd())
    {
    it.Set(100.0);
    ++it;
    }

  // Try the function inside and outside the region
  typedef itk::BinaryThresholdImageFunction<FloatImage> ImageFunction;
  ImageFunction::Pointer threshold = ImageFunction::New();

  ImageFunction::PointType  point;
  ImageFunction::ContinuousIndexType continuousIndex;

  threshold->SetInputImage(image);

  threshold->ThresholdBelow (100.0);
  index[0] = 11; index[1] = 11; index[2] = 11;

  int failed = 0;
  if (!threshold->EvaluateAtIndex(index)) failed++;

  threshold->ThresholdAbove (100.0);
  if (!threshold->EvaluateAtIndex(index)) failed++;

  threshold->ThresholdAbove (101.0);
  if (threshold->EvaluateAtIndex(index)) failed++;

  threshold->ThresholdBetween (100.0,100.0);
  if (!threshold->EvaluateAtIndex(index)) failed++;

  threshold->ThresholdBetween (-100.0,0.0);
  if (threshold->EvaluateAtIndex(index)) failed++;

  index[0] = 8; index[1] = 8; index[2] = 8;
  threshold->ThresholdBetween (100.0,200.0);
  if (threshold->EvaluateAtIndex(index)) failed++;

  std::cout << threshold;

  // Test Evaluate
  point[0] = 5; point[1] = 5; point[2] = 5;
  threshold->Evaluate(point);

  // Test EvaluateAtContinuousIndex
  continuousIndex[0] = 9;
  continuousIndex[1] = 9;
  continuousIndex[2] = 9;
  threshold->EvaluateAtContinuousIndex(continuousIndex);


  if (failed)
    {
    std::cout << "Failed!" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "Passed!" << std::endl;
    }
  return EXIT_SUCCESS;
}
