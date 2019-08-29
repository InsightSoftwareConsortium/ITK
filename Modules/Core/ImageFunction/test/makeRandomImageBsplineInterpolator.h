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

#ifndef __makeRandomImageBsplineInterpolator_h
#define __makeRandomImageBsplineInterpolator_h

#include "itkBSplineResampleImageFunction.h"
#include "itkRandomImageSource.h"
template <typename BSplineInterpolatorFunctionType>
typename BSplineInterpolatorFunctionType::Pointer
makeRandomImageInterpolator(const int SplineOrder)
{
  using ImageType = typename BSplineInterpolatorFunctionType::InputImageType;

  /** Generate a random input image and connect to BSpline decomposition filter */

  using SourceType = itk::RandomImageSource<ImageType>;
  typename SourceType::Pointer source = SourceType::New();
  {
    using DirectionType = typename ImageType::DirectionType;
    DirectionType nonTrivialDirection;

    nonTrivialDirection[0][0] = 0;
    nonTrivialDirection[0][1] = -1;
    nonTrivialDirection[1][0] = 1;
    nonTrivialDirection[1][1] = 0;
    std::cout << "DIRECTION\n" << nonTrivialDirection << std::endl;
    source->SetDirection(nonTrivialDirection);
  }
  {
    using SpacingType = typename ImageType::SpacingType;
    SpacingType spacing;
    spacing.Fill(2.0);
    source->SetSpacing(spacing);
  }
  {
    using PointType = typename ImageType::PointType;
    PointType origin;
    origin.Fill(10.0);
    source->SetOrigin(origin);
  }
  {
    using SizeType = typename ImageType::SizeType;
    SizeType size;
    size.Fill(32);
    source->SetSize(size);
  }

  source->SetMin(0.0);
  source->SetMax(10.0);
  source->Update();
  typename ImageType::Pointer randImage = source->GetOutput();

  /** Set up a BSplineInterpolateImageFunction for comparison. */

  typename BSplineInterpolatorFunctionType::Pointer interpolator = BSplineInterpolatorFunctionType::New();

  interpolator->SetSplineOrder(SplineOrder);
  interpolator->SetInputImage(randImage);
  return interpolator;
}

#endif
