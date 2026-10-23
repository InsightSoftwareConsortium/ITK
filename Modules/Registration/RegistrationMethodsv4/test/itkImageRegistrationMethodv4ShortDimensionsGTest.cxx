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

#include "itkImageRegistrationMethodv4.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkRegularStepGradientDescentOptimizerv4.h"
#include "itkTranslationTransform.h"

#include <cmath>

#include <gtest/gtest.h>

TEST(ImageRegistrationMethodv4ShortDimensions, SmoothSingletonImagesWithoutRecursiveGradientFilters)
{
  using ImageType = itk::Image<float, 3>;
  using TransformType = itk::TranslationTransform<double, 3>;
  using RegistrationType = itk::ImageRegistrationMethodv4<ImageType, ImageType, TransformType>;
  for (unsigned int shortAxis = 0; shortAxis < 3; ++shortAxis)
  {
    SCOPED_TRACE(shortAxis);
    auto size = ImageType::SizeType::Filled(16);
    size[shortAxis] = 1;
    auto image = ImageType::New();
    image->SetRegions(size);
    image->Allocate();
    for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd(); ++it)
    {
      const auto index = it.GetIndex();
      double     squaredDistance = 0;
      for (unsigned int d = 0; d < 3; ++d)
      {
        if (d != shortAxis)
        {
          const double distance = index[d] - 7.0;
          squaredDistance += distance * distance;
        }
      }
      it.Set(static_cast<float>(std::exp(-squaredDistance / 16.0)));
    }

    auto metric = itk::MeanSquaresImageToImageMetricv4<ImageType, ImageType>::New();
    metric->SetUseFixedImageGradientFilter(false);
    metric->SetUseMovingImageGradientFilter(false);
    auto optimizer = itk::RegularStepGradientDescentOptimizerv4<double>::New();
    optimizer->SetLearningRate(1.0);
    optimizer->SetMinimumStepLength(0.001);
    optimizer->SetNumberOfIterations(2);
    auto registration = RegistrationType::New();
    registration->SetFixedImage(image);
    registration->SetMovingImage(image);
    registration->SetMetric(metric);
    registration->SetOptimizer(optimizer);
    registration->SetInitialTransform(TransformType::New());
    registration->SetNumberOfLevels(2);
    RegistrationType::ShrinkFactorsArrayType shrinkFactors(2);
    shrinkFactors[0] = 2;
    shrinkFactors[1] = 1;
    registration->SetShrinkFactorsPerLevel(shrinkFactors);
    RegistrationType::SmoothingSigmasArrayType sigmas(2);
    sigmas[0] = 1.0;
    sigmas[1] = 0.5;
    registration->SetSmoothingSigmasPerLevel(sigmas);
    ASSERT_NO_THROW(registration->Update());
    for (const double parameter : registration->GetTransform()->GetParameters())
    {
      EXPECT_NEAR(parameter, 0.0, 1e-8);
    }
  }
}
