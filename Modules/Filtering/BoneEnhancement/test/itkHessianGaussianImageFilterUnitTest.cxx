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

#include "itkHessianGaussianImageFilter.h"
#include "gtest/gtest.h"

TEST(itkHessianGaussianImageFilterTest, ExerciseBasicMethods)
{
  const unsigned int Dimension = 2;
  using PixelType = int;
  using ImageType = itk::Image<PixelType, Dimension>;
  using HessianGaussianImageFilterType = itk::HessianGaussianImageFilter<ImageType>;
  HessianGaussianImageFilterType::Pointer hess_filter = HessianGaussianImageFilterType::New();

  /* Exercise basic set/get methods */
  EXPECT_EQ(1.0, hess_filter->GetSigma()) << "Initial value of Sigma should be 1";
  hess_filter->SetSigma(0.5);
  EXPECT_EQ(0.5, hess_filter->GetSigma());

  EXPECT_EQ(false, hess_filter->GetNormalizeAcrossScale()) << "Initial value of NormalizeAcrossScale should be false";
  hess_filter->SetNormalizeAcrossScale(true);
  EXPECT_EQ(true, hess_filter->GetNormalizeAcrossScale());
  hess_filter->NormalizeAcrossScaleOff();
  EXPECT_EQ(false, hess_filter->GetNormalizeAcrossScale());
  hess_filter->NormalizeAcrossScaleOn();
  EXPECT_EQ(true, hess_filter->GetNormalizeAcrossScale());
}
