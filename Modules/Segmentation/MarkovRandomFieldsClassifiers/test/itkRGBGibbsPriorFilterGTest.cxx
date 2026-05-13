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

#include "itkGTest.h"
#include "itkMRFImageFilter.h"
#include "itkRGBGibbsPriorFilter.h"
#include "itkImage.h"
#include "itkVector.h"

namespace
{
constexpr unsigned int NumberOfBands{ 1 };
constexpr unsigned int ImageDimension{ 3 };
using PixelType = itk::Vector<unsigned short, NumberOfBands>;
using InputImageType = itk::Image<PixelType, ImageDimension>;
using LabelImageType = itk::Image<unsigned short, ImageDimension>;
using BaseFilterType = itk::MRFImageFilter<InputImageType, LabelImageType>;
using GibbsFilterType = itk::RGBGibbsPriorFilter<InputImageType, LabelImageType>;
} // namespace

TEST(RGBGibbsPriorFilter, DefaultNumberOfClassesIsZero)
{
  auto filter = GibbsFilterType::New();
  EXPECT_EQ(filter->GetNumberOfClasses(), 0u);
}

TEST(RGBGibbsPriorFilter, DefaultMaximumNumberOfIterationsIsTen)
{
  auto filter = GibbsFilterType::New();
  EXPECT_EQ(filter->GetMaximumNumberOfIterations(), 10u);
}

TEST(MRFImageFilter, DefaultMaximumNumberOfIterationsIsFifty)
{
  auto filter = BaseFilterType::New();
  EXPECT_EQ(filter->GetMaximumNumberOfIterations(), 50u);
}

TEST(RGBGibbsPriorFilter, SetGetNumberOfClassesViaSubclass)
{
  auto filter = GibbsFilterType::New();
  filter->SetNumberOfClasses(7u);
  EXPECT_EQ(filter->GetNumberOfClasses(), 7u);
}

TEST(RGBGibbsPriorFilter, SetGetMaximumNumberOfIterationsViaSubclass)
{
  auto filter = GibbsFilterType::New();
  filter->SetMaximumNumberOfIterations(25u);
  EXPECT_EQ(filter->GetMaximumNumberOfIterations(), 25u);
}

TEST(RGBGibbsPriorFilter, SetGetNumberOfClassesViaBasePointer)
{
  auto                   gibbs = GibbsFilterType::New();
  const BaseFilterType * basePtr = gibbs;
  BaseFilterType *       baseMutable = gibbs;
  baseMutable->SetNumberOfClasses(5u);
  EXPECT_EQ(basePtr->GetNumberOfClasses(), 5u);
  EXPECT_EQ(gibbs->GetNumberOfClasses(), 5u);
}

TEST(RGBGibbsPriorFilter, SetGetMaximumNumberOfIterationsViaBasePointer)
{
  auto                   gibbs = GibbsFilterType::New();
  const BaseFilterType * basePtr = gibbs;
  BaseFilterType *       baseMutable = gibbs;
  baseMutable->SetMaximumNumberOfIterations(33u);
  EXPECT_EQ(basePtr->GetMaximumNumberOfIterations(), 33u);
  EXPECT_EQ(gibbs->GetMaximumNumberOfIterations(), 33u);
}

TEST(RGBGibbsPriorFilter, ModifiedTimeAdvancesOnNumberOfClassesChange)
{
  auto filter = GibbsFilterType::New();
  filter->SetNumberOfClasses(3u);
  const auto t1 = filter->GetMTime();
  filter->SetNumberOfClasses(4u);
  EXPECT_GT(filter->GetMTime(), t1);
}

TEST(RGBGibbsPriorFilter, ModifiedTimeUnchangedOnIdenticalAssignment)
{
  auto filter = GibbsFilterType::New();
  filter->SetNumberOfClasses(3u);
  const auto t1 = filter->GetMTime();
  filter->SetNumberOfClasses(3u);
  EXPECT_EQ(filter->GetMTime(), t1);
}
