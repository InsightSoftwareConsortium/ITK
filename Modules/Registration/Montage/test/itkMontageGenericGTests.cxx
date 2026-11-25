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

#include "itkPhaseCorrelationOptimizer.h"
#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkPhaseCorrelationOperator.h"
#include "itkGTest.h"
#include "itkTestDriverIncludeRequiredFactories.h"
#include "itkTileMergeImageFilter.h"
#include "itkTileMontage.h"

namespace
{
constexpr unsigned Dimension = 4;
using ImageType = itk::Image<short, Dimension>;
using PCMType = itk::PhaseCorrelationImageRegistrationMethod<ImageType, ImageType>;
using OperatorType = itk::PhaseCorrelationOperator<float, Dimension>;
using OptimizerType = itk::PhaseCorrelationOptimizer<float, Dimension>;
using MontageTypeD = itk::TileMontage<ImageType, double>;
using MontageTypeF = itk::TileMontage<ImageType, float>;
using MergingTypeD =
  itk::TileMergeImageFilter<ImageType, double, itk::LinearInterpolateImageFunction<ImageType, double>>;
using MergingTypeF = itk::TileMergeImageFilter<ImageType, float, itk::LinearInterpolateImageFunction<ImageType, float>>;
} // namespace

TEST(itkMontageGenericTests, BasicObjectMethods)
{
  RegisterRequiredFFTFactories();

  OperatorType::Pointer pcmOperator = OperatorType::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(pcmOperator, PhaseCorrelationOperator, ImageToImageFilter);
  EXPECT_THROW(pcmOperator->Update(), itk::ExceptionObject);

  OptimizerType::Pointer pcmOptimizer = OptimizerType::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(pcmOptimizer, PhaseCorrelationOptimizer, ProcessObject);
  EXPECT_THROW(pcmOptimizer->Update(), itk::ExceptionObject);

  PCMType::Pointer pcm = PCMType::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(pcm, PhaseCorrelationImageRegistrationMethod, ProcessObject);
  EXPECT_THROW(pcm->Update(), itk::ExceptionObject);

  MontageTypeD::Pointer tmD = MontageTypeD::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(tmD, TileMontage, ProcessObject);
  EXPECT_THROW(tmD->Update(), itk::ExceptionObject);

  MontageTypeF::Pointer tmF = MontageTypeF::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(tmF, TileMontage, ProcessObject);
  EXPECT_THROW(tmF->Update(), itk::ExceptionObject);

  MergingTypeD::Pointer mtD = MergingTypeD::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(mtD, TileMergeImageFilter, TileMontage);
  EXPECT_THROW(mtD->Update(), itk::ExceptionObject);

  MergingTypeF::Pointer mtF = MergingTypeF::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(mtF, TileMergeImageFilter, TileMontage);
  EXPECT_THROW(mtF->Update(), itk::ExceptionObject);
}

TEST(itkMontageGenericTests, IndexConversionsAndBooleanAccessors)
{
  using TileIndexType = typename MergingTypeF::TileIndexType;
  using SizeType = typename MergingTypeF::SizeType;

  MergingTypeF::Pointer mtF = MergingTypeF::New();

  SizeType      size = { 2, 3, 4, 5 };
  TileIndexType ind0 = { 0, 0, 0, 2 };
  TileIndexType ind1 = { 1, 2, 3, 2 };
  TileIndexType ind2 = { 0, 0, 0, 4 };
  mtF->SetMontageSize(size);
  mtF->SetTileTransform(ind0, nullptr);
  mtF->SetTileTransform(ind1, nullptr);
  mtF->SetTileTransform(ind2, nullptr);

  ITK_GTEST_SET_GET_BOOLEAN(mtF, CropToFill, true);
}
