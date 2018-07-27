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

#include "itkTileMontage.h"
#include "itkTileMergeImageFilter.h"
#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkPhaseCorrelationOperator.h"
#include "itkTestingMacros.h"
#include <iostream>

int itkMontageGenericTests( int, char** const )
{
  constexpr unsigned Dimension = 4;
  using ImageType = itk::Image<short, Dimension>;
  using PCMType = itk::PhaseCorrelationImageRegistrationMethod<ImageType, ImageType>;
  using OperatorType = itk::PhaseCorrelationOperator<float, Dimension>;
  using OptimizerType = itk::MaxPhaseCorrelationOptimizer<PCMType>;
  using MontageTypeD = itk::TileMontage<ImageType, double>;
  using MontageTypeF = itk::TileMontage<ImageType, float>;
  using MergingTypeD = itk::TileMergeImageFilter<ImageType, double, itk::LinearInterpolateImageFunction<ImageType, double> >;
  using MergingTypeF = itk::TileMergeImageFilter<ImageType, float, itk::LinearInterpolateImageFunction<ImageType, float> >;
  using TileIndexType = typename MergingTypeF::TileIndexType;
  using SizeType = typename MergingTypeF::SizeType;

  OperatorType::Pointer pcmOperator = OperatorType::New();
  EXERCISE_BASIC_OBJECT_METHODS(pcmOperator, PhaseCorrelationOperator, ImageToImageFilter);
  TRY_EXPECT_EXCEPTION(pcmOperator->Update()); //inputs not set!
  OptimizerType::Pointer pcmOptimizer = OptimizerType::New();
  EXERCISE_BASIC_OBJECT_METHODS(pcmOptimizer, MaxPhaseCorrelationOptimizer, PhaseCorrelationOptimizer);
  TRY_EXPECT_EXCEPTION(pcmOptimizer->Update()); //inputs not set!
  PCMType::Pointer pcm = PCMType::New();
  EXERCISE_BASIC_OBJECT_METHODS(pcm, PhaseCorrelationImageRegistrationMethod, ProcessObject);
  TRY_EXPECT_EXCEPTION(pcm->Update()); //inputs not set!
  MontageTypeD::Pointer tmD = MontageTypeD::New();
  EXERCISE_BASIC_OBJECT_METHODS(tmD, TileMontage, ProcessObject);
  TRY_EXPECT_EXCEPTION(tmD->Update()); //inputs not set!
  MontageTypeF::Pointer tmF = MontageTypeF::New();
  EXERCISE_BASIC_OBJECT_METHODS(tmF, TileMontage, ProcessObject);
  TRY_EXPECT_EXCEPTION(tmF->Update()); //inputs not set!
  MergingTypeD::Pointer mtD = MergingTypeD::New();
  EXERCISE_BASIC_OBJECT_METHODS(mtD, TileMergeImageFilter, TileMontage);
  TRY_EXPECT_EXCEPTION(mtD->Update()); //inputs not set!
  MergingTypeF::Pointer mtF = MergingTypeF::New();
  EXERCISE_BASIC_OBJECT_METHODS(mtF, TileMergeImageFilter, TileMontage);
  TRY_EXPECT_EXCEPTION(mtF->Update()); //inputs not set!

  //exercise nD index conversions
  SizeType size = { 2,3,4,5 };
  TileIndexType ind0 = { 0,0,0,2 };
  TileIndexType ind1 = { 1,2,3,2 };
  TileIndexType ind2 = { 0,0,0,4 };
  mtF->SetMontageSize(size);
  mtF->SetTileTransform(ind0, nullptr);
  mtF->SetTileTransform(ind1, nullptr);
  mtF->SetTileTransform(ind2, nullptr);
  TEST_SET_GET_BOOLEAN(mtF, CropToFill, true);

  return EXIT_SUCCESS;
}
