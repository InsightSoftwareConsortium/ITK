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

#include "itkScalarChanAndVeseDenseLevelSetImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"
#include "itkTestingMacros.h"

int
itkScalarChanAndVeseDenseLevelSetImageFilterTest2(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << "inputLevelSetImage inputFeatureImage ";
    std::cerr << " outputLevelSetImage CurvatureWeight AreaWeight";
    std::cerr << " ReinitializationWeight VolumeWeight Volume" << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int nb_iteration = 500;
  double       rms = 0.;
  double       epsilon = 1.;
  double       curvature_weight = std::stod(argv[4]);
  double       area_weight = std::stod(argv[5]);
  double       reinitialization_weight = std::stod(argv[6]);
  double       volume_weight = std::stod(argv[7]);
  double       volume = std::stod(argv[8]);
  double       l1 = 1.;
  double       l2 = 1.;

  constexpr unsigned int Dimension = 2;
  using ScalarPixelType = float;

  using LevelSetImageType = itk::Image<ScalarPixelType, Dimension>;
  using FeatureImageType = itk::Image<ScalarPixelType, Dimension>;

  using MultiLevelSetType =
    itk::ScalarChanAndVeseDenseLevelSetImageFilter<LevelSetImageType, FeatureImageType, LevelSetImageType>;

  using LevelSetReaderType = itk::ImageFileReader<LevelSetImageType>;
  using FeatureReaderType = itk::ImageFileReader<FeatureImageType>;
  using WriterType = itk::ImageFileWriter<LevelSetImageType>;

  using DomainFunctionType = itk::AtanRegularizedHeavisideStepFunction<ScalarPixelType, ScalarPixelType>;

  auto domainFunction = DomainFunctionType::New();

  domainFunction->SetEpsilon(epsilon);

  auto levelSetReader1 = LevelSetReaderType::New();
  levelSetReader1->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(levelSetReader1->Update());


  auto featureReader = FeatureReaderType::New();
  featureReader->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(featureReader->Update());


  auto levelSetFilter = MultiLevelSetType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    levelSetFilter, ScalarChanAndVeseDenseLevelSetImageFilter, MultiphaseDenseFiniteDifferenceImageFilter);


  levelSetFilter->SetFunctionCount(1); // Protected ?
  levelSetFilter->SetFeatureImage(featureReader->GetOutput());
  levelSetFilter->SetLevelSet(0, levelSetReader1->GetOutput());
  levelSetFilter->SetNumberOfIterations(nb_iteration);
  levelSetFilter->SetMaximumRMSError(rms);
  levelSetFilter->SetUseImageSpacing(false);
  levelSetFilter->SetInPlace(false);

  MultiLevelSetType::FunctionType * function = levelSetFilter->GetDifferenceFunction(0);
  function->SetDomainFunction(domainFunction);
  function->SetCurvatureWeight(curvature_weight);
  function->SetAreaWeight(area_weight);
  function->SetReinitializationSmoothingWeight(reinitialization_weight);
  function->SetVolumeMatchingWeight(volume_weight);
  function->SetVolume(volume);
  function->SetLambda1(l1);
  function->SetLambda2(l2);

  ITK_TRY_EXPECT_NO_EXCEPTION(levelSetFilter->Update());


  auto writer1 = WriterType::New();

  writer1->SetInput(levelSetFilter->GetOutput());
  writer1->UseCompressionOn();

  writer1->SetFileName(argv[3]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer1->Update());


  return EXIT_SUCCESS;
}
