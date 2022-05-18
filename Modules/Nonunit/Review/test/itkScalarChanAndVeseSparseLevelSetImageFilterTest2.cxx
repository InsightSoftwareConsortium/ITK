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

#include "itkScalarChanAndVeseSparseLevelSetImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"
#include "itkTestingMacros.h"

int
itkScalarChanAndVeseSparseLevelSetImageFilterTest2(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << "inputLevelSetImage inputFeatureImage ";
    std::cerr << " outputLevelSetImage" << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int nb_iteration = 50;
  double       rms = 0.;
  double       epsilon = 1.;
  double       mu = 0.;
  double       nu = 0.;
  double       l1 = 1.;
  double       l2 = 3.;

  constexpr unsigned int Dimension = 2;
  using ScalarPixelType = float;

  using LevelSetImageType = itk::Image<ScalarPixelType, Dimension>;
  using FeatureImageType = itk::Image<ScalarPixelType, Dimension>;

  using DataHelperType = itk::ScalarChanAndVeseLevelSetFunctionData<LevelSetImageType, FeatureImageType>;

  using SharedDataHelperType =
    itk::ConstrainedRegionBasedLevelSetFunctionSharedData<LevelSetImageType, FeatureImageType, DataHelperType>;

  using LevelSetFunctionType =
    itk::ScalarChanAndVeseLevelSetFunction<LevelSetImageType, FeatureImageType, SharedDataHelperType>;

  using MultiLevelSetType = itk::ScalarChanAndVeseSparseLevelSetImageFilter<LevelSetImageType,
                                                                            FeatureImageType,
                                                                            LevelSetImageType,
                                                                            LevelSetFunctionType,
                                                                            SharedDataHelperType>;

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
    levelSetFilter, ScalarChanAndVeseSparseLevelSetImageFilter, MultiphaseSparseFiniteDifferenceImageFilter);


  levelSetFilter->SetFunctionCount(1); // Protected ?
  levelSetFilter->SetFeatureImage(featureReader->GetOutput());
  levelSetFilter->SetLevelSet(0, levelSetReader1->GetOutput());
  levelSetFilter->SetNumberOfIterations(nb_iteration);
  levelSetFilter->SetMaximumRMSError(rms);
  levelSetFilter->SetUseImageSpacing(false);
  levelSetFilter->SetInPlace(false);

  levelSetFilter->GetDifferenceFunction(0)->SetDomainFunction(domainFunction);
  levelSetFilter->GetDifferenceFunction(0)->SetCurvatureWeight(mu);
  levelSetFilter->GetDifferenceFunction(0)->SetAreaWeight(nu);
  levelSetFilter->GetDifferenceFunction(0)->SetLambda1(l1);
  levelSetFilter->GetDifferenceFunction(0)->SetLambda2(l2);

  ITK_TRY_EXPECT_NO_EXCEPTION(levelSetFilter->Update());


  auto writer1 = WriterType::New();

  writer1->SetInput(levelSetFilter->GetOutput());
  writer1->UseCompressionOn();

  writer1->SetFileName(argv[3]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer1->Update());


  return EXIT_SUCCESS;
}
