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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationChanAndVeseExternalTerm.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkLevelSetEquationContainer.h"
#include "itkSinRegularizedHeavisideStepFunction.h"
#include "itkLevelSetEvolution.h"
#include "itkBinaryImageToLevelSetImageAdaptor.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"
#include "itkTestingMacros.h"

int
itkSingleLevelSetMalcolmImage2DTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputFilename numberOfIterations outputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned short;
  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using InputIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;
  using ReaderType = itk::ImageFileReader<InputImageType>;

  using SparseLevelSetType = itk::MalcolmSparseLevelSetImage<Dimension>;
  using BinaryToSparseAdaptorType = itk::BinaryImageToLevelSetImageAdaptor<InputImageType, SparseLevelSetType>;

  using IdentifierType = itk::IdentifierType;

  using LevelSetContainerType = itk::LevelSetContainer<IdentifierType, SparseLevelSetType>;

  using ChanAndVeseInternalTermType =
    itk::LevelSetEquationChanAndVeseInternalTerm<InputImageType, LevelSetContainerType>;
  using ChanAndVeseExternalTermType =
    itk::LevelSetEquationChanAndVeseExternalTerm<InputImageType, LevelSetContainerType>;
  using TermContainerType = itk::LevelSetEquationTermContainer<InputImageType, LevelSetContainerType>;

  using EquationContainerType = itk::LevelSetEquationContainer<TermContainerType>;

  using LevelSetEvolutionType = itk::LevelSetEvolution<EquationContainerType, SparseLevelSetType>;

  using LevelSetOutputRealType = SparseLevelSetType::OutputRealType;
  using HeavisideFunctionBaseType =
    itk::SinRegularizedHeavisideStepFunction<LevelSetOutputRealType, LevelSetOutputRealType>;
  using InputIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;

  // load input image
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();
  const InputImageType::Pointer input = reader->GetOutput();

  // Binary initialization
  auto binary = InputImageType::New();
  binary->SetRegions(input->GetLargestPossibleRegion());
  binary->CopyInformation(input);
  binary->Allocate();
  binary->FillBuffer(InputPixelType{});

  InputImageType::RegionType region;
  InputImageType::IndexType  index;
  InputImageType::SizeType   size;

  index.Fill(10);
  size.Fill(30);

  region.SetIndex(index);
  region.SetSize(size);

  InputIteratorType iIt(binary, region);
  iIt.GoToBegin();
  while (!iIt.IsAtEnd())
  {
    iIt.Set(itk::NumericTraits<InputPixelType>::OneValue());
    ++iIt;
  }

  // Convert binary mask to sparse level set
  auto adaptor = BinaryToSparseAdaptorType::New();
  adaptor->SetInputImage(binary);
  adaptor->Initialize();
  std::cout << "Finished converting to sparse format" << std::endl;

  const SparseLevelSetType::Pointer level_set = adaptor->GetModifiableLevelSet();

  // Define the Heaviside function
  auto heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon(2.0);

  // Insert the levelsets in a levelset container
  auto lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside(heaviside);

  const bool levelSetNotYetAdded = lscontainer->AddLevelSet(0, level_set, false);
  if (!levelSetNotYetAdded)
  {
    return EXIT_FAILURE;
  }
  std::cout << "Level set container created" << std::endl;

  // **************** CREATE ALL TERMS ****************

  // -----------------------------
  // *** 1st Level Set phi ***

  // Create ChanAndVese internal term for phi_{1}
  auto cvInternalTerm0 = ChanAndVeseInternalTermType::New();
  cvInternalTerm0->SetInput(input);
  cvInternalTerm0->SetCoefficient(1.0);
  std::cout << "LevelSet 1: CV internal term created" << std::endl;

  // Create ChanAndVese external term for phi_{1}
  auto cvExternalTerm0 = ChanAndVeseExternalTermType::New();
  cvExternalTerm0->SetInput(input);
  cvExternalTerm0->SetCoefficient(1.0);
  std::cout << "LevelSet 1: CV external term created" << std::endl;

  // **************** CREATE ALL EQUATIONS ****************

  // Create Term Container
  auto termContainer0 = TermContainerType::New();
  termContainer0->SetInput(input);
  termContainer0->SetCurrentLevelSetId(0);
  termContainer0->SetLevelSetContainer(lscontainer);

  termContainer0->AddTerm(0, cvInternalTerm0);
  termContainer0->AddTerm(1, cvExternalTerm0);
  std::cout << "Term container 0 created" << std::endl;

  auto equationContainer = EquationContainerType::New();
  equationContainer->AddEquation(0, termContainer0);
  equationContainer->SetLevelSetContainer(lscontainer);

  using StoppingCriterionType = itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion<LevelSetContainerType>;
  auto criterion = StoppingCriterionType::New();
  criterion->SetNumberOfIterations(std::stoi(argv[2]));

  auto evolution = LevelSetEvolutionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(evolution, LevelSetEvolution, LevelSetEvolutionBase);


  evolution->SetEquationContainer(equationContainer);
  ITK_TEST_SET_GET_VALUE(equationContainer, evolution->GetEquationContainer());

  evolution->SetStoppingCriterion(criterion);
  ITK_TEST_SET_GET_VALUE(criterion, evolution->GetStoppingCriterion());

  evolution->SetLevelSetContainer(lscontainer);

  try
  {
    evolution->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }

  using OutputImageType = itk::Image<signed char, Dimension>;
  auto outputImage = OutputImageType::New();
  outputImage->SetRegions(input->GetLargestPossibleRegion());
  outputImage->CopyInformation(input);
  outputImage->Allocate();
  outputImage->FillBuffer(0);

  using OutputIteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;
  OutputIteratorType oIt(outputImage, outputImage->GetLargestPossibleRegion());
  oIt.GoToBegin();

  while (!oIt.IsAtEnd())
  {
    const OutputImageType::IndexType idx = oIt.GetIndex();
    oIt.Set(level_set->Evaluate(idx));
    ++oIt;
  }

  using OutputWriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = OutputWriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(outputImage);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
  }

  return EXIT_SUCCESS;
}
