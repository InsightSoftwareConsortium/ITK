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
#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationChanAndVeseExternalTerm.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkLevelSetEquationContainer.h"
#include "itkSinRegularizedHeavisideStepFunction.h"
#include "itkLevelSetEvolution.h"
#include "itkBinaryImageToLevelSetImageAdaptor.h"
#include "itkLevelSetEquationCurvatureTerm.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"
#include "itkTestingMacros.h"

int
itkSingleLevelSetWhitakerImage2DWithCurvatureTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing Arguments" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned short;
  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using InputIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;
  using ReaderType = itk::ImageFileReader<InputImageType>;

  using PixelType = float;
  using OffsetType = InputImageType::OffsetType;

  using SparseLevelSetType = itk::WhitakerSparseLevelSetImage<PixelType, Dimension>;
  using BinaryToSparseAdaptorType = itk::BinaryImageToLevelSetImageAdaptor<InputImageType, SparseLevelSetType>;

  using IdentifierType = itk::IdentifierType;

  using LevelSetContainerType = itk::LevelSetContainer<IdentifierType, SparseLevelSetType>;

  using ChanAndVeseInternalTermType =
    itk::LevelSetEquationChanAndVeseInternalTerm<InputImageType, LevelSetContainerType>;
  using ChanAndVeseExternalTermType =
    itk::LevelSetEquationChanAndVeseExternalTerm<InputImageType, LevelSetContainerType>;
  using CurvatureTermType = itk::LevelSetEquationCurvatureTerm<InputImageType, LevelSetContainerType>;
  using TermContainerType = itk::LevelSetEquationTermContainer<InputImageType, LevelSetContainerType>;

  using EquationContainerType = itk::LevelSetEquationContainer<TermContainerType>;

  using LevelSetEvolutionType = itk::LevelSetEvolution<EquationContainerType, SparseLevelSetType>;

  using LevelSetOutputRealType = SparseLevelSetType::OutputRealType;
  using HeavisideFunctionBaseType =
    itk::SinRegularizedHeavisideStepFunction<LevelSetOutputRealType, LevelSetOutputRealType>;
  using InputIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;

  // load binary mask
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();
  InputImageType::Pointer input = reader->GetOutput();

  // Binary initialization
  auto binary = InputImageType::New();
  binary->SetRegions(input->GetLargestPossibleRegion());
  binary->CopyInformation(input);
  binary->Allocate();
  binary->FillBuffer(itk::NumericTraits<InputPixelType>::ZeroValue());

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

  SparseLevelSetType::Pointer level_set = adaptor->GetModifiableLevelSet();


  index = input->TransformPhysicalPointToIndex(binary->GetOrigin());
  OffsetType offset;
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    offset[i] = index[i];
  }
  level_set->SetDomainOffset(offset);

  // Define the Heaviside function
  auto heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon(1.0);

  // Insert the levelsets in a levelset container
  auto lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside(heaviside);

  bool LevelSetNotYetAdded = lscontainer->AddLevelSet(0, level_set, false);
  if (!LevelSetNotYetAdded)
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

  // Create ChanAndVese curvature term for phi_{1}
  auto curvatureTerm0 = CurvatureTermType::New();
  curvatureTerm0->SetInput(input);
  curvatureTerm0->SetCoefficient(1.0);
  std::cout << "LevelSet 1: Curvature term created" << std::endl;

  // **************** CREATE ALL EQUATIONS ****************

  // Create Term Container
  auto termContainer0 = TermContainerType::New();
  termContainer0->SetInput(input);
  termContainer0->SetCurrentLevelSetId(0);
  termContainer0->SetLevelSetContainer(lscontainer);

  termContainer0->AddTerm(0, cvInternalTerm0);
  termContainer0->AddTerm(1, cvExternalTerm0);
  termContainer0->AddTerm(2, curvatureTerm0);
  std::cout << "Term container 0 created" << std::endl;

  auto equationContainer = EquationContainerType::New();
  equationContainer->SetLevelSetContainer(lscontainer);
  equationContainer->AddEquation(0, termContainer0);

  using StoppingCriterionType = itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion<LevelSetContainerType>;
  auto criterion = StoppingCriterionType::New();

  typename StoppingCriterionType::IterationIdType numberOfIterations = 5;
  criterion->SetNumberOfIterations(numberOfIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, criterion->GetNumberOfIterations());

  auto evolution = LevelSetEvolutionType::New();

  evolution->SetEquationContainer(equationContainer);
  ITK_TEST_SET_GET_VALUE(equationContainer, evolution->GetEquationContainer());

  evolution->SetStoppingCriterion(criterion);
  ITK_TEST_SET_GET_VALUE(criterion, evolution->GetStoppingCriterion());

  evolution->SetLevelSetContainer(lscontainer);
  ITK_TEST_SET_GET_VALUE(lscontainer, evolution->GetLevelSetContainer());

  typename LevelSetEvolutionType::LevelSetOutputRealType alpha = 0.9;
  evolution->SetAlpha(alpha);
  ITK_TEST_SET_GET_VALUE(alpha, evolution->GetAlpha());

  try
  {
    evolution->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
