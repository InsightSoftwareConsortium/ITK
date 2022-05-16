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
#include "itkFastMarchingImageFilter.h"
#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationChanAndVeseExternalTerm.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkLevelSetEquationContainer.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"
#include "itkLevelSetEvolution.h"
#include "itkTestingMacros.h"

int
itkTwoLevelSetDenseImage2DTest(int argc, char * argv[])
{
  if (argc != 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv)
              << " inputFilename seedPosition0 seedPosition1 initialDistance outputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned short;
  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<InputImageType>;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;
  using LevelSetType = itk::LevelSetDenseImage<ImageType>;
  using LevelSetOutputRealType = LevelSetType::OutputRealType;

  using IdentifierType = itk::IdentifierType;
  using IdListType = std::list<IdentifierType>;
  using IdListImageType = itk::Image<IdListType, Dimension>;
  using CacheImageType = itk::Image<short, Dimension>;
  using DomainMapImageFilterType = itk::LevelSetDomainMapImageFilter<IdListImageType, CacheImageType>;

  using LevelSetContainerType = itk::LevelSetContainer<IdentifierType, LevelSetType>;
  using ChanAndVeseInternalTermType =
    itk::LevelSetEquationChanAndVeseInternalTerm<InputImageType, LevelSetContainerType>;
  using ChanAndVeseExternalTermType =
    itk::LevelSetEquationChanAndVeseExternalTerm<InputImageType, LevelSetContainerType>;
  using TermContainerType = itk::LevelSetEquationTermContainer<InputImageType, LevelSetContainerType>;

  using EquationContainerType = itk::LevelSetEquationContainer<TermContainerType>;

  using LevelSetEvolutionType = itk::LevelSetEvolution<EquationContainerType, LevelSetType>;

  using HeavisideFunctionBaseType =
    itk::AtanRegularizedHeavisideStepFunction<LevelSetOutputRealType, LevelSetOutputRealType>;

  using FastMarchingFilterType = itk::FastMarchingImageFilter<ImageType, ImageType>;
  using NodeContainer = FastMarchingFilterType::NodeContainer;
  using NodeType = FastMarchingFilterType::NodeType;

  // Read the image to be segmented
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();
  InputImageType::Pointer input = reader->GetOutput();

  auto fastMarching = FastMarchingFilterType::New();

  auto seeds = NodeContainer::New();

  ImageType::IndexType seedPosition;
  seedPosition[0] = std::stoi(argv[2]);
  seedPosition[1] = std::stoi(argv[3]);

  const double initialDistance = std::stod(argv[4]);
  const double seedValue = -initialDistance;

  NodeType node;
  node.SetValue(seedValue);
  node.SetIndex(seedPosition);

  //  The list of nodes is initialized and then every node is inserted using
  //  the \code{InsertElement()}.
  //
  seeds->Initialize();
  seeds->InsertElement(0, node);

  //  The set of seed nodes is passed now to the
  //  FastMarchingImageFilter with the method
  //  \code{SetTrialPoints()}.
  //
  fastMarching->SetTrialPoints(seeds);

  //  Since the FastMarchingImageFilter is used here just as a
  //  Distance Map generator. It does not require a speed image as input.
  //  Instead the constant value $1.0$ is passed using the
  //  \code{SetSpeedConstant()} method.
  //
  fastMarching->SetSpeedConstant(1.0);

  //  The FastMarchingImageFilter requires the user to specify the
  //  size of the image to be produced as output. This is done using the
  //  \code{SetOutputSize()}. Note that the size is obtained here from the
  //  output image of the smoothing filter. The size of this image is valid
  //  only after the \code{Update()} methods of this filter has been called
  //  directly or indirectly.
  //
  InputImageType::RegionType inputBufferedRegion = input->GetBufferedRegion();
  InputImageType::SizeType   inputBufferedRegionSize = inputBufferedRegion.GetSize();

  fastMarching->SetOutputSize(inputBufferedRegionSize);
  fastMarching->Update();

  IdListType list_ids;
  list_ids.push_back(1);
  list_ids.push_back(2);

  auto id_image = IdListImageType::New();
  id_image->SetRegions(input->GetLargestPossibleRegion());
  id_image->Allocate();
  id_image->FillBuffer(list_ids);

  auto domainMapFilter = DomainMapImageFilterType::New();
  domainMapFilter->SetInput(id_image);
  domainMapFilter->Update();
  std::cout << "Domain map computed" << std::endl;

  // Define the Heaviside function
  auto heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon(1.0);

  // Map of levelset bases
  auto level_set1 = LevelSetType::New();
  level_set1->SetImage(fastMarching->GetOutput());

  auto level_set2 = LevelSetType::New();
  level_set2->SetImage(fastMarching->GetOutput());

  // Insert the levelsets in a levelset container
  auto lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside(heaviside);
  lscontainer->SetDomainMapFilter(domainMapFilter);

  bool LevelSetNotYetAdded = lscontainer->AddLevelSet(0, level_set1, false);
  if (!LevelSetNotYetAdded)
  {
    return EXIT_FAILURE;
  }

  LevelSetNotYetAdded = lscontainer->AddLevelSet(1, level_set2, false);
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

  // -----------------------------
  // *** 2nd Level Set phi ***

  auto cvInternalTerm1 = ChanAndVeseInternalTermType::New();
  cvInternalTerm1->SetInput(input);
  cvInternalTerm1->SetCoefficient(1.0);
  std::cout << "LevelSet 2: CV internal term created" << std::endl;

  // Create ChanAndVese external term for phi_{1}
  auto cvExternalTerm1 = ChanAndVeseExternalTermType::New();
  cvExternalTerm1->SetInput(input);
  cvExternalTerm1->SetCoefficient(1.0);
  std::cout << "LevelSet 2: CV external term created" << std::endl;

  // **************** CREATE ALL EQUATIONS ****************

  // Create Term Container
  auto termContainer0 = TermContainerType::New();
  termContainer0->SetInput(input);
  termContainer0->SetCurrentLevelSetId(0);
  termContainer0->SetLevelSetContainer(lscontainer);

  termContainer0->AddTerm(0, cvInternalTerm0);
  termContainer0->AddTerm(1, cvExternalTerm0);
  std::cout << "Term container 0 created" << std::endl;

  // Create Term Container
  auto termContainer1 = TermContainerType::New();
  termContainer1->SetInput(input);
  termContainer1->SetCurrentLevelSetId(1);
  termContainer1->SetLevelSetContainer(lscontainer);

  termContainer1->AddTerm(0, cvInternalTerm1);
  termContainer1->AddTerm(1, cvExternalTerm1);
  std::cout << "Term container 1 created" << std::endl;

  auto equationContainer = EquationContainerType::New();
  equationContainer->AddEquation(0, termContainer0);
  equationContainer->AddEquation(1, termContainer1);
  equationContainer->SetLevelSetContainer(lscontainer);

  using StoppingCriterionType = itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion<LevelSetContainerType>;
  auto criterion = StoppingCriterionType::New();
  criterion->SetNumberOfIterations(10);

  auto evolution = LevelSetEvolutionType::New();
  evolution->SetEquationContainer(equationContainer);
  evolution->SetStoppingCriterion(criterion);
  evolution->SetLevelSetContainer(lscontainer);

  try
  {
    evolution->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
