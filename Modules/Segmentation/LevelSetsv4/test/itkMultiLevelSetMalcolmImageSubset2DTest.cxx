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
#include "itkMalcolmSparseLevelSetImage.h"
#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationChanAndVeseInternalTerm.h"
#include "itkLevelSetEquationChanAndVeseExternalTerm.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkLevelSetEquationContainer.h"
#include "itkLevelSetEvolution.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"
#include "itkBinaryImageToLevelSetImageAdaptor.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"
#include "itkLevelSetDomainMapImageFilter.h"
#include "itkTestingMacros.h"

int
itkMultiLevelSetMalcolmImageSubset2DTest(int, char *[])
{
  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned short;
  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using InputIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;

  using PixelType = float;
  using LevelSetType = itk::MalcolmSparseLevelSetImage<Dimension>;

  using LevelSetOutputRealType = LevelSetType::OutputRealType;

  using IdentifierType = itk::IdentifierType;
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

  using BinaryImageToLevelSetType = itk::BinaryImageToLevelSetImageAdaptor<InputImageType, LevelSetType>;

  using IdListType = std::list<IdentifierType>;
  using IdListImageType = itk::Image<IdListType, Dimension>;
  using CacheImageType = itk::Image<short, Dimension>;
  using IdIteratorType = itk::ImageRegionIteratorWithIndex<IdListImageType>;
  using DomainMapImageFilterType = itk::LevelSetDomainMapImageFilter<IdListImageType, CacheImageType>;

  // load binary input
  InputImageType::SizeType size;
  size.Fill(1000);

  InputImageType::PointType origin;
  origin[0] = 0.0;
  origin[1] = 0.0;

  InputImageType::SpacingType spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;

  InputImageType::IndexType index;
  index.Fill(0);

  InputImageType::RegionType region;
  region.SetIndex(index);
  region.SetSize(size);

  // Input initialization
  auto input = InputImageType::New();
  input->SetRegions(region);
  input->SetSpacing(spacing);
  input->SetOrigin(origin);
  input->Allocate();
  input->FillBuffer(itk::NumericTraits<InputPixelType>::ZeroValue());

  index.Fill(910);
  size.Fill(80);

  region.SetIndex(index);
  region.SetSize(size);

  InputIteratorType iIt(input, region);
  iIt.GoToBegin();
  while (!iIt.IsAtEnd())
  {
    iIt.Set(100 * itk::NumericTraits<InputPixelType>::OneValue());
    ++iIt;
  }

  index.Fill(0);
  size.Fill(100);
  origin.Fill(900.0);
  region.SetIndex(index);
  region.SetSize(size);

  // Binary initialization
  auto binary = InputImageType::New();
  binary->SetRegions(region);
  binary->SetSpacing(spacing);
  binary->SetOrigin(origin);
  binary->Allocate();
  binary->FillBuffer(itk::NumericTraits<InputPixelType>::ZeroValue());

  index.Fill(30);
  size.Fill(40);
  region.SetIndex(index);
  region.SetSize(size);

  InputIteratorType bIt(binary, region);
  bIt.GoToBegin();
  while (!bIt.IsAtEnd())
  {
    bIt.Set(itk::NumericTraits<InputPixelType>::OneValue());
    ++bIt;
  }

  // Convert binary mask to dense level set
  auto adaptor1 = BinaryImageToLevelSetType::New();
  adaptor1->SetInputImage(binary);
  adaptor1->Initialize();
  LevelSetType::Pointer levelSet1 = adaptor1->GetModifiableLevelSet();

  index = input->TransformPhysicalPointToIndex(binary->GetOrigin());
  InputImageType::OffsetType offset;
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    offset[i] = index[i];
  }
  levelSet1->SetDomainOffset(offset);

  IdListType listIds;
  listIds.clear();

  index.Fill(900);
  size.Fill(100);
  region.SetIndex(index);
  region.SetSize(size);

  auto idImage = IdListImageType::New();
  idImage->SetRegions(input->GetLargestPossibleRegion());
  idImage->Allocate();
  idImage->FillBuffer(listIds);

  listIds.push_back(1);
  IdIteratorType it(idImage, region);
  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    it.Set(listIds);
    ++it;
  }

  auto domainMapFilter = DomainMapImageFilterType::New();
  domainMapFilter->SetInput(idImage);
  domainMapFilter->Update();
  std::cout << "Domain map computed" << std::endl;

  // Define the Heaviside function
  auto heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon(1.0);

  // Insert the levelsets in a levelset container
  auto lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside(heaviside);
  lscontainer->SetDomainMapFilter(domainMapFilter);

  bool levelSetNotYetAdded = lscontainer->AddLevelSet(0, levelSet1, false);
  if (!levelSetNotYetAdded)
  {
    return EXIT_FAILURE;
  }
  std::cout << "Level set container created" << std::endl;

  // Create ChanAndVese internal term for phi_{1}
  auto cvInternalTerm0 = ChanAndVeseInternalTermType::New();
  cvInternalTerm0->SetInput(input);
  cvInternalTerm0->SetCoefficient(1.0);
  std::cout << "LevelSet 0: CV internal term created" << std::endl;

  // Create ChanAndVese external term for phi_{1}
  auto cvExternalTerm0 = ChanAndVeseExternalTermType::New();
  cvExternalTerm0->SetInput(input);
  cvExternalTerm0->SetCoefficient(1.0);
  std::cout << "LevelSet 0: CV external term created" << std::endl;


  // Create Term Container
  auto termContainer0 = TermContainerType::New();
  termContainer0->SetInput(input);
  termContainer0->SetCurrentLevelSetId(0);
  termContainer0->SetLevelSetContainer(lscontainer);

  termContainer0->AddTerm(0, cvInternalTerm0);
  termContainer0->AddTerm(1, cvExternalTerm0);
  std::cout << "Term container 0 created" << std::endl;


  auto equationContainer = EquationContainerType::New();
  equationContainer->SetLevelSetContainer(lscontainer);
  equationContainer->AddEquation(0, termContainer0);
  std::cout << "Equation container created" << std::endl;

  using StoppingCriterionType = itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion<LevelSetContainerType>;
  auto criterion = StoppingCriterionType::New();
  criterion->SetNumberOfIterations(5000);
  std::cout << "Stopping criterion created" << std::endl;

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

  PixelType mean = cvInternalTerm0->GetMean();
  if ((mean < 80) || (mean > 105))
  {
    std::cerr << "( ( mean < 95 ) || ( mean > 105 ) )" << std::endl;
    std::cerr << "mean = " << mean << std::endl;
    return EXIT_FAILURE;
  }

  mean = cvExternalTerm0->GetMean();
  if ((mean > 50.0))
  {
    std::cerr << "( ( mean < 0 ) || ( mean > 5 ) )" << std::endl;
    std::cerr << "External mean = " << mean << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
