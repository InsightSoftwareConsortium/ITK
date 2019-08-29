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

#include "itkMath.h"
#include "itkNumericTraits.h"
#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationChanAndVeseInternalTerm.h"
#include "itkSinRegularizedHeavisideStepFunction.h"
#include "itkBinaryImageToLevelSetImageAdaptor.h"
#include "itkTestingMacros.h"

int
itkLevelSetEquationChanAndVeseInternalTermTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Program " << itkNameOfTestExecutableMacro(argv) << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned short;
  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using IdentifierType = itk::IdentifierType;

  using InputPixelType = unsigned short;
  using InputImageType = itk::Image<InputPixelType, Dimension>;

  using PixelType = float;
  using SparseLevelSetType = itk::WhitakerSparseLevelSetImage<PixelType, Dimension>;
  using BinaryToSparseAdaptorType = itk::BinaryImageToLevelSetImageAdaptor<InputImageType, SparseLevelSetType>;

  using LevelSetContainerType = itk::LevelSetContainer<IdentifierType, SparseLevelSetType>;
  using ChanAndVeseInternalTermType =
    itk::LevelSetEquationChanAndVeseInternalTerm<InputImageType, LevelSetContainerType>;

  using IdListType = std::list<IdentifierType>;
  using IdListImageType = itk::Image<IdListType, Dimension>;
  using CacheImageType = itk::Image<short, Dimension>;
  using DomainMapImageFilterType = itk::LevelSetDomainMapImageFilter<IdListImageType, CacheImageType>;

  using ChanAndVeseInternalTermType =
    itk::LevelSetEquationChanAndVeseInternalTerm<InputImageType, LevelSetContainerType>;

  using LevelSetOutputRealType = SparseLevelSetType::OutputRealType;
  using HeavisideFunctionBaseType =
    itk::SinRegularizedHeavisideStepFunction<LevelSetOutputRealType, LevelSetOutputRealType>;
  using InputImageIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;

  // load binary mask
  InputImageType::SizeType size;
  size.Fill(50);

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

  // Binary initialization
  InputImageType::Pointer binary = InputImageType::New();
  binary->SetRegions(region);
  binary->SetSpacing(spacing);
  binary->SetOrigin(origin);
  binary->Allocate();
  binary->FillBuffer(itk::NumericTraits<InputPixelType>::ZeroValue());

  index.Fill(10);
  size.Fill(30);

  region.SetIndex(index);
  region.SetSize(size);

  InputImageIteratorType iIt(binary, region);
  iIt.GoToBegin();
  while (!iIt.IsAtEnd())
  {
    iIt.Set(itk::NumericTraits<InputPixelType>::OneValue());
    ++iIt;
  }

  // Convert binary mask to sparse level set
  BinaryToSparseAdaptorType::Pointer adaptor = BinaryToSparseAdaptorType::New();
  adaptor->SetInputImage(binary);
  adaptor->Initialize();
  std::cout << "Finished converting to sparse format" << std::endl;

  SparseLevelSetType::Pointer level_set = adaptor->GetModifiableLevelSet();

  IdListType list_ids;
  list_ids.push_back(1);

  IdListImageType::Pointer id_image = IdListImageType::New();
  id_image->SetRegions(binary->GetLargestPossibleRegion());
  id_image->Allocate();
  id_image->FillBuffer(list_ids);

  DomainMapImageFilterType::Pointer domainMapFilter = DomainMapImageFilterType::New();
  domainMapFilter->SetInput(id_image);
  domainMapFilter->Update();
  std::cout << "Domain map computed" << std::endl;

  // Define the Heaviside function
  HeavisideFunctionBaseType::Pointer heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon(1.0);

  // Insert the levelsets in a levelset container
  LevelSetContainerType::Pointer lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside(heaviside);
  lscontainer->SetDomainMapFilter(domainMapFilter);

  bool LevelSetNotYetAdded = lscontainer->AddLevelSet(0, level_set, false);
  if (!LevelSetNotYetAdded)
  {
    return EXIT_FAILURE;
  }

  // Create ChanAndVese internal term for phi_{1}
  ChanAndVeseInternalTermType::Pointer cvInternalTerm0 = ChanAndVeseInternalTermType::New();
  cvInternalTerm0->SetInput(binary);
  cvInternalTerm0->SetCoefficient(1.0);
  cvInternalTerm0->SetCurrentLevelSetId(0);
  cvInternalTerm0->SetLevelSetContainer(lscontainer);
  std::cout << "CV internal term created" << std::endl;

  // Initialize the ChanAndVese term here
  cvInternalTerm0->InitializeParameters();
  InputImageIteratorType it(binary, binary->GetLargestPossibleRegion());
  it.GoToBegin();

  while (!it.IsAtEnd())
  {
    cvInternalTerm0->Initialize(it.GetIndex());
    ++it;
  }

  cvInternalTerm0->Update();

  if (itk::Math::NotAlmostEquals(cvInternalTerm0->GetMean(),
                                 itk::NumericTraits<ChanAndVeseInternalTermType::InputPixelRealType>::OneValue()))
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
