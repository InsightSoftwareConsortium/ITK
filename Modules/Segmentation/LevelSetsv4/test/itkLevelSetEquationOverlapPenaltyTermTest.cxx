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
#include "itkLevelSetEquationOverlapPenaltyTerm.h"
#include "itkSinRegularizedHeavisideStepFunction.h"
#include "itkBinaryImageToLevelSetImageAdaptor.h"

int itkLevelSetEquationOverlapPenaltyTermTest( int argc, char* argv[] )
{

  if( argc < 1 )
    {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Program " << argv[0] << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned short                                    InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >           InputImageType;
  typedef itk::IdentifierType                               IdentifierType;

  typedef unsigned short                                    InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >           InputImageType;

  typedef float                                             PixelType;
  typedef itk::WhitakerSparseLevelSetImage< PixelType, Dimension >
                                                            SparseLevelSetType;
  typedef itk::BinaryImageToLevelSetImageAdaptor< InputImageType, SparseLevelSetType >
                                                            BinaryToSparseAdaptorType;

  typedef itk::LevelSetContainer< IdentifierType, SparseLevelSetType >  LevelSetContainerType;
  typedef itk::LevelSetEquationOverlapPenaltyTerm< InputImageType, LevelSetContainerType >
                                                                        OverlapPenaltyTermType;

  typedef std::list< IdentifierType >                       IdListType;
  typedef itk::Image< IdListType, Dimension >               IdListImageType;
  typedef itk::Image< short, Dimension >                    CacheImageType;
  typedef itk::LevelSetDomainMapImageFilter< IdListImageType, CacheImageType >
                                                            DomainMapImageFilterType;

  typedef SparseLevelSetType::OutputRealType                  LevelSetOutputRealType;
  typedef itk::SinRegularizedHeavisideStepFunction< LevelSetOutputRealType, LevelSetOutputRealType >
                                                            HeavisideFunctionBaseType;
  typedef itk::ImageRegionIteratorWithIndex< InputImageType > InputImageIteratorType;

  // load binary mask
  InputImageType::SizeType size;
  size.Fill( 50 );

  InputImageType::PointType origin;
  origin[0] = 0.0;
  origin[1] = 0.0;

  InputImageType::SpacingType spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;

  InputImageType::IndexType index;
  index.Fill( 0 );

  InputImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize( size );

  // Binary initialization
  InputImageType::Pointer binary = InputImageType::New();
  binary->SetRegions( region );
  binary->SetSpacing( spacing );
  binary->SetOrigin( origin );
  binary->Allocate();
  binary->FillBuffer( itk::NumericTraits<InputPixelType>::ZeroValue() );

  index.Fill( 10 );
  size.Fill( 30 );

  region.SetIndex( index );
  region.SetSize( size );

  InputImageIteratorType iIt( binary, region );
  iIt.GoToBegin();
  while( !iIt.IsAtEnd() )
    {
    iIt.Set( itk::NumericTraits<InputPixelType>::OneValue() );
    ++iIt;
    }

  // Convert binary mask to sparse level set
  BinaryToSparseAdaptorType::Pointer adaptor1 = BinaryToSparseAdaptorType::New();
  adaptor1->SetInputImage( binary );
  adaptor1->Initialize();
  std::cout << "Finished converting levelset1 to sparse format" << std::endl;

  // Convert binary mask to sparse level set
  BinaryToSparseAdaptorType::Pointer adaptor2 = BinaryToSparseAdaptorType::New();
  adaptor2->SetInputImage( binary );
  adaptor2->Initialize();
  std::cout << "Finished converting levelset2 to sparse format" << std::endl;

  SparseLevelSetType::Pointer level_set1 = adaptor1->GetModifiableLevelSet();
  SparseLevelSetType::Pointer level_set2 = adaptor2->GetModifiableLevelSet();

  IdListType list_ids;
  list_ids.push_back( 1 );
  list_ids.push_back( 2 );

  IdListImageType::Pointer id_image = IdListImageType::New();
  id_image->SetRegions( binary->GetLargestPossibleRegion() );
  id_image->Allocate();
  id_image->FillBuffer( list_ids );

  DomainMapImageFilterType::Pointer domainMapFilter = DomainMapImageFilterType::New();
  domainMapFilter->SetInput( id_image );
  domainMapFilter->Update();
  std::cout << "Domain map computed" << std::endl;

  // Define the Heaviside function
  HeavisideFunctionBaseType::Pointer heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon( 2.0 );

  // Insert the levelsets in a levelset container
  LevelSetContainerType::Pointer lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside( heaviside );
  lscontainer->SetDomainMapFilter( domainMapFilter );

  bool LevelSetNotYetAdded = lscontainer->AddLevelSet( 0, level_set1, false );
  if ( !LevelSetNotYetAdded )
    {
    return EXIT_FAILURE;
    }

  LevelSetNotYetAdded = lscontainer->AddLevelSet( 1, level_set2, false );
  if ( !LevelSetNotYetAdded )
    {
    return EXIT_FAILURE;
    }


  // Create overlap penalty term
  OverlapPenaltyTermType::Pointer penaltyTerm0 = OverlapPenaltyTermType::New();
  penaltyTerm0->SetInput( binary );
  penaltyTerm0->SetCoefficient( 1000.0 );
  penaltyTerm0->SetCurrentLevelSetId( 0 );
  penaltyTerm0->SetLevelSetContainer( lscontainer );

  OverlapPenaltyTermType::Pointer penaltyTerm1 = OverlapPenaltyTermType::New();
  penaltyTerm1->SetInput( binary );
  penaltyTerm1->SetCoefficient( 1000.0 );
  penaltyTerm1->SetCurrentLevelSetId( 1 );
  penaltyTerm1->SetLevelSetContainer( lscontainer );

  std::cout << "Penalty terms created" << std::endl;

  // Initialize the ChanAndVese term here
  penaltyTerm0->InitializeParameters();
  penaltyTerm1->InitializeParameters();

  InputImageIteratorType it( binary, binary->GetLargestPossibleRegion() );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    penaltyTerm0->Initialize( it.GetIndex() );
    penaltyTerm1->Initialize( it.GetIndex() );
    ++it;
    }

  penaltyTerm0->Update();
  penaltyTerm1->Update();

  index[0] = 5;
  index[1] = 5;

  std::cout << penaltyTerm0->Evaluate( index ) << std::endl;
  if ( itk::Math::NotAlmostEquals( penaltyTerm0->Evaluate( index ),
       itk::NumericTraits< OverlapPenaltyTermType::LevelSetOutputRealType >::ZeroValue() ) )
    {
    return EXIT_FAILURE;
    }

  index[0] = 20;
  index[1] = 20;

  std::cout << penaltyTerm0->Evaluate( index ) << std::endl;
  if ( itk::Math::NotAlmostEquals( penaltyTerm0->Evaluate( index ), 1000 ) )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
