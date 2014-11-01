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

#include "itkImageFileReader.h"
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

int itkMultiLevelSetDenseImageSubset2DTest( int argc, char* argv[] )
{
  if( argc < 1 )
    {
    std::cerr << "Missing Arguments: " << argv[0] << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned short                                      InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >             InputImageType;
  typedef itk::ImageRegionIteratorWithIndex< InputImageType > InputIteratorType;

  typedef float                                          PixelType;
  typedef itk::Image< PixelType, Dimension >             ImageType;
  typedef itk::LevelSetDenseImage< ImageType >           LevelSetType;
  typedef LevelSetType::OutputRealType                   LevelSetOutputRealType;

  typedef itk::IdentifierType                                     IdentifierType;
  typedef itk::LevelSetContainer< IdentifierType, LevelSetType >  LevelSetContainerType;

  typedef itk::LevelSetEquationChanAndVeseInternalTerm< InputImageType, LevelSetContainerType >
                                                                      ChanAndVeseInternalTermType;
  typedef itk::LevelSetEquationChanAndVeseExternalTerm< InputImageType, LevelSetContainerType >
                                                                      ChanAndVeseExternalTermType;
  typedef itk::LevelSetEquationTermContainer< InputImageType, LevelSetContainerType >
                                                                      TermContainerType;

  typedef itk::LevelSetEquationContainer< TermContainerType >           EquationContainerType;
  typedef itk::LevelSetEvolution< EquationContainerType, LevelSetType > LevelSetEvolutionType;

  typedef itk::AtanRegularizedHeavisideStepFunction<
      LevelSetOutputRealType, LevelSetOutputRealType >          HeavisideFunctionBaseType;

  typedef itk::BinaryImageToLevelSetImageAdaptor< InputImageType, LevelSetType > BinaryImageToLevelSetType;

  typedef std::list< IdentifierType >                          IdListType;
  typedef itk::Image< IdListType, Dimension >                  IdListImageType;
  typedef itk::Image< short, Dimension >                       CacheImageType;
  typedef itk::ImageRegionIteratorWithIndex< IdListImageType > IdIteratorType;
  typedef itk::LevelSetDomainMapImageFilter< IdListImageType, CacheImageType >
                                                               DomainMapImageFilterType;

  // load binary input
  InputImageType::SizeType size;
  size.Fill( 1000 );

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

  // Input initialization
  InputImageType::Pointer input = InputImageType::New();
  input->SetRegions( region );
  input->SetSpacing( spacing );
  input->SetOrigin( origin );
  input->Allocate();
  input->FillBuffer( itk::NumericTraits<InputPixelType>::ZeroValue() );

  index.Fill( 910 );
  size.Fill( 80 );

  region.SetIndex( index );
  region.SetSize( size );

  InputIteratorType iIt( input, region );
  iIt.GoToBegin();
  while( !iIt.IsAtEnd() )
    {
    iIt.Set( 100*itk::NumericTraits<InputPixelType>::OneValue() );
    ++iIt;
    }

  index.Fill( 0 );
  size.Fill( 100 );
  origin.Fill( 900.0 );
  region.SetIndex( index );
  region.SetSize( size );

  // Binary initialization
  InputImageType::Pointer binary = InputImageType::New();
  binary->SetRegions( region );
  binary->SetSpacing( spacing );
  binary->SetOrigin( origin );
  binary->Allocate();
  binary->FillBuffer( itk::NumericTraits<InputPixelType>::ZeroValue() );

  index.Fill( 30 );
  size.Fill( 40 );
  region.SetIndex( index );
  region.SetSize( size );

  InputIteratorType bIt( binary, region );
  bIt.GoToBegin();
  while( !bIt.IsAtEnd() )
    {
    bIt.Set( itk::NumericTraits<InputPixelType>::OneValue() );
    ++bIt;
    }

  // Convert binary mask to dense level set
  BinaryImageToLevelSetType::Pointer adaptor1 = BinaryImageToLevelSetType::New();
  adaptor1->SetInputImage( binary );
  adaptor1->Initialize();
  LevelSetType::Pointer levelSet1 = adaptor1->GetModifiableLevelSet();

  BinaryImageToLevelSetType::Pointer adaptor2 = BinaryImageToLevelSetType::New();
  adaptor2->SetInputImage( binary );
  adaptor2->Initialize();
  LevelSetType::Pointer levelSet2 = adaptor2->GetModifiableLevelSet();

  BinaryImageToLevelSetType::Pointer adaptor3 = BinaryImageToLevelSetType::New();
  adaptor3->SetInputImage( binary );
  adaptor3->Initialize();
  LevelSetType::Pointer levelSet3 = adaptor3->GetModifiableLevelSet();

  input->TransformPhysicalPointToIndex( binary->GetOrigin(), index );
  InputImageType::OffsetType offset;
  for( unsigned int i = 0; i < Dimension; i++ )
    {
    offset[i] = index[i];
    }
  levelSet1->SetDomainOffset( offset );
  levelSet2->SetDomainOffset( offset );
  levelSet3->SetDomainOffset( offset );

  IdListType listIds;
  listIds.clear();

  index.Fill( 900 );
  size.Fill( 100 );
  region.SetIndex( index );
  region.SetSize( size );

  IdListImageType::Pointer idImage = IdListImageType::New();
  idImage->SetRegions( input->GetLargestPossibleRegion() );
  idImage->Allocate();
  idImage->FillBuffer( listIds );

  listIds.push_back( 1 );
  listIds.push_back( 2 );
  listIds.push_back( 3 );
  IdIteratorType it( idImage, region );
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( listIds );
    ++it;
    }

  DomainMapImageFilterType::Pointer domainMapFilter = DomainMapImageFilterType::New();
  domainMapFilter->SetInput( idImage );
  domainMapFilter->Update();
  std::cout << "Domain map computed" << std::endl;

  // Define the Heaviside function
  HeavisideFunctionBaseType::Pointer heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon( 1.0 );

  // Insert the levelsets in a levelset container
  LevelSetContainerType::Pointer lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside( heaviside );
  lscontainer->SetDomainMapFilter( domainMapFilter );

  bool levelSetNotYetAdded = lscontainer->AddLevelSet( 0, levelSet1, false );
  if ( !levelSetNotYetAdded )
    {
    return EXIT_FAILURE;
    }
  levelSetNotYetAdded = lscontainer->AddLevelSet( 1, levelSet2, false );
    if ( !levelSetNotYetAdded )
      {
      return EXIT_FAILURE;
      }
  levelSetNotYetAdded = lscontainer->AddLevelSet( 2, levelSet3, false );
    if ( !levelSetNotYetAdded )
      {
      return EXIT_FAILURE;
      }
  std::cout << "Level set container created" << std::endl;

  // Create ChanAndVese internal term for phi_{1}
  ChanAndVeseInternalTermType::Pointer cvInternalTerm0 = ChanAndVeseInternalTermType::New();
  cvInternalTerm0->SetInput( input );
  cvInternalTerm0->SetCoefficient( 1.0 );
  std::cout << "LevelSet 0: CV internal term created" << std::endl;

  // Create ChanAndVese external term for phi_{1}
  ChanAndVeseExternalTermType::Pointer cvExternalTerm0 = ChanAndVeseExternalTermType::New();
  cvExternalTerm0->SetInput( input );
  cvExternalTerm0->SetCoefficient( 1.0 );
  std::cout << "LevelSet 0: CV external term created" << std::endl;


  // Create ChanAndVese internal term for phi_{2}
  ChanAndVeseInternalTermType::Pointer cvInternalTerm1 = ChanAndVeseInternalTermType::New();
  cvInternalTerm1->SetInput( input );
  cvInternalTerm1->SetCoefficient( 1.0 );
  std::cout << "LevelSet 1: CV internal term created" << std::endl;

  // Create ChanAndVese external term for phi_{2}
  ChanAndVeseExternalTermType::Pointer cvExternalTerm1 = ChanAndVeseExternalTermType::New();
  cvExternalTerm1->SetInput( input );
  cvExternalTerm1->SetCoefficient( 1.0 );
  std::cout << "LevelSet 1: CV external term created" << std::endl;


  // Create ChanAndVese internal term for phi_{3}
  ChanAndVeseInternalTermType::Pointer cvInternalTerm2 = ChanAndVeseInternalTermType::New();
  cvInternalTerm2->SetInput( input );
  cvInternalTerm2->SetCoefficient( 1.0 );
  std::cout << "LevelSet 2: CV internal term created" << std::endl;

  // Create ChanAndVese external term for phi_{3}
  ChanAndVeseExternalTermType::Pointer cvExternalTerm2 = ChanAndVeseExternalTermType::New();
  cvExternalTerm2->SetInput( input );
  cvExternalTerm2->SetCoefficient( 1.0 );
  std::cout << "LevelSet 2: CV external term created" << std::endl;


  // Create Term Container
  TermContainerType::Pointer termContainer0 = TermContainerType::New();
  termContainer0->SetInput( input );
  termContainer0->SetCurrentLevelSetId( 0 );
  termContainer0->SetLevelSetContainer( lscontainer );

  termContainer0->AddTerm( 0, cvInternalTerm0 );
  termContainer0->AddTerm( 1, cvExternalTerm0 );
  std::cout << "Term container 0 created" << std::endl;

  TermContainerType::Pointer termContainer1 = TermContainerType::New();
  termContainer1->SetInput( input );
  termContainer1->SetCurrentLevelSetId( 1 );
  termContainer1->SetLevelSetContainer( lscontainer );

  termContainer1->AddTerm( 0, cvInternalTerm1 );
  termContainer1->AddTerm( 1, cvExternalTerm1 );
  std::cout << "Term container 1 created" << std::endl;

  TermContainerType::Pointer termContainer2 = TermContainerType::New();
  termContainer2->SetInput( input );
  termContainer2->SetCurrentLevelSetId( 2 );
  termContainer2->SetLevelSetContainer( lscontainer );

  termContainer2->AddTerm( 0, cvInternalTerm2 );
  termContainer2->AddTerm( 1, cvExternalTerm2 );
  std::cout << "Term container 2 created" << std::endl;

  EquationContainerType::Pointer equationContainer = EquationContainerType::New();
  equationContainer->SetLevelSetContainer( lscontainer );
  equationContainer->AddEquation( 0, termContainer0 );
  equationContainer->AddEquation( 1, termContainer1 );
  equationContainer->AddEquation( 2, termContainer2 );
  std::cout << "Equation container created" << std::endl;

  typedef itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion< LevelSetContainerType >
      StoppingCriterionType;
  StoppingCriterionType::Pointer criterion = StoppingCriterionType::New();
  criterion->SetNumberOfIterations( 10 );
  std::cout << "Stopping criterion created" << std::endl;

  LevelSetEvolutionType::Pointer evolution = LevelSetEvolutionType::New();
  evolution->SetEquationContainer( equationContainer );
  evolution->SetStoppingCriterion( criterion );
  evolution->SetLevelSetContainer( lscontainer );
  evolution->SetNumberOfThreads( 1 );

  try
    {
    evolution->Update();
    }
  catch ( itk::ExceptionObject& err )
    {
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  PixelType mean = cvInternalTerm0->GetMean();
  if ( ( mean < 95 ) || ( mean > 105 ) )
    {
    std::cerr << "( ( mean < 95 ) || ( mean > 105 ) )" <<std::endl;
    std::cerr << "mean = " <<mean <<std::endl;
    return EXIT_FAILURE;
    }

  mean = cvExternalTerm0->GetMean();
  if ( ( mean < 0 ) || ( mean > 5 ) )
    {
    std::cerr << "( ( mean < 0 ) || ( mean > 5 ) )" <<std::endl;
    std::cerr << "External mean = " <<mean <<std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
