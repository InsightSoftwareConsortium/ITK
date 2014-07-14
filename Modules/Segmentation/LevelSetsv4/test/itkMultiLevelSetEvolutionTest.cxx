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

#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationChanAndVeseExternalTerm.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkLevelSetEquationContainer.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"
#include "itkLevelSetEvolution.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"

int itkMultiLevelSetEvolutionTest( int , char* [] )
{
  const unsigned int Dimension = 2;

  typedef unsigned char                                       InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >             InputImageType;

  typedef float                                          PixelType;
  typedef itk::Image< PixelType, Dimension >             ImageType;
  typedef itk::LevelSetDenseImage< ImageType >           LevelSetType;
  typedef LevelSetType::OutputRealType                   LevelSetOutputRealType;
  typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;
  typedef itk::IdentifierType                            IdentifierType;
  typedef std::list< IdentifierType >                    IdListType;
  typedef itk::Image< IdListType, Dimension >            IdListImageType;
  typedef itk::Image< short, Dimension >                 CacheImageType;
  typedef itk::LevelSetDomainMapImageFilter< IdListImageType, CacheImageType >
                                                         DomainMapImageFilterType;

  typedef itk::LevelSetContainer< IdentifierType, LevelSetType >
      LevelSetContainerType;
  typedef itk::LevelSetEquationChanAndVeseInternalTerm< InputImageType, LevelSetContainerType >
      ChanAndVeseInternalTermType;
  typedef itk::LevelSetEquationChanAndVeseExternalTerm< InputImageType, LevelSetContainerType >
      ChanAndVeseExternalTermType;
  typedef itk::LevelSetEquationTermContainer< InputImageType, LevelSetContainerType >
      TermContainerType;

  typedef itk::LevelSetEquationContainer< TermContainerType >     EquationContainerType;

  typedef itk::LevelSetEvolution< EquationContainerType, LevelSetType > LevelSetEvolutionType;

  typedef itk::AtanRegularizedHeavisideStepFunction<
      LevelSetOutputRealType, LevelSetOutputRealType > HeavisideFunctionBaseType;

  ImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  ImageType::SizeType size;
  size[0] = 10;
  size[1] = 10;

  ImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize( size );

  PixelType value = 0.;

  InputImageType::Pointer input = InputImageType::New();
  input->SetRegions( region );
  input->Allocate();
  input->FillBuffer( 1 );

  ImageType::Pointer input1 = ImageType::New();
  input1->SetRegions( region );
  input1->Allocate();
  input1->FillBuffer( value );

  ImageType::Pointer input2 = ImageType::New();
  input2->SetRegions( region );
  input2->Allocate();
  input2->FillBuffer( value );

  ImageType::IndexType idx;
  IdListType list_ids;

  IdListImageType::Pointer id_image = IdListImageType::New();
  id_image->SetRegions( region );
  id_image->Allocate();
  id_image->FillBuffer( list_ids );

  IteratorType it1( input1, input1->GetLargestPossibleRegion() );
  IteratorType it2( input2, input2->GetLargestPossibleRegion() );
  it1.GoToBegin();
  it2.GoToBegin();

  while( !it1.IsAtEnd() )
    {
    idx = it1.GetIndex();
    list_ids.clear();

    if( ( idx[0] < 5 ) && ( idx[1] < 5 ) )
      {
      list_ids.push_back( 1 );
      }

    if( ( idx[0] > 1 ) && ( idx[1] > 1 ) &&
        ( idx[0] < 8 ) && ( idx[1] < 8 ) )
      {
      list_ids.push_back( 2 );
      }

    id_image->SetPixel( idx, list_ids );
    input->SetPixel( idx, idx[0] );

    it1.Set( std::sqrt(
             static_cast< float> ( ( idx[0] - 2 ) * ( idx[0] - 2 ) +
                                   ( idx[1] - 2 ) * ( idx[1] - 2 ) ) ) - 1.5);

    it2.Set( std::sqrt(
             static_cast< float> ( ( idx[0] - 5 ) * ( idx[0] - 5 ) +
                                   ( idx[1] - 5 ) * ( idx[1] - 5 ) ) ) - 2.5 );
    ++it1;
    ++it2;
    }

  DomainMapImageFilterType::Pointer domainMapFilter = DomainMapImageFilterType::New();
  domainMapFilter->SetInput( id_image );
  domainMapFilter->Update();

  // Define the Heaviside function
  HeavisideFunctionBaseType::Pointer heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon( 1.0 );

  // Map of levelset bases
  std::map< itk::IdentifierType, LevelSetType::Pointer > level_set;
  level_set[1] = LevelSetType::New();
  level_set[1]->SetImage( input1 );

  level_set[2] = LevelSetType::New();
  level_set[2]->SetImage( input2 );

  // Insert the levelsets in a levelset container
  LevelSetContainerType::Pointer lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside( heaviside );
  lscontainer->SetDomainMapFilter( domainMapFilter );

  bool LevelSetNotYetAdded = lscontainer->AddLevelSet( 0, level_set[1], false );
  if ( !LevelSetNotYetAdded )
    {
    return EXIT_FAILURE;
    }

  LevelSetNotYetAdded = lscontainer->AddLevelSet( 1, level_set[2], false );
  if ( !LevelSetNotYetAdded )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Level set container created" << std::endl;

  // **************** CREATE ALL TERMS ****************

  // -----------------------------
  // *** 1st Level Set phi_{1} ***

  // Create ChanAndVese internal term for phi_{1}
  ChanAndVeseInternalTermType::Pointer cvInternalTerm0 = ChanAndVeseInternalTermType::New();
  cvInternalTerm0->SetInput( input );
  cvInternalTerm0->SetCoefficient( 1.0 );
  std::cout << "LevelSet 1: CV internal term created" << std::endl;

  // Create ChanAndVese external term for phi_{1}
  ChanAndVeseExternalTermType::Pointer cvExternalTerm0 = ChanAndVeseExternalTermType::New();
  cvExternalTerm0->SetInput( input );
  cvExternalTerm0->SetCoefficient( 1.0 );
  std::cout << "LevelSet 1: CV external term created" << std::endl;

  // -----------------------------
  // *** 2nd Level Set phi_{2} ***

  // Create ChanAndVese internal term for phi_{1}
  ChanAndVeseInternalTermType::Pointer cvInternalTerm1 = ChanAndVeseInternalTermType::New();
  cvInternalTerm1->SetInput( input );
  cvInternalTerm1->SetCoefficient( 1.0 );
  std::cout << "LevelSet 2: CV internal term created" << std::endl;

  // Create ChanAndVese external term for phi_{2}
  ChanAndVeseExternalTermType::Pointer cvExternalTerm1 = ChanAndVeseExternalTermType::New();
  cvExternalTerm1->SetInput( input );
  cvExternalTerm1->SetCoefficient( 1.0 );
  std::cout << "LevelSet 2: CV external term created" << std::endl;

  // **************** CREATE ALL EQUATIONS ****************

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

  typedef itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion< LevelSetContainerType >
      StoppingCriterionType;
  StoppingCriterionType::Pointer criterion = StoppingCriterionType::New();
  criterion->SetNumberOfIterations( 2 );

  EquationContainerType::Pointer equationContainer = EquationContainerType::New();
  equationContainer->AddEquation( 0, termContainer0 );
  equationContainer->AddEquation( 1, termContainer1 );
  equationContainer->SetLevelSetContainer( lscontainer );

  LevelSetEvolutionType::Pointer evolution = LevelSetEvolutionType::New();
  evolution->SetEquationContainer( equationContainer );
  evolution->SetStoppingCriterion( criterion );
  evolution->SetLevelSetContainer( lscontainer );
  try
    {
    evolution->Update();
    }
  catch ( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
