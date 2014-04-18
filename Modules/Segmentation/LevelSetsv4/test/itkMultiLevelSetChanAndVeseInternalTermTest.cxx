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
#include "itkLevelSetEquationChanAndVeseInternalTerm.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"

int itkMultiLevelSetChanAndVeseInternalTermTest( int , char* [] )
{
  const unsigned int Dimension = 2;

  typedef unsigned char                                       InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >             InputImageType;
  typedef itk::ImageRegionIteratorWithIndex< InputImageType > InputIteratorType;

  typedef float                                PixelType;
  typedef itk::Image< PixelType, Dimension >   ImageType;
  typedef itk::LevelSetDenseImage< ImageType > LevelSetType;
  typedef LevelSetType::OutputRealType         LevelSetOutputRealType;

  typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;
  typedef itk::IdentifierType                            IdentifierType;
  typedef std::list< IdentifierType >                    IdListType;
  typedef itk::Image< IdListType, Dimension >            IdListImageType;
  typedef itk::Image< short, Dimension >                 CacheImageType;
  typedef itk::LevelSetDomainMapImageFilter< IdListImageType, CacheImageType >
                                                         DomainMapImageFilterType;

  typedef itk::LevelSetContainer< IdentifierType, LevelSetType >  LevelSetContainerType;
  typedef itk::LevelSetEquationChanAndVeseInternalTerm< InputImageType, LevelSetContainerType > ChanAndVeseTermType;
  typedef itk::LevelSetEquationTermContainer< InputImageType, LevelSetContainerType > TermContainerType;

  typedef itk::AtanRegularizedHeavisideStepFunction<
      LevelSetOutputRealType, LevelSetOutputRealType >  HeavisideFunctionBaseType;

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
  input->FillBuffer( 0 );

  InputIteratorType it( input, input->GetLargestPossibleRegion() );
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( 1 );
    ++it;
    }

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

  // Define the Heaviside function
  HeavisideFunctionBaseType::Pointer heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon( 1.0 );

  DomainMapImageFilterType::Pointer domainMapFilter = DomainMapImageFilterType::New();
  domainMapFilter->SetInput( id_image );
  domainMapFilter->Update();
  CacheImageType::Pointer output = domainMapFilter->GetOutput();
  std::cout << "Domain partition computed" << std::endl;

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

    it1.Set( std::sqrt(
             static_cast< float> ( ( idx[0] - 2 ) * ( idx[0] - 2 ) +
                                   ( idx[1] - 2 ) * ( idx[1] - 2 ) ) ) );

    it2.Set( std::sqrt(
             static_cast< float> ( ( idx[0] - 5 ) * ( idx[0] - 5 ) +
                                   ( idx[1] - 5 ) * ( idx[1] - 5 ) ) ) );
    ++it1;
    ++it2;
    }

  // Map of levelset bases
  std::map< itk::IdentifierType, LevelSetType::Pointer > level_set;
  level_set[1] = LevelSetType::New();
  level_set[1]->SetImage( input1 );

  level_set[2] = LevelSetType::New();
  level_set[2]->SetImage( input2 );

  // Insert the levelsets in a levelset container
  bool levelSetNotYetAdded;
  LevelSetContainerType::Pointer lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside( heaviside );
  lscontainer->SetDomainMapFilter( domainMapFilter );

  levelSetNotYetAdded = lscontainer->AddLevelSet( 0, level_set[1], false );
  if ( !levelSetNotYetAdded )
    {
    return EXIT_FAILURE;
    }
  levelSetNotYetAdded = lscontainer->AddLevelSet( 1, level_set[2], false );
  if ( !levelSetNotYetAdded )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Level set container created" << std::endl;

  // Create ChanAndVeseTerm
  ChanAndVeseTermType::Pointer cvTerm = ChanAndVeseTermType::New();
  cvTerm->SetInput( input );
  cvTerm->SetCoefficient( 1.0 );
  cvTerm->SetCurrentLevelSetId( 0 );
  cvTerm->SetLevelSetContainer( lscontainer );
  std::cout << "CV term created" << std::endl;

  // Create Term Container
  TermContainerType::Pointer termContainer = TermContainerType::New();
  termContainer->AddTerm( 0, cvTerm );
  std::cout << "Term container created" << std::endl;

  typedef DomainMapImageFilterType::DomainMapType DomainMapType;
  typedef DomainMapType::const_iterator           DomainIteratorType;
  const DomainMapType domainMap = domainMapFilter->GetDomainMap();
  DomainIteratorType map_it     = domainMap.begin();
  DomainIteratorType map_end    = domainMap.end();

  LevelSetType::Pointer levelSet;
  ChanAndVeseTermType::Pointer eqTerm;
  while( map_it != map_end )
    {
    const IdListImageType::RegionType temp_region = *(map_it->second.GetRegion());

    itk::ImageRegionConstIteratorWithIndex<IdListImageType >
        temp_it( id_image, temp_region );
    temp_it.GoToBegin();

    while( !temp_it.IsAtEnd() )
      {
      std::cout << temp_it.GetIndex() << std::endl;
      const IdListType * lout = map_it->second.GetIdList();

      if( lout->empty() )
        {
        return EXIT_FAILURE;
        }

      for( IdListType::const_iterator lIt = lout->begin(); lIt != lout->end(); ++lIt )
        {
        std::cout << *lIt << " ";
        levelSet = lscontainer->GetLevelSet( *lIt - 1);
        std::cout << levelSet->Evaluate( temp_it.GetIndex() ) << std::endl;

        if ( *lIt - 1 == 0 )
          {
          eqTerm =  dynamic_cast< ChanAndVeseTermType* >( termContainer->GetTerm( *lIt - 1 ) );
          std::cout << eqTerm->Evaluate( temp_it.GetIndex() ) << std::endl;
          }
        }
      std::cout << std::endl;
      ++temp_it;
      }
    ++map_it;
    }

  return EXIT_SUCCESS;
}
