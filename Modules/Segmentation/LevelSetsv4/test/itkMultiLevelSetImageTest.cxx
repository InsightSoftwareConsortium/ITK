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

#include "itkImage.h"
#include "itkLevelSetImageBase.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkLevelSetDomainMapImageFilter.h"

int itkMultiLevelSetImageTest( int , char* [] )
{
  const unsigned int Dimension = 2;

  typedef float                                          PixelType;
  typedef itk::Image< PixelType, Dimension >             ImageType;
  typedef itk::LevelSetImageBase< ImageType >            LevelSetType;
  typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;
  typedef std::list< itk::IdentifierType >               IdListType;
  typedef itk::Image< IdListType, Dimension >            IdListImageType;
  typedef itk::Image< short, Dimension >                 CacheImageType;
  typedef itk::LevelSetDomainMapImageFilter< IdListImageType, CacheImageType >
                                                         DomainMapImageFilterType;
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

    it1.Set( vcl_sqrt(
             static_cast< float> ( ( idx[0] - 2 ) * ( idx[0] - 2 ) +
                                   ( idx[1] - 2 ) * ( idx[1] - 2 ) ) ) );

    it2.Set( vcl_sqrt(
             static_cast< float> ( ( idx[0] - 5 ) * ( idx[0] - 5 ) +
                                   ( idx[1] - 5 ) * ( idx[1] - 5 ) ) ) );
    ++it1;
    ++it2;
    }

  std::map< itk::IdentifierType, LevelSetType::Pointer > level_set;
  level_set[1] = LevelSetType::New();
  level_set[1]->SetImage( input1 );

  level_set[2] = LevelSetType::New();
  level_set[2]->SetImage( input2 );

  DomainMapImageFilterType::Pointer filter = DomainMapImageFilterType::New();
  filter->SetInput( id_image );
  filter->Update();
  CacheImageType::Pointer output = filter->GetOutput();

  itk::ImageRegionConstIteratorWithIndex<CacheImageType >
      it( output, output->GetLargestPossibleRegion() );

  it.GoToBegin();

  CacheImageType::IndexType out_index;
  CacheImageType::PixelType out_id;

  while( !it.IsAtEnd() )
    {
    out_index = it.GetIndex();
    out_id = it.Get();

    IdListType solution;
    if( ( out_index[0] < 5 ) && ( out_index[1] < 5 ) )
      {
      solution.push_back( 1 );
      }

    if( ( out_index[0] > 1 ) && ( out_index[1] > 1 ) &&
        ( out_index[0] < 8 ) && ( out_index[1] < 8 ) )
      {
      solution.push_back( 2 );
      }
    solution.sort();

    std::cout <<"***" << std::endl;
    std::cout << out_index <<std::endl;

    if( out_id != 0 )
      {
      IdListType lout = filter->m_LevelSetMap[out_id].m_List;
      std::cout << filter->m_LevelSetMap[out_id].m_Region;
      if( lout.empty() )
        {
        return EXIT_FAILURE;
        }
      else
        {
        for( IdListType::iterator lIt = lout.begin(); lIt != lout.end(); ++lIt )
          {
          std::cout << *lIt <<" " << level_set[*lIt]->Evaluate( out_index )
                    << std::endl;
          }
        std::cout << std::endl;

        lout.sort();
        if( lout != solution )
          {
          std::cerr <<"FAILURE!!!" <<std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    ++it;
    }

  typedef DomainMapImageFilterType::DomainIteratorType DomainIteratorType;
  DomainIteratorType map_it = filter->m_LevelSetMap.begin();
  DomainIteratorType map_end = filter->m_LevelSetMap.end();

  while( map_it != map_end )
    {
    IdListImageType::RegionType temp_region = map_it->second.m_Region;

    itk::ImageRegionConstIteratorWithIndex<IdListImageType >
        temp_it( id_image, temp_region );
    temp_it.GoToBegin();

    // Iterate through image regions with same list pixels
    while( !temp_it.IsAtEnd() )
      {
      std::cout << temp_it.GetIndex() << std::endl;
      IdListType lout = map_it->second.m_List;

      if( lout.empty() )
        {
        return EXIT_FAILURE;
        }

      // Iterate through all the levelsets at a given pixel location
      for( IdListType::iterator lIt = lout.begin(); lIt != lout.end(); ++lIt )
        {
        std::cout << *lIt <<" " << level_set[*lIt]->Evaluate( temp_it.GetIndex() )
                  << std::endl;
        }
      std::cout << std::endl;
      ++temp_it;
      }
    ++map_it;
    }

  return EXIT_SUCCESS;
}
