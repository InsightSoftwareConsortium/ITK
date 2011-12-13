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

#include "itkLevelSetDomainMapImageFilter.h"

int itkLevelSetDomainMapImageFilterTest( int, char* [] )
{
  const unsigned int Dimension = 2;

  typedef std::list<int>                 ListPixelType;
  typedef std::list<int>::const_iterator ListIteratorType;

  typedef itk::Image< ListPixelType, Dimension >   InputImageType;
  typedef itk::Image< unsigned short, Dimension >  OutputImageType;

  typedef itk::LevelSetDomainMapImageFilter< InputImageType, OutputImageType >
                                                  DomainMapImageFilterType;
  typedef DomainMapImageFilterType::DomainMapType DomainMapType;

  InputImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  InputImageType::SizeType size;
  size[0] = 10;
  size[1] = 10;

  InputImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize( size );

  ListPixelType l;

  InputImageType::Pointer input = InputImageType::New();
  input->SetRegions( region );
  input->Allocate();
  input->FillBuffer( l );

  for( unsigned int i = 0; i < 10; i++ )
    {
    ListPixelType ll;
    ll.push_back(i);
    ll.push_back(i+1);

    index[0] = index[1] = i;
    input->SetPixel( index, ll );
    }

  DomainMapImageFilterType::Pointer filter = DomainMapImageFilterType::New();
  filter->SetInput( input );
  filter->Update();

  OutputImageType::Pointer output = filter->GetOutput();

  typedef itk::ImageRegionConstIteratorWithIndex<OutputImageType > OutputImageIteratorType;
  OutputImageIteratorType it( output, output->GetLargestPossibleRegion() );

  it.GoToBegin();

  OutputImageType::IndexType out_index;
  OutputImageType::PixelType out_id;

  const DomainMapType domainMap  = filter->GetDomainMap();
  DomainMapType::const_iterator mapIt;
  const DomainMapType::const_iterator mapEnd = domainMap.end();
  while( !it.IsAtEnd() )
    {
    out_index = it.GetIndex();
    out_id = it.Get();

    if( out_id > 0 )
      {
      std::cout << "*** " <<std::endl;
      std::cout << out_index << " # " << out_id <<std::endl;
      mapIt = domainMap.find( out_id );
      if( mapIt != mapEnd )
        {
        const InputImageType::RegionType domainMapRegion = *(mapIt->second.GetRegion());
        std::cout << domainMapRegion;

        const ListPixelType * lout = mapIt->second.GetIdList();
        if( lout->empty() )
          {
          return EXIT_FAILURE;
          }
        else
          {
          for( ListIteratorType lIt = lout->begin(); lIt != lout->end(); ++lIt )
            {
            std::cout << *lIt << " ";
            }
          std::cout << std::endl;
          }
        }
        }
    ++it;
    }

  return EXIT_SUCCESS;
}
