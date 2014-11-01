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

#include "itkBinaryImageToLevelSetImageAdaptor.h"
#include "itkLevelSetDomainPartitionImage.h"

int itkLevelSetDomainPartitionImageTest( int argc, char* argv[] )
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

  typedef itk::LevelSetDomainPartitionImage< InputImageType > DomainPartitionSourceType;
  typedef DomainPartitionSourceType::ListImageType            ListImageType;
  typedef DomainPartitionSourceType::LevelSetDomainRegionVectorType LevelSetDomainRegionVectorType;

  typedef ListImageType::PixelType                                ListType;
  typedef itk::ImageRegionConstIteratorWithIndex< ListImageType > ListImageIteratorType;

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

  IdentifierType numberOfLevelSetFunctions = 2;

  LevelSetDomainRegionVectorType regionVector;
  regionVector.resize( numberOfLevelSetFunctions );
  regionVector[0] = region;
  regionVector[1] = region;

  DomainPartitionSourceType::Pointer partitionSource = DomainPartitionSourceType::New();
  partitionSource->SetNumberOfLevelSetFunctions( numberOfLevelSetFunctions );
  partitionSource->SetImage( binary );
  partitionSource->SetLevelSetDomainRegionVector( regionVector );
  partitionSource->PopulateListDomain();


  bool flag = true;

  ListType ll;
  ListImageType::ConstPointer listImage = partitionSource->GetListDomain();
  ListImageIteratorType It( listImage, listImage->GetLargestPossibleRegion() );
  It.GoToBegin();
  while( !It.IsAtEnd() )
    {
    ll =  It.Get();
    if ( ll.size() != 2 )
      {
      flag = false;
      break;
      }

    ListType::iterator it=ll.begin();

    while( it != ll.end() )
      {
      if ( ( *it != 0 ) && ( *it != 1 ) )
        {
        flag = false;
        break;
        }
      ++it;
      }

    ++It;
    }

  if ( !flag )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
