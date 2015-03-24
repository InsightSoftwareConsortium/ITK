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
#ifndef itkLevelSetDomainPartitionImage_hxx
#define itkLevelSetDomainPartitionImage_hxx

#include "itkLevelSetDomainPartitionImage.h"

namespace itk
{
template< typename TImage >
LevelSetDomainPartitionImage< TImage >
::LevelSetDomainPartitionImage()
{
}

template< typename TImage >
LevelSetDomainPartitionImage< TImage >
::~LevelSetDomainPartitionImage()
{
}

template< typename TImage >
void
LevelSetDomainPartitionImage< TImage >
::SetLevelSetDomainRegionVector( const LevelSetDomainRegionVectorType& domain )
{
  m_LevelSetDomainRegionVector = domain;
}

template< typename TImage >
const typename LevelSetDomainPartitionImage< TImage >::LevelSetDomainRegionVectorType&
LevelSetDomainPartitionImage< TImage >
::GetLevelSetDomainRegionVector() const
{
  return m_LevelSetDomainRegionVector;
}

template< typename TImage >
void LevelSetDomainPartitionImage< TImage >
::PopulateListDomain()
{
  this->AllocateListDomain();

  const ListRegionType & region = this->m_ListDomain->GetLargestPossibleRegion();
  ListIteratorType lIt(this->m_ListDomain, region);

  for ( lIt.GoToBegin(); !lIt.IsAtEnd(); ++lIt )
    {
    ListIndexType listIndex = lIt.GetIndex();
    IdentifierListType identifierList;
    IdentifierType i = NumericTraits< IdentifierType >::ZeroValue();
    while( i < this->m_NumberOfLevelSetFunctions )
      {
      if ( this->m_LevelSetDomainRegionVector[i].IsInside( listIndex ) )
        {
        identifierList.push_back(i);
        }
      i++;
      }
    lIt.Set(identifierList);
    }
}

template< typename TImage >
void LevelSetDomainPartitionImage< TImage >
::AllocateListDomain()
{
  if( m_Image.IsNull() )
    {
    itkGenericExceptionMacro( "m_Image is null" );
    }
  this->m_ListDomain = ListImageType::New();
  this->m_ListDomain->CopyInformation( this->m_Image );
  this->m_ListDomain->SetRegions( this->m_Image->GetLargestPossibleRegion() );
  this->m_ListDomain->Allocate();
}

} //end namespace itk

#endif
