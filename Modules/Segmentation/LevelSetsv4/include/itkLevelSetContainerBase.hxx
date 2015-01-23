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

#ifndef itkLevelSetContainerBase_hxx
#define itkLevelSetContainerBase_hxx

#include "itkLevelSetContainerBase.h"

namespace itk
{

template< typename TIdentifier, typename TLevelSet >
LevelSetContainerBase< TIdentifier, TLevelSet >
::LevelSetContainerBase()
{
}

template< typename TIdentifier, typename TLevelSet >
LevelSetContainerBase< TIdentifier, TLevelSet >
::~LevelSetContainerBase()
{
}

template< typename TIdentifier, typename TLevelSet >
const typename
LevelSetContainerBase< TIdentifier, TLevelSet >::LevelSetContainerType&
LevelSetContainerBase< TIdentifier, TLevelSet >::GetContainer() const
{
  return m_Container;
}

template< typename TIdentifier, typename TLevelSet >
void
LevelSetContainerBase< TIdentifier, TLevelSet >
::SetContainer(const LevelSetContainerType &iContainer)
{
  m_Container = iContainer;
}

template< typename TIdentifier, typename TLevelSet >
typename LevelSetContainerBase< TIdentifier, TLevelSet >::Iterator
LevelSetContainerBase< TIdentifier, TLevelSet >::Begin()
{
  return Iterator( m_Container.begin() );
}

template< typename TIdentifier, typename TLevelSet >
typename LevelSetContainerBase< TIdentifier, TLevelSet >::ConstIterator
LevelSetContainerBase< TIdentifier, TLevelSet >::Begin() const
{
  return ConstIterator( m_Container.begin() );
}

template< typename TIdentifier, typename TLevelSet >
typename LevelSetContainerBase< TIdentifier, TLevelSet >::Iterator
LevelSetContainerBase< TIdentifier, TLevelSet >::End()
{
  return Iterator( m_Container.end() );
}

template< typename TIdentifier, typename TLevelSet >
typename LevelSetContainerBase< TIdentifier, TLevelSet >::ConstIterator
LevelSetContainerBase< TIdentifier, TLevelSet >::End() const
{
  return ConstIterator( m_Container.end() );
}

template< typename TIdentifier, typename TLevelSet >
typename LevelSetContainerBase< TIdentifier, TLevelSet >::LevelSetIdentifierType
LevelSetContainerBase< TIdentifier, TLevelSet >::Size() const
{
  return static_cast< LevelSetIdentifierType >( m_Container.size() );
}

template< typename TIdentifier, typename TLevelSet >
typename LevelSetContainerBase< TIdentifier, TLevelSet >::LevelSetPointer
LevelSetContainerBase< TIdentifier, TLevelSet >
::GetLevelSet( const LevelSetIdentifierType& iId ) const
{
  LevelSetContainerConstIteratorType it = m_Container.find( iId );

  if( it != m_Container.end() )
    {
    return it->second;
    }
  else
    {
    return ITK_NULLPTR;
    }
}

template< typename TIdentifier, typename TLevelSet >
bool LevelSetContainerBase< TIdentifier, TLevelSet >
::AddLevelSet( const LevelSetIdentifierType& iId,
               LevelSetType * iLevelSet,
               const bool iForce )
{
  if( iForce )
    {
    m_Container[iId] = iLevelSet;
    this->Modified();
    return true;
    }
  else
    {
    if( m_Container.empty() )
      {
      m_Container.insert( LevelSetPairType( iId, iLevelSet ) );
      this->Modified();
      return true;
      }
    else
      {
      LevelSetContainerIteratorType it = m_Container.find( iId );

      if( it != m_Container.end() )
        {
        return false;
        }
      else
        {
        m_Container.insert( LevelSetPairType( iId, iLevelSet ) );
        this->Modified();
        return true;
        }
      }
    }
}

template< typename TIdentifier, typename TLevelSet >
bool
LevelSetContainerBase< TIdentifier, TLevelSet >
::RemoveLevelSet( const LevelSetIdentifierType& iId )
{
  LevelSetContainerIteratorType it = m_Container.find( iId );

  if( it != m_Container.end() )
    {
    it->second = ITK_NULLPTR;
    m_Container.erase( it );

    this->Modified();

    return true;
    }
  else
    {
    return false;
    }
}

template< typename TIdentifier, typename TLevelSet >
bool
LevelSetContainerBase< TIdentifier, TLevelSet >
::HasDomainMap() const
{
  if( !this->m_DomainMapFilter.IsNull() && this->m_DomainMapFilter->GetDomainMap().size() > 0 )
    {
    return true;
    }
  return false;
}

}

#endif // itkLevelSetContainerBase_hxx
