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

#ifndef __itkLevelSetEquationContainer_hxx
#define __itkLevelSetEquationContainer_hxx

#include "itkLevelSetEquationContainer.h"
#include "itkNumericTraits.h"

namespace itk
{
template< class TTermContainer >
LevelSetEquationContainer< TTermContainer >
::LevelSetEquationContainer()
{
}

template< class TTermContainer >
LevelSetEquationContainer< TTermContainer >
::~LevelSetEquationContainer()
{
}

template< class TTermContainer >
void
LevelSetEquationContainer< TTermContainer >
::AddEquation( const LevelSetIdentifierType& iId,
               TermContainerType * iEquation )
{
  if ( iEquation )
    {
    if( this->m_LevelSetContainer.IsNotNull() )
      {
      iEquation->SetLevelSetContainer( this->m_LevelSetContainer );
      }
    else
      {
      if( ! iEquation->GetLevelSetContainer() )
        {
        itkGenericExceptionMacro( << "m_LevelSetContainer and iEquation->GetLevelSetContainer() are NULL" );
        }
      }
    this->m_Container[iId] = iEquation;
    if( iEquation->GetInput() )
      {
      this->m_Input = iEquation->GetModifiableInput();
      }
    this->Modified();
    }
  else
    {
    itkGenericExceptionMacro( <<"Term supplied is null" );
    }
}

template< class TTermContainer >
typename LevelSetEquationContainer< TTermContainer >::TermContainerType*
LevelSetEquationContainer< TTermContainer >
::GetEquation( const LevelSetIdentifierType& iId ) const
{
  if( this->m_Container.empty() )
    {
    itkGenericExceptionMacro( << "m_Container is empty" );
    }

  MapContainerConstIterator it = this->m_Container.find( iId );
  if( it == this->m_Container.end() )
    {
    itkGenericExceptionMacro( <<"this equation " << iId << " does not exist" );
    }
  return it->second;
}

template< class TTermContainer >
typename LevelSetEquationContainer< TTermContainer >::Iterator
LevelSetEquationContainer< TTermContainer >::Begin()
{
  return Iterator( m_Container.begin() );
}

template< class TTermContainer >
typename LevelSetEquationContainer< TTermContainer >::Iterator
LevelSetEquationContainer< TTermContainer >::End()
{
  return Iterator( m_Container.end() );
}

template< class TTermContainer >
typename LevelSetEquationContainer< TTermContainer >::ConstIterator
LevelSetEquationContainer< TTermContainer >::Begin() const
{
  return ConstIterator( m_Container.begin() );
}

template< class TTermContainer >
typename LevelSetEquationContainer< TTermContainer >::ConstIterator
LevelSetEquationContainer< TTermContainer >::End() const
{
  return ConstIterator( m_Container.end() );
}

template< class TTermContainer >
void
LevelSetEquationContainer< TTermContainer >
::UpdateInternalEquationTerms()
{
  for( MapContainerIterator it = this->m_Container.begin();
    it != this->m_Container.end();
    ++it )
    {
    (it->second )->Update();
    }
}

template< class TTermContainer >
void
LevelSetEquationContainer< TTermContainer >
::UpdatePixel( const LevelSetInputIndexType& iP,
               const LevelSetOutputRealType & oldValue,
               const LevelSetOutputRealType & newValue )
{
  for( MapContainerIterator it = this->m_Container.begin();
    it != this->m_Container.end(); ++it )
    {
    (it->second )->UpdatePixel( iP, oldValue, newValue );
    }
}

template< class TTermContainer >
void
LevelSetEquationContainer< TTermContainer >
::InitializeParameters()
{
  for( MapContainerIterator it = this->m_Container.begin();
    it != this->m_Container.end();
    ++it )
    {
    (it->second )->InitializeParameters();
    }
  }

template< class TTermContainer >
typename LevelSetEquationContainer< TTermContainer >::LevelSetOutputRealType
LevelSetEquationContainer< TTermContainer >
::ComputeCFLContribution() const
{
  LevelSetOutputRealType oValue = NumericTraits< LevelSetOutputRealType >::max();

  for( MapContainerConstIterator it = this->m_Container.begin();
       it != this->m_Container.end();
       ++it )
    {
    oValue = vnl_math_min( oValue, ( it->second )->ComputeCFLContribution() );
    }

  return oValue;
}

}

#endif // __itkLevelSetEquationContainer_hxx
