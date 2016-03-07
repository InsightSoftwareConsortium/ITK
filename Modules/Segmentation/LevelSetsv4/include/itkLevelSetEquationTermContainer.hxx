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

#ifndef itkLevelSetEquationTermContainer_hxx
#define itkLevelSetEquationTermContainer_hxx

#include "itkMath.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkObject.h"

namespace itk
{
// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::LevelSetEquationTermContainer()
{
  this->m_CurrentLevelSetId = LevelSetIdentifierType();
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::~LevelSetEquationTermContainer()
{}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
typename LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::Iterator
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::Begin()
{
  return Iterator( this->m_Container.begin() );
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
typename LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::Iterator
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::End()
{
  return Iterator( this->m_Container.end() );
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
typename LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::ConstIterator
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::Begin() const
{
  return ConstIterator( this->m_Container.begin() );
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
typename LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::ConstIterator
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::End() const
{
  return ConstIterator( this->m_Container.end() );
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
void
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::AddTerm( const TermIdType& iId, TermType* iTerm )
{
  if( iTerm )
    {
    if( ! iTerm->GetInput() )
      {
      if( m_Input.IsNotNull() )
        {
        iTerm->SetInput( m_Input );
        }
      else
        {
        itkGenericExceptionMacro( <<"m_Input and iTerm->GetInput are ITK_NULLPTR" );
        }
      }
    iTerm->SetCurrentLevelSetId( this->m_CurrentLevelSetId );

    if( this->m_LevelSetContainer.IsNotNull() )
      {
      iTerm->SetLevelSetContainer( this->m_LevelSetContainer );
      }
    else
      {
      if( ! iTerm->GetLevelSetContainer() )
        {
        itkGenericExceptionMacro( <<"m_LevelSetContainer and iTerm->GetLevelSetContainer() are ITK_NULLPTR" );
        }
      }

    m_Container[iId] = iTerm;
    m_TermContribution[iId] = NumericTraits< LevelSetOutputPixelType >::ZeroValue();
    m_NameContainer[ iTerm->GetTermName() ] = iTerm;

    RequiredDataType termRequiredData = iTerm->GetRequiredData();

    typename RequiredDataType::const_iterator dIt   = termRequiredData.begin();
    typename RequiredDataType::const_iterator dEnd  = termRequiredData.end();

    while( dIt != dEnd )
      {
      m_RequiredData.insert( *dIt );
      ++dIt;
      }

    this->Modified();
    }
  else
    {
    itkGenericExceptionMacro( <<"Term supplied is null" );
    }
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
void
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::PushTerm( TermType* iTerm )
{
  if( iTerm )
    {
    if( ! iTerm->GetInput() )
      {
      if( m_Input.IsNotNull() )
        {
        iTerm->SetInput( m_Input );
        }
      else
        {
        itkGenericExceptionMacro( <<"m_Input and iTerm->GetInput are ITK_NULLPTR" );
        }
      }

    iTerm->SetCurrentLevelSetId( this->m_CurrentLevelSetId );

    if( this->m_LevelSetContainer.isNotNull() )
      {
      iTerm->SetLevelSetContainer( this->m_LevelSetContainer );
      }
    else
      {
      if( ! iTerm->GetLevelSetContainer() )
        {
        itkGenericExceptionMacro( <<"m_LevelSetContainer and iTerm->GetLevelSetContainer() are ITK_NULLPTR" );
        }
      }

    TermIdType id = ( m_Container.rbegin() )->first;
    ++id;

    m_Container[ id ] = iTerm;
    m_TermContribution[ id ] = NumericTraits< LevelSetOutputPixelType >::ZeroValue();
    m_NameContainer[ iTerm->GetTermName() ] = iTerm;

    RequiredDataType termRequiredData = iTerm->GetRequiredData();

    typename RequiredDataType::const_iterator dIt = termRequiredData.begin();
    typename RequiredDataType::const_iterator dEnd = termRequiredData.end();

    while( dIt != dEnd )
      {
      m_RequiredData.insert( *dIt );
      ++dIt;
      }

    this->Modified();
    }
  else
    {
    itkGenericExceptionMacro( <<"Term supplied is null" );
    }
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
typename LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >::TermType*
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >::
GetTerm( const std::string& iName )
{
  MapTermContainerIteratorType it = m_Container.find( iName );

  if( it == m_Container.end() )
    {
    itkGenericExceptionMacro( <<"the term " << iName.c_str() << " is not present in the container" );
    }

  return it->second;
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
typename LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >::TermType*
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >::
GetTerm( const TermIdType& iId )
{
  MapTermContainerIteratorType it = m_Container.find( iId );

  if( it == m_Container.end() )
    {
    itkGenericExceptionMacro( <<"the term " << iId << " is not present in the container" );
    }

  return it->second;
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
void
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::Initialize( const LevelSetInputIndexType& iP )
{
  MapTermContainerIteratorType term_it  = m_Container.begin();
  MapTermContainerIteratorType term_end = m_Container.end();

  while( term_it != term_end )
    {
    ( term_it->second )->Initialize( iP );
    ++term_it;
    }
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
void
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::UpdatePixel( const LevelSetInputIndexType& iP,
               const LevelSetOutputRealType & oldValue,
               const LevelSetOutputRealType & newValue )
{
  MapTermContainerIteratorType term_it = m_Container.begin();
  MapTermContainerIteratorType term_end = m_Container.end();

  while( term_it != term_end )
    {
    ( term_it->second )->UpdatePixel( iP, oldValue, newValue );
    ++term_it;
    }
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
void
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::InitializeParameters()
{
  MapTermContainerIteratorType term_it = m_Container.begin();
  MapTermContainerIteratorType term_end = m_Container.end();

  while( term_it != term_end )
    {
    ( term_it->second )->InitializeParameters();
    ++term_it;
    }
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
typename LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::Evaluate( const LevelSetInputIndexType& iP )
{
  MapTermContainerIteratorType term_it  = m_Container.begin();
  MapTermContainerIteratorType term_end = m_Container.end();

  MapCFLContainerIterator cfl_it = m_TermContribution.begin();

  LevelSetOutputRealType oValue = NumericTraits< LevelSetOutputRealType >::ZeroValue();

  while( term_it != term_end )
    {
    LevelSetOutputRealType temp_val = ( term_it->second )->Evaluate( iP );

    cfl_it->second = std::max( itk::Math::abs( temp_val ), cfl_it->second );

    oValue += temp_val;
    ++term_it;
    ++cfl_it;
    }

  return oValue;
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
typename LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::Evaluate( const LevelSetInputIndexType& iP, const LevelSetDataType& iData )
{
  MapTermContainerIteratorType term_it  = m_Container.begin();
  MapTermContainerIteratorType term_end = m_Container.end();

  MapCFLContainerIterator cfl_it = m_TermContribution.begin();

  LevelSetOutputRealType oValue = NumericTraits< LevelSetOutputRealType >::ZeroValue();

  while( term_it != term_end )
    {
    LevelSetOutputRealType temp_val = ( term_it->second )->Evaluate( iP, iData );

    cfl_it->second = std::max( itk::Math::abs( temp_val ), cfl_it->second );

    oValue += temp_val;
    ++term_it;
    ++cfl_it;
    }

  return oValue;
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
void
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::Update()
{
  MapTermContainerIteratorType term_it = m_Container.begin();
  MapTermContainerIteratorType term_end = m_Container.end();

  MapCFLContainerIterator cfl_it = m_TermContribution.begin();

  while( term_it != term_end )
    {
    ( term_it->second )->Update();
    ( cfl_it->second )= NumericTraits< LevelSetOutputPixelType >::ZeroValue();
    ++term_it;
    ++cfl_it;
    }
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
typename LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::ComputeCFLContribution() const
{
  MapTermContainerConstIteratorType term_it = m_Container.begin();
  MapTermContainerConstIteratorType term_end = m_Container.end();

  MapCFLContainerConstIterator cfl_it = m_TermContribution.begin();

  LevelSetOutputRealType oValue = NumericTraits< LevelSetOutputRealType >::ZeroValue();

  while( term_it != term_end )
    {
    LevelSetOutputRealType cfl = ( term_it->second )->GetCFLContribution();

    if( Math::AlmostEquals( cfl, NumericTraits< LevelSetOutputRealType >::ZeroValue() ) )
      {
      cfl = ( cfl_it->second );
      }

    oValue += cfl;
    ++term_it;
    ++cfl_it;
    }

  return oValue;
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
void
LevelSetEquationTermContainer< TInputImage, TLevelSetContainer >
::ComputeRequiredData( const LevelSetInputIndexType& iP, LevelSetDataType& ioData )
{
  typename RequiredDataType::const_iterator dIt = m_RequiredData.begin();
  typename RequiredDataType::const_iterator dEnd = m_RequiredData.end();

  MapTermContainerIteratorType tIt = m_Container.begin();

  LevelSetPointer levelset = ( tIt->second )->GetModifiableCurrentLevelSetPointer();

  while( dIt != dEnd )
    {
    if( *dIt == "Value" )
      {
      levelset->Evaluate( iP, ioData );
      }
    if( *dIt == "Gradient" )
      {
      levelset->EvaluateGradient( iP, ioData );
      }
    if( *dIt == "Hessian" )
      {
      levelset->EvaluateHessian( iP, ioData );
      }
    if( *dIt == "Laplacian" )
      {
      levelset->EvaluateLaplacian( iP, ioData );
      }
    if( *dIt == "GradientNorm" )
      {
      levelset->EvaluateGradientNorm( iP, ioData );
      }
    if( *dIt == "MeanCurvature" )
      {
      levelset->EvaluateMeanCurvature( iP, ioData );
      }
    if( *dIt == "ForwardGradient" )
      {
      levelset->EvaluateForwardGradient( iP, ioData );
      }
    if( *dIt == "BackwardGradient" )
      {
      levelset->EvaluateBackwardGradient( iP, ioData );
      }
    // here add new characteristics
    ++dIt;
    }
}

}
#endif // itkLevelSetEquationTermContainer_hxx
