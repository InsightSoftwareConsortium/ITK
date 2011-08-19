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

#ifndef __itkLevelSetEquationTermContainerBase_hxx
#define __itkLevelSetEquationTermContainerBase_hxx

#include "itkLevelSetEquationTermContainerBase.h"
#include "itkObject.h"

namespace itk
{
// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
::LevelSetEquationTermContainerBase() : Superclass(), m_Input( NULL )
  {}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
::~LevelSetEquationTermContainerBase()
  {}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
void
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
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
        itkGenericExceptionMacro( <<"m_Input and iTerm->GetInput are NULL" );
        }
      }
    m_Container[iId] = iTerm;
    m_TermContribution[iId] = NumericTraits< LevelSetOutputPixelType >::Zero;
    m_NameContainer[ iTerm->GetTermName() ] = iTerm;

    RequiredDataType termRequiredData = iTerm->GetListOfRequiredData();

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

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
void
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
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
        itkGenericExceptionMacro( <<"m_Input and iTerm->GetInput are NULL" );
        }
      }

    TermIdType id = ( m_Container.rbegin() )->first;
    ++id;

    m_Container[ id ] = iTerm;
    m_TermContribution[ id ] = NumericTraits< LevelSetOutputPixelType >::Zero;
    m_NameContainer[ iTerm->GetTermName() ] = iTerm;

    RequiredDataType termRequiredData = iTerm->GetListOfRequiredData();

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

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
typename LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >::TermType*
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >::
GetTerm( const std::string& iName )
{
  typename HashMapStringTermContainerType::iterator
      it = m_Container.find( iName );

  if( it == m_Container.end() )
    {
    itkGenericExceptionMacro( <<"the term " << iName.c_str() << " is not present in the container" );
    }

  return it->second;
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
typename LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >::TermType*
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >::
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

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
void
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
::Initialize( const LevelSetInputIndexType& iP )
{
  MapTermContainerIteratorType term_it = m_Container.begin();
  MapTermContainerIteratorType term_end = m_Container.end();

  while( term_it != term_end )
    {
    ( term_it->second )->Initialize( iP );
    ++term_it;
    }
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
void
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
::UpdatePixel( const LevelSetInputIndexType& iP,
               const LevelSetOutputRealType & oldValue,
               const LevelSetOutputRealType & newValue )
{
  typename std::map< unsigned int, TermPointer >::iterator
    term_it = m_Container.begin();
  typename std::map< unsigned int, TermPointer >::iterator
    term_end = m_Container.end();

  while( term_it != term_end )
    {
    ( term_it->second )->UpdatePixel( iP, oldValue, newValue );
    ++term_it;
    }
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
void
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
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

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
typename LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
::Evaluate( const LevelSetInputIndexType& iP )
{
  MapTermContainerIteratorType term_it  = m_Container.begin();
  MapTermContainerIteratorType term_end = m_Container.end();

  MapCFLContainerIterator cfl_it = m_TermContribution.begin();

  LevelSetOutputRealType oValue = NumericTraits< LevelSetOutputRealType >::Zero;

  while( term_it != term_end )
    {
    LevelSetOutputRealType temp_val = ( term_it->second )->Evaluate( iP );

    cfl_it->second = vnl_math_max( vnl_math_abs( temp_val ), cfl_it->second );

    oValue += temp_val;
    ++term_it;
    ++cfl_it;
    }

  return oValue;
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
typename LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
::Evaluate( const LevelSetInputIndexType& iP, const LevelSetDataType& iData )
{
  MapTermContainerIteratorType term_it  = m_Container.begin();
  MapTermContainerIteratorType term_end = m_Container.end();

  MapCFLContainerIterator cfl_it = m_TermContribution.begin();

  LevelSetOutputRealType oValue = NumericTraits< LevelSetOutputRealType >::Zero;

  while( term_it != term_end )
    {
    LevelSetOutputRealType temp_val = ( term_it->second )->Evaluate( iP, iData );

    cfl_it->second = vnl_math_max( vnl_math_abs( temp_val ), cfl_it->second );

    oValue += temp_val;
    ++term_it;
    ++cfl_it;
    }

  return oValue;
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
void
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
::Update()
{
  MapTermContainerIteratorType term_it = m_Container.begin();
  MapTermContainerIteratorType term_end = m_Container.end();

  MapCFLContainerIterator cfl_it = m_TermContribution.begin();

  while( term_it != term_end )
    {
    ( term_it->second )->Update();
    ( cfl_it->second )= NumericTraits< LevelSetOutputPixelType >::Zero;
    ++term_it;
    ++cfl_it;
    }
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
typename LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
::ComputeCFLContribution() const
{
  MapTermContainerConstIteratorType term_it = m_Container.begin();
  MapTermContainerConstIteratorType term_end = m_Container.end();

  MapCFLContainerConstIterator cfl_it = m_TermContribution.begin();

  LevelSetOutputRealType oValue = NumericTraits< LevelSetOutputRealType >::Zero;

  while( term_it != term_end )
    {
    LevelSetOutputRealType cfl = ( term_it->second )->GetCFLContribution();

    if( cfl == NumericTraits< LevelSetOutputRealType >::Zero )
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

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
void
LevelSetEquationTermContainerBase< TInputImage, TLevelSetContainer >
::ComputeRequiredData( const LevelSetInputIndexType& iP, LevelSetDataType& ioData )
{
  typename RequiredDataType::const_iterator dIt = m_RequiredData.begin();
  typename RequiredDataType::const_iterator dEnd = m_RequiredData.begin();

  MapTermContainerIteratorType tIt = m_Container.begin();

  LevelSetPointer levelset = ( tIt->second )->GetCurrentLevelSetPointer();

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
    // here add new characteristics
    ++dIt;
    }
}
// ----------------------------------------------------------------------------

}
#endif // __itkLevelSetEquationTermContainerBase_hxx
