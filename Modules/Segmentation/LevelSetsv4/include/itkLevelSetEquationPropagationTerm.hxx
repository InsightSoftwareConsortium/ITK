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

#ifndef __itkLevelSetEquationPropagationTerm_hxx
#define __itkLevelSetEquationPropagationTerm_hxx

#include "itkLevelSetEquationPropagationTerm.h"

namespace itk
{
template< class TInput, class TLevelSetContainer >
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >
::LevelSetEquationPropagationTerm()
{
  this->m_TermName = "Propagation term";
  this->m_RequiredData.insert( "BackwardGradient" );
  this->m_RequiredData.insert( "ForwardGradient" );
}

template< class TInput, class TLevelSetContainer >
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >
::~LevelSetEquationPropagationTerm()
{
}


template< class TInput, class TLevelSetContainer >
void
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >
::InitializeParameters()
{
  this->SetUp();
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >
::Initialize( const LevelSetInputIndexType& )
{
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >
::Update()
{
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >
::UpdatePixel( const LevelSetInputIndexType& itkNotUsed( iP ),
               const LevelSetOutputRealType& itkNotUsed( oldValue ),
               const LevelSetOutputRealType& itkNotUsed( newValue ) )
{
}

template< class TInput, class TLevelSetContainer >
typename LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >
::PropagationSpeed( const LevelSetInputIndexType& iP ) const
{
  return ( static_cast< LevelSetOutputRealType >( this->m_Input->GetPixel(iP) ) );
}

template< class TInput, class TLevelSetContainer >
typename LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >
::Value( const LevelSetInputIndexType& iP )
{
  LevelSetGradientType backwardGradient = this->m_CurrentLevelSetPointer->EvaluateBackwardGradient( iP );
  LevelSetGradientType forwardGradient  = this->m_CurrentLevelSetPointer->EvaluateForwardGradient( iP );

  const LevelSetOutputRealType zero = NumericTraits< LevelSetOutputRealType >::Zero;

  /// \todo why this initialization ?
  LevelSetOutputRealType propagation_gradient = zero;

  if ( propagation_gradient > NumericTraits< LevelSetOutputRealType >::Zero )
    {
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      propagation_gradient +=
        vnl_math_sqr( vnl_math_max( backwardGradient[i], zero ) ) +
        vnl_math_sqr( vnl_math_min( forwardGradient[i],  zero ) );
      }
    }
  else
    {
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      propagation_gradient +=
        vnl_math_sqr( vnl_math_min( backwardGradient[i], zero ) ) +
        vnl_math_sqr( vnl_math_max( forwardGradient[i],  zero) );
      }
    }
  propagation_gradient *= this->PropagationSpeed( iP );

  return propagation_gradient;
}

template< class TInput, class TLevelSetContainer >
typename LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer >
::Value( const LevelSetInputIndexType& iP,
         const LevelSetDataType& iData )
{
  const LevelSetOutputRealType zero = NumericTraits< LevelSetOutputRealType >::Zero;
  LevelSetOutputRealType propagation_gradient = zero;

  if ( propagation_gradient > zero )
    {
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      propagation_gradient +=
        vnl_math_sqr( vnl_math_max( iData.BackwardGradient.m_Value[i], zero ) ) +
        vnl_math_sqr( vnl_math_min( iData.ForwardGradient.m_Value[i],  zero ) );
      }
    }
  else
    {
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      propagation_gradient +=
        vnl_math_sqr( vnl_math_min( iData.BackwardGradient.m_Value[i], zero ) ) +
        vnl_math_sqr( vnl_math_max( iData.ForwardGradient.m_Value[i],  zero) );
      }
    }
  propagation_gradient *= this->PropagationSpeed( iP );

  return propagation_gradient;
}

}

#endif
