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

#ifndef itkLevelSetEquationPropagationTerm_hxx
#define itkLevelSetEquationPropagationTerm_hxx

#include "itkLevelSetEquationPropagationTerm.h"
#include "itkCastImageFilter.h"

namespace itk
{
template< typename TInput, typename TLevelSetContainer, typename TPropagationImage >
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >
::LevelSetEquationPropagationTerm()
{
  this->m_TermName = "Propagation term";
  this->m_RequiredData.insert( "BackwardGradient" );
  this->m_RequiredData.insert( "ForwardGradient" );
}

template< typename TInput, typename TLevelSetContainer, typename TPropagationImage >
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >
::~LevelSetEquationPropagationTerm()
{
}


template< typename TInput, typename TLevelSetContainer, typename TPropagationImage >
void
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >
::InitializeParameters()
{
  this->SetUp();

  if( this->m_PropagationImage.IsNull() )
    {
    typedef CastImageFilter< TInput, TPropagationImage > CastFilterType;
    typename CastFilterType::Pointer castFilter = CastFilterType::New();
    castFilter->SetInput( this->m_Input );
    castFilter->Update();

    this->m_PropagationImage = castFilter->GetOutput();
    }
}

template< typename TInput, typename TLevelSetContainer, typename TPropagationImage >
void
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >
::Initialize( const LevelSetInputIndexType& )
{
}

template< typename TInput, typename TLevelSetContainer, typename TPropagationImage >
void
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >
::Update()
{
}

template< typename TInput, typename TLevelSetContainer, typename TPropagationImage >
void
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >
::UpdatePixel( const LevelSetInputIndexType& itkNotUsed( iP ),
               const LevelSetOutputRealType& itkNotUsed( oldValue ),
               const LevelSetOutputRealType& itkNotUsed( newValue ) )
{
}

template< typename TInput, typename TLevelSetContainer, typename TPropagationImage >
typename LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >::LevelSetOutputRealType
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >
::PropagationSpeed( const LevelSetInputIndexType& iP ) const
{
  return ( static_cast< LevelSetOutputRealType >( this->m_PropagationImage->GetPixel(iP) ) );
}

template< typename TInput, typename TLevelSetContainer, typename TPropagationImage >
typename LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >::LevelSetOutputRealType
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >
::Value( const LevelSetInputIndexType& iP )
{
  LevelSetGradientType backwardGradient = this->m_CurrentLevelSetPointer->EvaluateBackwardGradient( iP );
  LevelSetGradientType forwardGradient  = this->m_CurrentLevelSetPointer->EvaluateForwardGradient( iP );

  const LevelSetOutputRealType zero = NumericTraits< LevelSetOutputRealType >::ZeroValue();

  //
  // Construct upwind gradient values for use in the propagation speed term:
  //  $\beta G(\mathbf{x})\mid\nabla\phi\mid$
  //
  // The following scheme for "upwinding" in the normal direction is taken
  // from Sethian, Ch. 6 as referenced above.411
  //
  LevelSetOutputRealType propagation_gradient = zero;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    propagation_gradient +=
      itk::Math::sqr( std::max( backwardGradient[i], zero ) ) +
      itk::Math::sqr( std::min( forwardGradient[i],  zero ) );
    }

  propagation_gradient *= this->PropagationSpeed( iP );

  return propagation_gradient;
}

template< typename TInput, typename TLevelSetContainer, typename TPropagationImage >
typename LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >::LevelSetOutputRealType
LevelSetEquationPropagationTerm< TInput, TLevelSetContainer, TPropagationImage >
::Value( const LevelSetInputIndexType& iP,
         const LevelSetDataType& iData )
{
  const LevelSetOutputRealType zero = NumericTraits< LevelSetOutputRealType >::ZeroValue();
  LevelSetOutputRealType propagation_gradient = zero;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    propagation_gradient +=
      itk::Math::sqr( std::max( iData.BackwardGradient.m_Value[i], zero ) ) +
      itk::Math::sqr( std::min( iData.ForwardGradient.m_Value[i],  zero ) );
    }

  propagation_gradient *= this->PropagationSpeed( iP );

  return propagation_gradient;
}

}

#endif
