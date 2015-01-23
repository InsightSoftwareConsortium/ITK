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

#ifndef itkLevelSetEquationChanAndVeseInternalTerm_hxx
#define itkLevelSetEquationChanAndVeseInternalTerm_hxx

#include "itkLevelSetEquationChanAndVeseInternalTerm.h"

namespace itk
{

template< typename TInput, typename TLevelSetContainer >
LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >
::LevelSetEquationChanAndVeseInternalTerm() :
  m_Mean( NumericTraits< InputPixelRealType >::ZeroValue() ),
  m_TotalValue( NumericTraits< InputPixelRealType >::ZeroValue() ),
  m_TotalH( NumericTraits< LevelSetOutputRealType >::ZeroValue() )
{
  this->m_TermName = "Internal Chan And Vese term";
  this->m_RequiredData.insert( "Value" );
}

template< typename TInput, typename TLevelSetContainer >
LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >
::~LevelSetEquationChanAndVeseInternalTerm()
{
}

template< typename TInput, typename TLevelSetContainer >
void LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >
::Update()
{
  if( this->m_TotalH > NumericTraits< LevelSetOutputRealType >::epsilon() )
    {
    const LevelSetOutputRealType inv_total_h = 1. / this->m_TotalH;

    // depending on the pixel type, it may be more efficient to do
    // a multiplication than to do a division
    this->m_Mean = this->m_TotalValue * inv_total_h;
    }
  else
    {
    this->m_Mean = NumericTraits< InputPixelRealType >::ZeroValue();
    }
}

template< typename TInput, typename TLevelSetContainer >
void LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >
::InitializeParameters()
{
  this->m_TotalValue = NumericTraits< InputPixelRealType >::ZeroValue();
  this->m_TotalH = NumericTraits< LevelSetOutputRealType >::ZeroValue();
  this->SetUp();
}


template< typename TInput, typename TLevelSetContainer >
void LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >
::Initialize( const LevelSetInputIndexType& inputIndex )
{
  if( this->m_Heaviside.IsNotNull() )
    {
    InputPixelType pixel = this->m_Input->GetPixel( inputIndex );

    LevelSetOutputRealType prod;
    this->ComputeProduct( inputIndex, prod );
    this->Accumulate( pixel, prod );
    }
  else
    {
    itkWarningMacro( << "m_Heaviside is ITK_NULLPTR" );
    }
}


template< typename TInput, typename TLevelSetContainer >
void LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >
::ComputeProduct( const LevelSetInputIndexType& inputIndex, LevelSetOutputRealType& prod )
{
  LevelSetOutputRealType value = this->m_CurrentLevelSetPointer->Evaluate( inputIndex );
  prod = this->m_Heaviside->Evaluate( -value );
}


template< typename TInput, typename TLevelSetContainer >
void LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >
::UpdatePixel( const LevelSetInputIndexType& inputIndex,
               const LevelSetOutputRealType & oldValue,
               const LevelSetOutputRealType & newValue )
{
  // For each affected h val: h val = new hval (this will dirty some cvals)
  InputPixelType input = this->m_Input->GetPixel( inputIndex );

  const LevelSetOutputRealType oldH = this->m_Heaviside->Evaluate( -oldValue );
  const LevelSetOutputRealType newH = this->m_Heaviside->Evaluate( -newValue );
  const LevelSetOutputRealType change = newH - oldH;

  // update the foreground constant for current level-set function
  this->m_TotalH += change;
  this->m_TotalValue += input * change;
}

template< typename TInput, typename TLevelSetContainer >
typename LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >
::Value( const LevelSetInputIndexType& inputIndex )
{
  if( this->m_Heaviside.IsNotNull() )
    {
    const LevelSetOutputRealType value =
      static_cast< LevelSetOutputRealType >( this->m_CurrentLevelSetPointer->Evaluate( inputIndex ) );

    const LevelSetOutputRealType d_val = this->m_Heaviside->EvaluateDerivative( -value );

    const InputPixelType pixel = this->m_Input->GetPixel( inputIndex );
    LevelSetOutputRealType prod = 1;

    this->ComputeProductTerm( inputIndex, prod );

    const LevelSetOutputRealType oValue = d_val * prod *
      static_cast< LevelSetOutputRealType >( ( pixel - this->m_Mean ) * ( pixel - this->m_Mean ) );

    return oValue;
    }
  else
    {
    itkWarningMacro( << "m_Heaviside is ITK_NULLPTR" );
    }
  return NumericTraits< LevelSetOutputPixelType >::ZeroValue();
}

template< typename TInput, typename TLevelSetContainer >
typename LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >
::Value( const LevelSetInputIndexType& inputIndex, const LevelSetDataType& data )
{
  if( this->m_Heaviside.IsNotNull() )
    {
    const LevelSetOutputRealType value = data.Value.m_Value;

    const LevelSetOutputRealType d_val = this->m_Heaviside->EvaluateDerivative( -value );

    const InputPixelType pixel = this->m_Input->GetPixel( inputIndex );

    LevelSetOutputRealType prod = 1;

    this->ComputeProductTerm( inputIndex, prod );

    const LevelSetOutputRealType oValue = d_val * prod *
      static_cast< LevelSetOutputRealType >( ( pixel - this->m_Mean ) * ( pixel - this->m_Mean ) );

    return oValue;
    }
  else
    {
    itkWarningMacro( << "m_Heaviside is ITK_NULLPTR" );
    }
  return NumericTraits< LevelSetOutputPixelType >::ZeroValue();
}

template< typename TInput, typename TLevelSetContainer >
void LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >
::Accumulate( const InputPixelType& inputPixel, const LevelSetOutputRealType& heavisideValue )
{
  this->m_TotalValue += static_cast< InputPixelRealType >( inputPixel ) *
      static_cast< LevelSetOutputRealType >( heavisideValue );
  this->m_TotalH += static_cast< LevelSetOutputRealType >( heavisideValue );
}

}
#endif
