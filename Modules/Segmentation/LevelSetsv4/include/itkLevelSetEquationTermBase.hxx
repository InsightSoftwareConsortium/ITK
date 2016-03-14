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

#ifndef itkLevelSetEquationTermBase_hxx
#define itkLevelSetEquationTermBase_hxx

#include "itkLevelSetEquationTermBase.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

namespace itk
{
// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::LevelSetEquationTermBase()
{
  this->m_CurrentLevelSetId = LevelSetIdentifierType();

  this->m_Coefficient = NumericTraits< LevelSetOutputRealType >::OneValue();
  this->m_CFLContribution = NumericTraits< LevelSetOutputRealType >::ZeroValue();
  this->m_TermName = "";
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::~LevelSetEquationTermBase()
{
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
const typename LevelSetEquationTermBase< TInputImage, TLevelSetContainer >::RequiredDataType &
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::GetRequiredData() const
{
  return this->m_RequiredData;
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
void
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::SetLevelSetContainer( LevelSetContainerType* iContainer )
{
  if( iContainer )
    {
    this->m_LevelSetContainer = iContainer;
    this->m_Heaviside = iContainer->GetHeaviside();
    this->Modified();
    }
  else
    {
    itkGenericExceptionMacro( << "iContainer is ITK_NULLPTR" );
    }
}

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
typename
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::LevelSetOutputRealType
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::Evaluate( const LevelSetInputIndexType& iP )
{
  if( itk::Math::abs( this->m_Coefficient ) > NumericTraits< LevelSetOutputRealType >::epsilon() )
    {
    return this->m_Coefficient * this->Value( iP );
    }
  else
    {
    return NumericTraits< LevelSetOutputRealType >::ZeroValue();
    }
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
typename
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::LevelSetOutputRealType
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::Evaluate( const LevelSetInputIndexType& iP,
            const LevelSetDataType& iData )
{
  if( itk::Math::abs( this->m_Coefficient ) > NumericTraits< LevelSetOutputRealType >::epsilon() )
    {
    return this->m_Coefficient * this->Value( iP, iData );
    }
  else
    {
    return NumericTraits< LevelSetOutputRealType >::ZeroValue();
    }
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< typename TInputImage, typename TLevelSetContainer >
void
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::SetUp()
{
  this->m_CFLContribution = NumericTraits< LevelSetOutputRealType >::ZeroValue();

  if( this->m_CurrentLevelSetPointer.IsNull() )
    {
    if( this->m_LevelSetContainer.IsNull() )
      {
      itkGenericExceptionMacro( <<"m_LevelSetContainer is ITK_NULLPTR" );
      }
    this->m_CurrentLevelSetPointer = this->m_LevelSetContainer->GetLevelSet( this->m_CurrentLevelSetId );

    if( this->m_CurrentLevelSetPointer.IsNull() )
      {
      itkWarningMacro(
      << "m_CurrentLevelSetId does not exist in the level set container" );
      }
    }

  if( !this->m_Heaviside.IsNotNull() )
    {
    itkWarningMacro( << "m_Heaviside is ITK_NULLPTR" );
    }
}
// ----------------------------------------------------------------------------

}

#endif // itkLevelSetEquationTermBase_hxx
