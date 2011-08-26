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

#ifndef __itkLevelSetEquationTermBase_hxx
#define __itkLevelSetEquationTermBase_hxx

#include "itkLevelSetEquationTermBase.h"

#include "itkNumericTraits.h"

namespace itk
{
// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::LevelSetEquationTermBase(): Superclass(),
  m_Input( NULL ), m_LevelSetContainer( NULL ),
  m_CurrentLevelSet( NumericTraits< LevelSetIdentifierType >::Zero ),
  m_CurrentLevelSetPointer( NULL ),
  m_Coefficient( NumericTraits< LevelSetOutputRealType >::One ),
  m_CFLContribution( NumericTraits< LevelSetOutputRealType >::Zero ),
  m_Heaviside( NULL ),
  m_TermName( )
{
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
void
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::SetLevelSetContainer( LevelSetContainerType* iContainer )
{
  m_LevelSetContainer = iContainer;
  m_Heaviside = iContainer->GetHeaviside();
  this->Modified();
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
typename
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::LevelSetOutputRealType
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::Evaluate( const LevelSetInputIndexType& iP )
{
  return m_Coefficient * this->Value( iP );
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
typename
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::LevelSetOutputRealType
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::Evaluate( const LevelSetInputIndexType& iP,
            const LevelSetDataType& iData )
{
  return m_Coefficient * this->Value( iP, iData );
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TInputImage, class TLevelSetContainer >
void
LevelSetEquationTermBase< TInputImage, TLevelSetContainer >
::SetUp()
{
  this->m_CFLContribution = NumericTraits< LevelSetOutputRealType >::Zero;
  this->SetDefaultTermName();

  if( m_CurrentLevelSetPointer.IsNull() )
    {
    m_CurrentLevelSetPointer =
    this->m_LevelSetContainer->GetLevelSet( this->m_CurrentLevelSet );

    if( m_CurrentLevelSetPointer.IsNull() )
      {
      itkWarningMacro(
      << "m_CurrentLevelSet does not exist in the level set container" );
      }
    }

  if( !this->m_Heaviside.IsNotNull() )
    {
    itkWarningMacro( << "m_Heaviside is NULL" );
    }
}
// ----------------------------------------------------------------------------

}

#endif // __itkLevelSetEquationTermBase_hxx
