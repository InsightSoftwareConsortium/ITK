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

#ifndef __itkLevelSetEquationLaplacianTerm_hxx
#define __itkLevelSetEquationLaplacianTerm_hxx

#include "itkLevelSetEquationLaplacianTerm.h"

namespace itk
{
template< class TInput, class TLevelSetContainer >
LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >
::LevelSetEquationLaplacianTerm()
{
  this->m_TermName = "Laplacian term";
  this->m_RequiredData.insert( "Laplacian" );
}

template< class TInput, class TLevelSetContainer >
LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >
::~LevelSetEquationLaplacianTerm()
{
}


template< class TInput, class TLevelSetContainer >
void
LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >
::InitializeParameters()
{
  this->SetUp();
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >
::Initialize( const LevelSetInputIndexType& )
{
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >
::Update()
{
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >
::UpdatePixel( const LevelSetInputIndexType& itkNotUsed( iP ),
               const LevelSetOutputRealType& itkNotUsed( oldValue ),
               const LevelSetOutputRealType& itkNotUsed( newValue ) )
{
}

template< class TInput, class TLevelSetContainer >
typename LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >
::LaplacianSpeed( const LevelSetInputIndexType& itkNotUsed(iP) ) const
{
  return NumericTraits< LevelSetOutputRealType >::One;
}

template< class TInput, class TLevelSetContainer >
typename LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >
::Value( const LevelSetInputIndexType& iP )
{
  LevelSetOutputRealType laplacian = this->m_CurrentLevelSetPointer->EvaluateLaplacian( iP );

  laplacian *= this->LaplacianSpeed( iP );

  return laplacian;
}

template< class TInput, class TLevelSetContainer >
typename LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationLaplacianTerm< TInput, TLevelSetContainer >
::Value( const LevelSetInputIndexType& iP, const LevelSetDataType& iData )
{
  // Laplacian should be computed by this point.
  itkAssertInDebugAndIgnoreInReleaseMacro( iData.Laplacian.m_Computed == true );

  LevelSetOutputRealType laplacian = iData.Laplacian.m_Value;

  laplacian *= this->LaplacianSpeed( iP );

  return laplacian;
}

}

#endif
