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

#ifndef __itkLevelSetEquationCurvatureTerm_hxx
#define __itkLevelSetEquationCurvatureTerm_hxx

#include "itkLevelSetEquationCurvatureTerm.h"

namespace itk
{
template< class TInput, class TLevelSetContainer >
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::LevelSetEquationCurvatureTerm()
{
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    this->m_NeighborhoodScales[i] = 1.0;
    }
  this->m_TermName = "Curvature term";
  this->m_RequiredData.insert( "MeanCurvature" );
}

template< class TInput, class TLevelSetContainer >
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::~LevelSetEquationCurvatureTerm()
{
}

template< class TInput, class TLevelSetContainer >
typename LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::Value( const LevelSetInputIndexType& itkNotUsed(iP), const LevelSetDataType& iData )
{
  // MeanCurvature has should be computed by this point.
  itkAssertInDebugAndIgnoreInReleaseMacro( iData.MeanCurvature.m_Computed == true );

  return iData.MeanCurvature.m_Value;
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::InitializeParameters()
{
  this->SetUp();
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::Initialize( const LevelSetInputIndexType& )
{
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::Update()
{
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::UpdatePixel( const LevelSetInputIndexType& itkNotUsed( iP ),
               const LevelSetOutputRealType& itkNotUsed( oldValue ),
               const LevelSetOutputRealType& itkNotUsed( newValue ) )
{
}

template< class TInput, class TLevelSetContainer >
typename LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::Value( const LevelSetInputIndexType& iP )
{
  return this->m_CurrentLevelSetPointer->EvaluateMeanCurvature( iP );
}

}
#endif
