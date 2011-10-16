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


#ifndef __itkLevelSetEvolutionBase_hxx
#define __itkLevelSetEvolutionBase_hxx

#include "itkLevelSetEvolutionBase.h"

namespace itk
{
template< class TEquationContainer, class TLevelSet >
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::LevelSetEvolutionBase()
{
  this->m_Alpha = 0.9;
  this->m_Dt = 1.;
  this->m_RMSChangeAccumulator = 0.;
  this->m_UserGloballyDefinedTimeStep = false;
}

template< class TEquationContainer, class TLevelSet >
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::~LevelSetEvolutionBase()
{}

template< class TEquationContainer, class TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::SetTimeStep( const LevelSetOutputRealType& iDt )
{
  if( iDt > NumericTraits< LevelSetOutputRealType >::epsilon() )
    {
    this->m_UserGloballyDefinedTimeStep = true;
    this->m_Dt = iDt;
    this->Modified();
    }
  else
    {
    itkGenericExceptionMacro( <<"iDt should be > epsilon")
    }
}

template< class TEquationContainer, class TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::CheckSetUp()
{
  if( this->m_LevelSetContainer.IsNull() )
    {
    itkGenericExceptionMacro( << "this->m_LevelSetContainer is NULL" );
    }

  if( this->m_EquationContainer.IsNull() )
    {
    itkGenericExceptionMacro( << "m_EquationContainer is NULL" );
    }

  typename EquationContainerType::Iterator eqIt = this->m_EquationContainer->Begin();

  if( eqIt == this->m_EquationContainer->End() )
    {
    itkGenericExceptionMacro( <<"this->m_EquationContainer is empty" );
    }
  if( !eqIt->GetEquation() )
    {
    itkGenericExceptionMacro( << "m_EquationContainer->GetEquation( 0 ) is NULL" );
    }

  if( this->m_LevelSetContainer != this->m_EquationContainer->GetLevelSetContainer() )
    {
    itkGenericExceptionMacro( << "this->m_LevelSetContainer != this->m_EquationContainer->GetLevelSetContainer()");
    }

  // Get the image to be segmented
  InputImageConstPointer inputImage = this->m_EquationContainer->GetInput();

  if( inputImage.IsNull() )
    {
    itkGenericExceptionMacro( << "input Image is NULL" );
    }

  // Get the LevelSetContainer from the EquationContainer
  TermContainerPointer termContainer = eqIt->GetEquation();
  typename TermContainerType::Iterator termIt = termContainer->Begin();

  if( termIt == termContainer->End() )
    {
    itkGenericExceptionMacro( << "TermContainer is empty" );
    }

  if( this->m_LevelSetContainer != termContainer->GetLevelSetContainer() )
    {
    itkGenericExceptionMacro( << "this->m_LevelSetContainer != termContainer->GetLevelSetContainer()" );
    }

  TermPointer term = termIt->GetTerm();

  if( this->m_LevelSetContainer != term->GetLevelSetContainer() )
    {
    itkGenericExceptionMacro( << "this->m_LevelSetContainer != term->GetLevelSetContainer()" );
    }

  if( this->m_StoppingCriterion.IsNull() )
    {
    itkGenericExceptionMacro( << "m_StoppingCriterion is NULL" );
    }
}

}
#endif // __itkLevelSetEvolutionBase_hxx
