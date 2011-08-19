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


#ifndef __itkLevelSetMalcolmEvolutionBase_hxx
#define __itkLevelSetMalcolmEvolutionBase_hxx

#include "itkLevelSetMalcolmEvolutionBase.h"

namespace itk
{
template< class TEquationContainer >
LevelSetMalcolmEvolutionBase< TEquationContainer >
::LevelSetMalcolmEvolutionBase()
{
  this->m_Alpha = 0.9;
  this->m_Dt = 1.;
  this->m_RMSChangeAccumulator = 0.;
  this->m_UserGloballyDefinedTimeStep = true;
}

template< class TEquationContainer >
LevelSetMalcolmEvolutionBase< TEquationContainer >
::~LevelSetMalcolmEvolutionBase()
{}

template< class TEquationContainer >
void LevelSetMalcolmEvolutionBase< TEquationContainer >
::SetTimeStep( const LevelSetOutputRealType& )
{}


template< class TEquationContainer >
void LevelSetMalcolmEvolutionBase< TEquationContainer >
::AllocateUpdateBuffer()
{}

template< class TEquationContainer >
void LevelSetMalcolmEvolutionBase< TEquationContainer >
::ComputeIteration()
{}

template< class TEquationContainer >
void LevelSetMalcolmEvolutionBase< TEquationContainer >
::ComputeTimeStepForNextIteration()
{}

template< class TEquationContainer >
void LevelSetMalcolmEvolutionBase< TEquationContainer >
::Update()
{
  if( this->m_EquationContainer.IsNull() )
    {
    itkGenericExceptionMacro( << "m_EquationContainer is NULL" );
    }

  if( !m_EquationContainer->GetEquation( 0 ) )
    {
    itkGenericExceptionMacro( << "m_EquationContainer->GetEquation( 0 ) is NULL" );
    }

  // Get the image to be segmented
  InputImageConstPointer inputImage = this->m_EquationContainer->GetInput();

  if( inputImage.IsNull() )
    {
    itkGenericExceptionMacro( << "Input Image is NULL" );
    }

  TermContainerPointer equation0 = this->m_EquationContainer->GetEquation( 0 );
  TermPointer term0 = equation0->GetTerm( 0 );

  // Get the LevelSetContainer from the EquationContainer
  this->m_LevelSetContainer = term0->GetLevelSetContainer();

  if( term0.IsNull() )
    {
    itkGenericExceptionMacro( << "m_EquationContainer->GetEquation( 0 ) is NULL" );
    }

  if( !term0->GetLevelSetContainer() )
    {
    itkGenericExceptionMacro( << "m_LevelSetContainer is NULL" );
    }

  //Run iteration
  this->RunOneIteration();
}

template< class TEquationContainer >
void LevelSetMalcolmEvolutionBase< TEquationContainer >
::RunOneIteration()
{
  this->AllocateUpdateBuffer();

  this->InitializeIteration();

  typename StoppingCriterionType::IterationIdType iter = 0;
  this->m_StoppingCriterion->SetCurrentIteration( iter );
  this->m_StoppingCriterion->SetLevelSetContainer( this->m_LevelSetContainer );

  while( !m_StoppingCriterion->IsSatisfied() )
    {
    this->m_RMSChangeAccumulator = NumericTraits< LevelSetOutputRealType >::Zero;

    // one iteration over all container
    // update each level set based on the different equations provided
    this->ComputeIteration();

    this->ComputeTimeStepForNextIteration();

    this->UpdateLevelSets();
    this->UpdateEquations();

    ++iter;

    this->m_StoppingCriterion->SetRMSChangeAccumulator( this->m_RMSChangeAccumulator );
    this->m_StoppingCriterion->SetCurrentIteration( iter );
    this->InvokeEvent( IterationEvent() );
    }
}

template< class TEquationContainer >
void LevelSetMalcolmEvolutionBase< TEquationContainer >
::InitializeIteration()
{
  // Get the image to be segmented
  InputImageConstPointer inputImage = this->m_EquationContainer->GetInput();

  DomainMapImageFilterPointer domainMapFilter = this->m_LevelSetContainer->GetDomainMapFilter();

  DomainIteratorType map_it   = domainMapFilter->m_LevelSetMap.begin();
  DomainIteratorType map_end  = domainMapFilter->m_LevelSetMap.end();

  // Initialize parameters here
  this->m_EquationContainer->InitializeParameters();

  while( map_it != map_end )
    {
    InputImageConstIteratorType it( inputImage, map_it->second.m_Region );
    it.GoToBegin();

    while( !it.IsAtEnd() )
      {
      IdListType lout = map_it->second.m_List;

      if( lout.empty() )
        {
        itkGenericExceptionMacro( <<"No level set exists at voxel" );
        }

      for( IdListIterator lIt = lout.begin(); lIt != lout.end(); ++lIt )
        {
        TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( *lIt - 1 );
        termContainer->Initialize( it.GetIndex() );
        }
      ++it;
      }
    ++map_it;
    }
  this->m_EquationContainer->UpdateInternalEquationTerms();
}

template< class TEquationContainer >
void LevelSetMalcolmEvolutionBase< TEquationContainer >
::UpdateLevelSets()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();

  while( it != this->m_LevelSetContainer->End() )
    {
    LevelSetPointer         levelSet    = it->GetLevelSet();
    LevelSetIdentifierType  levelSetId  = it->GetIdentifier();

    UpdateLevelSetFilterPointer update_levelset = UpdateLevelSetFilterType::New();
    update_levelset->SetInputLevelSet( levelSet );
    update_levelset->SetCurrentLevelSetId( levelSetId );
    update_levelset->SetEquationContainer( this->m_EquationContainer );
    update_levelset->Update();

    levelSet->Graft( update_levelset->GetOutputLevelSet() );

    this->m_RMSChangeAccumulator = update_levelset->GetRMSChangeAccumulator();

    ++it;
    }
}

template< class TEquationContainer >
void LevelSetMalcolmEvolutionBase< TEquationContainer >
::UpdateEquations()
{
  this->m_EquationContainer->UpdateInternalEquationTerms();
}

}
#endif // __itkLevelSetMalcolmEvolutionBase_hxx
