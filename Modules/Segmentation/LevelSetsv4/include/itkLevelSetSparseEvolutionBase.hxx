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


#ifndef __itkLevelSetSparseEvolutionBase_hxx
#define __itkLevelSetSparseEvolutionBase_hxx

#include "itkLevelSetSparseEvolutionBase.h"

namespace itk
{
template< class TEquationContainer >
LevelSetSparseEvolutionBase<TEquationContainer>
::LevelSetSparseEvolutionBase()
{
  this->m_Alpha = 0.9;
  this->m_Dt = 1.;
  this->m_RMSChangeAccumulator = 0.;
  this->m_UserDefinedDt = false;
}

template< class TEquationContainer >
LevelSetSparseEvolutionBase<TEquationContainer>
::~LevelSetSparseEvolutionBase()
{
  typename LevelSetContainerType::ConstIterator it = this->m_LevelSetContainer->Begin();
  while( it != this->m_LevelSetContainer->End() )
    {
    delete this->m_UpdateBuffer[ it->GetIdentifier() ];
    ++it;
    }
}

template< class TEquationContainer >
void
LevelSetSparseEvolutionBase<TEquationContainer>
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
    itkGenericExceptionMacro( << "input Image is NULL" );
    }

  // Get the LevelSetContainer from the EquationContainer
  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( 0 );

  TermPointer term0 = termContainer->GetTerm( 0 );

  this->m_LevelSetContainer = term0->GetLevelSetContainer();

  if( this->m_StoppingCriterion.IsNull() )
    {
    itkGenericExceptionMacro( << "m_StoppingCriterion is NULL" );
    }

  //Run iteration
  this->RunOneIteration();
}

template< class TEquationContainer >
void
LevelSetSparseEvolutionBase<TEquationContainer>
::SetTimeStep( const LevelSetOutputRealType& iDt )
{
  if( iDt > NumericTraits< LevelSetOutputRealType >::epsilon() )
    {
    this->m_UserDefinedDt = true;
    this->m_Dt = iDt;
    this->Modified();
    }
  else
    {
    itkGenericExceptionMacro( <<"iDt should be > epsilon")
    }
}


template< class TEquationContainer >
void
LevelSetSparseEvolutionBase<TEquationContainer>
::AllocateUpdateBuffer()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();
  while( it != this->m_LevelSetContainer->End() )
    {
    IdentifierType id = it->GetIdentifier();

    if( this->m_UpdateBuffer.find( id ) == this->m_UpdateBuffer.end() )
      {
      this->m_UpdateBuffer[ id ] = new LevelSetLayerType;
      }
    else
      {
      if( this->m_UpdateBuffer[ id ] )
        {
        this->m_UpdateBuffer[ id ]->clear();
        }
      else
        {
        this->m_UpdateBuffer[ id ] = new LevelSetLayerType;
        }
      }
    ++it;
    }
}

template< class TEquationContainer >
void
LevelSetSparseEvolutionBase<TEquationContainer>
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
void
LevelSetSparseEvolutionBase<TEquationContainer>
::InitializeIteration()
{
  InputImageConstPointer inputImage = this->m_EquationContainer->GetInput();

  DomainMapImageFilterPointer domainMapFilter = this->m_LevelSetContainer->GetDomainMapFilter();

  DomainIteratorType map_it  = domainMapFilter->m_LevelSetMap.begin();
  DomainIteratorType map_end = domainMapFilter->m_LevelSetMap.end();

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
void
LevelSetSparseEvolutionBase<TEquationContainer>
::ComputeIteration()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();

  while( it != this->m_LevelSetContainer->End() )
    {
    LevelSetPointer levelSet = it->GetLevelSet();

    LevelSetIdentifierType levelSetId = it->GetIdentifier();
    TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( levelSetId );

    LevelSetLayerIterator list_it = levelSet->GetLayer( 0 ).begin();
    LevelSetLayerIterator list_end = levelSet->GetLayer( 0 ).end();

    while( list_it != list_end )
      {
      const LevelSetInputType idx = list_it->first;

      LevelSetDataType characteristics;

      termContainer->ComputeRequiredData( idx, characteristics );

      const LevelSetOutputType temp_update =
          static_cast< LevelSetOutputType >( termContainer->Evaluate( idx, characteristics ) );

      this->m_UpdateBuffer[ levelSetId ]->insert(
            NodePairType( idx, temp_update ) );

      ++list_it;
      }
    ++it;
    }
}

template< class TEquationContainer >
void
LevelSetSparseEvolutionBase<TEquationContainer>
::ComputeTimeStepForNextIteration()
{
  if( !m_UserDefinedDt )
    {
    if( ( this->m_Alpha > NumericTraits< LevelSetOutputRealType >::Zero ) &&
        ( this->m_Alpha < NumericTraits< LevelSetOutputRealType >::One ) )
      {
      LevelSetOutputRealType contribution = this->m_EquationContainer->ComputeCFLContribution();

      if( contribution > NumericTraits< LevelSetOutputRealType >::epsilon() )
        {
        this->m_Dt = this->m_Alpha / contribution;
        }
      else
        {
        if( contribution == NumericTraits< LevelSetOutputRealType >::max() )
          {
          itkGenericExceptionMacro( << "contribution is " << contribution );
          }
        else
          {
          itkGenericExceptionMacro( << "contribution is too low " << contribution );
          }
        }
      }
    else
      {
      itkGenericExceptionMacro( <<"m_Alpha should be in ]0,1[" );
      }
  }
}

template< class TEquationContainer >
void
LevelSetSparseEvolutionBase<TEquationContainer>
::UpdateLevelSets()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();
  while( it != this->m_LevelSetContainer->End() )
    {
    LevelSetPointer levelSet = it->GetLevelSet();

    UpdateLevelSetFilterPointer update_levelset = UpdateLevelSetFilterType::New();
    update_levelset->SetInputLevelSet( levelSet );
    update_levelset->SetUpdate( * this->m_UpdateBuffer[it->GetIdentifier()] );
    update_levelset->SetEquationContainer( this->m_EquationContainer );
    update_levelset->SetTimeStep( this->m_Dt );
    update_levelset->SetCurrentLevelSetId( it->GetIdentifier() );
    update_levelset->Update();

    levelSet->Graft( update_levelset->GetOutputLevelSet() );

    this->m_RMSChangeAccumulator = update_levelset->GetRMSChangeAccumulator();

    this->m_UpdateBuffer[it->GetIdentifier()]->clear();
    ++it;
    }
}

template< class TEquationContainer >
void
LevelSetSparseEvolutionBase<TEquationContainer>
::UpdateEquations()
{
  this->m_EquationContainer->UpdateInternalEquationTerms();
}

}
#endif // __itkLevelSetSparseEvolutionBase_hxx
