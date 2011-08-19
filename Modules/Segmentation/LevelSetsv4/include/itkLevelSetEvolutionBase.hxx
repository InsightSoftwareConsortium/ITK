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
template< class TEquationContainer >
LevelSetEvolutionBase< TEquationContainer >
::LevelSetEvolutionBase()
{
  this->m_Alpha = 0.9;
  this->m_Dt = 1.;
  this->m_RMSChangeAccumulator = 0.;
  this->m_UserGloballyDefinedTimeStep = false;
}

template< class TEquationContainer >
LevelSetEvolutionBase< TEquationContainer >
::~LevelSetEvolutionBase()
{}

template< class TEquationContainer >
void
LevelSetEvolutionBase< TEquationContainer >::Update()
{
  //Run iteration
  this->RunOneIteration();
}

template< class TEquationContainer >
void
LevelSetEvolutionBase< TEquationContainer >
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


template< class TEquationContainer >
void
LevelSetEvolutionBase< TEquationContainer >
::AllocateUpdateBuffer()
{
  this->m_UpdateBuffer = LevelSetContainerType::New();
  this->m_UpdateBuffer->CopyInformationAndAllocate( this->m_LevelSetContainer, true );
}

template< class TEquationContainer >
void
LevelSetEvolutionBase< TEquationContainer >
::RunOneIteration()
{
  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( 0 );

  TermPointer term = termContainer->GetTerm( 0 );

  // Get the LevelSetContainer from the EquationContainer
  this->m_LevelSetContainer = term->GetLevelSetContainer();

  this->AllocateUpdateBuffer();

  this->InitializeIteration();

  typename StoppingCriterionType::IterationIdType iter = 0;
  this->m_StoppingCriterion->SetCurrentIteration( iter );
  this->m_StoppingCriterion->SetLevelSetContainer( this->m_LevelSetContainer );

  while( !this->m_StoppingCriterion->IsSatisfied() )
    {
    this->m_RMSChangeAccumulator = 0;

    // one iteration over all container
    // update each level set based on the different equations provided
    this->ComputeIteration();

    //       ComputeCFL();

    this->ComputeTimeStepForNextIteration();

    this->UpdateLevelSets();
    this->Reinitialize();
    this->UpdateEquations();

    ++iter;

    this->m_StoppingCriterion->SetRMSChangeAccumulator( this->m_RMSChangeAccumulator );
    this->m_StoppingCriterion->SetCurrentIteration( iter );

    this->InvokeEvent( IterationEvent() );
    }
}

template< class TEquationContainer >
void
LevelSetEvolutionBase< TEquationContainer >
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
void
LevelSetEvolutionBase< TEquationContainer >
::ComputeIteration()
{
  InputImageConstPointer inputImage = this->m_EquationContainer->GetInput();

  DomainMapImageFilterPointer domainMapFilter = this->m_LevelSetContainer->GetDomainMapFilter();

  DomainIteratorType map_it   = domainMapFilter->m_LevelSetMap.begin();
  DomainIteratorType map_end  = domainMapFilter->m_LevelSetMap.end();

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
        LevelSetPointer levelSetUpdate = this->m_UpdateBuffer->GetLevelSet( *lIt - 1);

        LevelSetDataType characteristics;

        TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( *lIt - 1 );
        termContainer->ComputeRequiredData( it.GetIndex(), characteristics );

        LevelSetOutputRealType temp_update = termContainer->Evaluate( it.GetIndex(), characteristics );

        LevelSetImageType* levelSetImage = levelSetUpdate->GetImage();
        levelSetImage->SetPixel( it.GetIndex(), temp_update );
        }
      ++it;
      }
      ++map_it;
    }
}

template< class TEquationContainer >
void
LevelSetEvolutionBase< TEquationContainer >
::ComputeTimeStepForNextIteration()
{
  // if the time step is not globally set
  if( !this->m_UserGloballyDefinedTimeStep )
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
          itkGenericExceptionMacro( << "contribution is too low" );
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
LevelSetEvolutionBase< TEquationContainer >
::UpdateLevelSets()
{
  typename LevelSetContainerType::Iterator it1 = this->m_LevelSetContainer->Begin();
  typename LevelSetContainerType::ConstIterator it2 = this->m_UpdateBuffer->Begin();

  LevelSetOutputRealType p;

  while( it1 != this->m_LevelSetContainer->End() )
    {
    LevelSetPointer ls1 = it1->GetLevelSet();
    LevelSetPointer ls2 = it2->GetLevelSet();

    LevelSetImagePointer image1 = ls1->GetImage();
    LevelSetImagePointer image2 = ls2->GetImage();

    LevelSetImageIteratorType imIt1( image1, image1->GetBufferedRegion() );
    LevelSetImageIteratorType imIt2( image2, image2->GetBufferedRegion() );
    imIt1.GoToBegin();
    imIt2.GoToBegin();

    while( !imIt1.IsAtEnd() )
      {
      p = this->m_Dt * imIt2.Get();
      imIt1.Set( imIt1.Get() + p );

      this->m_RMSChangeAccumulator += p*p;

      ++imIt1;
      ++imIt2;
      }

    ++it1;
    ++it2;
    }
}

template< class TEquationContainer >
void
LevelSetEvolutionBase< TEquationContainer >
::UpdateEquations()
{
  this->InitializeIteration();
}

template< class TEquationContainer >
void
LevelSetEvolutionBase< TEquationContainer >
::Reinitialize()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();

  while( it != this->m_LevelSetContainer->End() )
    {
    LevelSetImagePointer image = it->GetLevelSet()->GetImage();

    ThresholdFilterPointer thresh = ThresholdFilterType::New();
    thresh->SetLowerThreshold( NumericTraits< LevelSetOutputRealType >::NonpositiveMin() );
    thresh->SetUpperThreshold( NumericTraits< LevelSetOutputRealType >::Zero );
    thresh->SetInsideValue( NumericTraits< LevelSetOutputRealType >::One );
    thresh->SetOutsideValue( NumericTraits< LevelSetOutputRealType >::Zero );
    thresh->SetInput( image );
    thresh->Update();

    MaurerPointer maurer = MaurerType::New();
    maurer->SetInput( thresh->GetOutput() );
    maurer->SetSquaredDistance( false );
    maurer->SetUseImageSpacing( true );
    maurer->SetInsideIsPositive( false );

    maurer->Update();

    image->Graft( maurer->GetOutput() );

    ++it;
    }
}

}
#endif // __itkLevelSetEvolutionBase_hxx
