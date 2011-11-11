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


#ifndef __itkLevelSetEvolution_hxx
#define __itkLevelSetEvolution_hxx

#include "itkLevelSetEvolution.h"

namespace itk
{
template< class TEquationContainer, class TImage >
LevelSetEvolution< TEquationContainer, LevelSetDenseImageBase< TImage > >
::LevelSetEvolution()
{}

template< class TEquationContainer, class TImage >
LevelSetEvolution< TEquationContainer, LevelSetDenseImageBase< TImage > >
::~LevelSetEvolution()
{}

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImageBase< TImage > >::Update()
{
  this->CheckSetUp();

  //Run iteration
  this->RunOneIteration();
}

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImageBase< TImage > >
::AllocateUpdateBuffer()
{
  this->m_UpdateBuffer = LevelSetContainerType::New();
  this->m_UpdateBuffer->CopyInformationAndAllocate( this->m_LevelSetContainer, true );
}

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImageBase< TImage > >
::RunOneIteration()
{
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

    this->m_NumberOfIterations++;
    this->InvokeEvent( IterationEvent() );
    }
}

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImageBase< TImage > >
::ComputeIteration()
{
  InputImageConstPointer inputImage = this->m_EquationContainer->GetInput();

  DomainMapImageFilterPointer domainMapFilter = this->m_LevelSetContainer->GetDomainMapFilter();

  if( !domainMapFilter.IsNull() && domainMapFilter->GetDomainMap().size() > 0 )
    {
    typedef typename DomainMapImageFilterType::DomainMapType DomainMapType;
    const DomainMapType domainMap = domainMapFilter->GetDomainMap();
    typename DomainMapType::const_iterator map_it   = domainMap.begin();
    typename DomainMapType::const_iterator map_end  = domainMap.end();

    while( map_it != map_end )
      {
      ImageRegionConstIteratorWithIndex< InputImageType > it( inputImage, map_it->second.m_Region );
      it.GoToBegin();

      while( !it.IsAtEnd() )
        {
        IdListType lout = map_it->second.m_List;

        itkAssertInDebugAndIgnoreInReleaseMacro( !lout.empty() );

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
  else // assume there is one level set that covers the RequestedRegion of the InputImage
    {
    ImageRegionConstIteratorWithIndex< InputImageType > it( inputImage, inputImage->GetRequestedRegion() );
    it.GoToBegin();
    while( !it.IsAtEnd() )
      {
      LevelSetPointer levelSetUpdate = this->m_UpdateBuffer->GetLevelSet( 0 );

      LevelSetDataType characteristics;

      TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( 0 );
      termContainer->ComputeRequiredData( it.GetIndex(), characteristics );

      LevelSetOutputRealType temp_update = termContainer->Evaluate( it.GetIndex(), characteristics );

      LevelSetImageType* levelSetImage = levelSetUpdate->GetImage();
      levelSetImage->SetPixel( it.GetIndex(), temp_update );
      ++it;
      }
    }
}

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImageBase< TImage > >
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

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImageBase< TImage > >
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

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImageBase< TImage > >
::UpdateEquations()
{
  this->InitializeIteration();
}

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImageBase< TImage > >
::Reinitialize()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();

  while( it != this->m_LevelSetContainer->End() )
    {
    LevelSetImagePointer image = it->GetLevelSet()->GetImage();

    ThresholdFilterPointer thresh = ThresholdFilterType::New();
    thresh->SetLowerThreshold( NumericTraits< LevelSetOutputType >::NonpositiveMin() );
    thresh->SetUpperThreshold( NumericTraits< LevelSetOutputType >::Zero );
    thresh->SetInsideValue( NumericTraits< LevelSetOutputType >::One );
    thresh->SetOutsideValue( NumericTraits< LevelSetOutputType >::Zero );
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


// Whitaker --------------------------------------------------------------------
template< class TEquationContainer, typename TOutput, unsigned int VDimension >
LevelSetEvolution< TEquationContainer, WhitakerSparseLevelSetImage< TOutput, VDimension > >
::LevelSetEvolution()
{
}

template< class TEquationContainer, typename TOutput, unsigned int VDimension >
LevelSetEvolution< TEquationContainer, WhitakerSparseLevelSetImage< TOutput, VDimension > >
::~LevelSetEvolution()
{
  typename LevelSetContainerType::ConstIterator it = this->m_LevelSetContainer->Begin();
  while( it != this->m_LevelSetContainer->End() )
    {
    delete this->m_UpdateBuffer[ it->GetIdentifier() ];
    ++it;
    }
}

template< class TEquationContainer, typename TOutput, unsigned int VDimension >
void
LevelSetEvolution< TEquationContainer, WhitakerSparseLevelSetImage< TOutput, VDimension > >
::Update()
{
  this->CheckSetUp();

  //Run iteration
  this->RunOneIteration();
}

template< class TEquationContainer, typename TOutput, unsigned int VDimension >
void
LevelSetEvolution< TEquationContainer, WhitakerSparseLevelSetImage< TOutput, VDimension > >
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

template< class TEquationContainer, typename TOutput, unsigned int VDimension >
void
LevelSetEvolution< TEquationContainer, WhitakerSparseLevelSetImage< TOutput, VDimension > >
::RunOneIteration()
{
  this->AllocateUpdateBuffer();

  this->InitializeIteration();

  typename StoppingCriterionType::IterationIdType iter = 0;
  this->m_StoppingCriterion->SetCurrentIteration( iter );
  this->m_StoppingCriterion->SetLevelSetContainer( this->m_LevelSetContainer );

  while( !this->m_StoppingCriterion->IsSatisfied() )
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

    this->m_NumberOfIterations++;
    this->InvokeEvent( IterationEvent() );
    }
}

template< class TEquationContainer, typename TOutput, unsigned int VDimension >
void
LevelSetEvolution< TEquationContainer, WhitakerSparseLevelSetImage< TOutput, VDimension > >
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

template< class TEquationContainer, typename TOutput, unsigned int VDimension >
void
LevelSetEvolution< TEquationContainer, WhitakerSparseLevelSetImage< TOutput, VDimension > >
::ComputeTimeStepForNextIteration()
{
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

template< class TEquationContainer, typename TOutput, unsigned int VDimension >
void
LevelSetEvolution< TEquationContainer, WhitakerSparseLevelSetImage< TOutput, VDimension > >
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

template< class TEquationContainer, typename TOutput, unsigned int VDimension >
void
LevelSetEvolution< TEquationContainer, WhitakerSparseLevelSetImage< TOutput, VDimension > >
::UpdateEquations()
{
  this->m_EquationContainer->UpdateInternalEquationTerms();
}

// Shi
template< class TEquationContainer, unsigned int VDimension >
LevelSetEvolution< TEquationContainer, ShiSparseLevelSetImage< VDimension > >
::LevelSetEvolution()
{
}

template< class TEquationContainer, unsigned int VDimension >
LevelSetEvolution< TEquationContainer, ShiSparseLevelSetImage< VDimension > >
::~LevelSetEvolution()
{}


template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, ShiSparseLevelSetImage< VDimension > >
::AllocateUpdateBuffer()
{}

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, ShiSparseLevelSetImage< VDimension > >
::Update()
{
  this->CheckSetUp();

  //Run iteration
  this->RunOneIteration();
}

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, ShiSparseLevelSetImage< VDimension > >
::RunOneIteration()
{
  this->AllocateUpdateBuffer();

  this->InitializeIteration();

  typename StoppingCriterionType::IterationIdType iter = 0;
  this->m_StoppingCriterion->SetCurrentIteration( iter );
  this->m_StoppingCriterion->SetLevelSetContainer( this->m_LevelSetContainer );

  while( !this->m_StoppingCriterion->IsSatisfied() )
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

    this->m_NumberOfIterations++;
    this->InvokeEvent( IterationEvent() );
    }
}

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, ShiSparseLevelSetImage< VDimension > >
::ComputeIteration()
{
}

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, ShiSparseLevelSetImage< VDimension > >
::ComputeTimeStepForNextIteration()
{
}


template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, ShiSparseLevelSetImage< VDimension > >
::UpdateLevelSets()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();

  while( it != this->m_LevelSetContainer->End() )
    {
    LevelSetPointer levelSet = it->GetLevelSet();

    UpdateLevelSetFilterPointer update_levelset = UpdateLevelSetFilterType::New();
    update_levelset->SetInputLevelSet( levelSet );
    update_levelset->SetCurrentLevelSetId( it->GetIdentifier() );
    update_levelset->SetEquationContainer( this->m_EquationContainer );
    update_levelset->Update();

    levelSet->Graft( update_levelset->GetOutputLevelSet() );

    this->m_RMSChangeAccumulator = update_levelset->GetRMSChangeAccumulator();

    ++it;
    }
}

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, ShiSparseLevelSetImage< VDimension > >
::UpdateEquations()
{
  this->m_EquationContainer->UpdateInternalEquationTerms();
}

// Malcolm
template< class TEquationContainer, unsigned int VDimension >
LevelSetEvolution< TEquationContainer, MalcolmSparseLevelSetImage< VDimension > >
::LevelSetEvolution()
{
}

template< class TEquationContainer, unsigned int VDimension >
LevelSetEvolution< TEquationContainer, MalcolmSparseLevelSetImage< VDimension > >
::~LevelSetEvolution()
{}

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, MalcolmSparseLevelSetImage< VDimension > >
::AllocateUpdateBuffer()
{}

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, MalcolmSparseLevelSetImage< VDimension > >
::ComputeIteration()
{}

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, MalcolmSparseLevelSetImage< VDimension > >
::ComputeTimeStepForNextIteration()
{}

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, MalcolmSparseLevelSetImage< VDimension > >
::Update()
{
  this->CheckSetUp();

  //Run iteration
  this->RunOneIteration();
}

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, MalcolmSparseLevelSetImage< VDimension > >
::RunOneIteration()
{
  this->AllocateUpdateBuffer();

  this->InitializeIteration();

  typename StoppingCriterionType::IterationIdType iter = 0;
  this->m_StoppingCriterion->SetCurrentIteration( iter );
  this->m_StoppingCriterion->SetLevelSetContainer( this->m_LevelSetContainer );

  while( !this->m_StoppingCriterion->IsSatisfied() )
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

    this->m_NumberOfIterations++;
    this->InvokeEvent( IterationEvent() );
    }
}

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, MalcolmSparseLevelSetImage< VDimension > >
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

template< class TEquationContainer, unsigned int VDimension >
void LevelSetEvolution< TEquationContainer, MalcolmSparseLevelSetImage< VDimension > >
::UpdateEquations()
{
  this->m_EquationContainer->UpdateInternalEquationTerms();
}
}
#endif // __itkLevelSetEvolution_hxx
