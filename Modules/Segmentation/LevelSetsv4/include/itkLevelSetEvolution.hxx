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
LevelSetEvolution< TEquationContainer, LevelSetDenseImage< TImage > >
::LevelSetEvolution()
{
  this->m_SplitLevelSetComputeIterationThreader = SplitLevelSetComputeIterationThreaderType::New();
  this->m_SplitDomainMapComputeIterationThreader = SplitDomainMapComputeIterationThreaderType::New();
  this->m_SplitLevelSetUpdateLevelSetsThreader = SplitLevelSetUpdateLevelSetsThreaderType::New();
}

template< class TEquationContainer, class TImage >
LevelSetEvolution< TEquationContainer, LevelSetDenseImage< TImage > >
::~LevelSetEvolution()
{}

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImage< TImage > >
::AllocateUpdateBuffer()
{
  this->m_UpdateBuffer = LevelSetContainerType::New();
  this->m_UpdateBuffer->CopyInformationAndAllocate( this->m_LevelSetContainer, true );
}

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImage< TImage > >
::ComputeIteration()
{
  InputImageConstPointer inputImage = this->m_EquationContainer->GetInput();

  if( this->m_LevelSetContainer->HasDomainMap() )
    {
    typename DomainMapImageFilterType::ConstPointer domainMapFilter = this->m_LevelSetContainer->GetDomainMapFilter();
    typedef typename DomainMapImageFilterType::DomainMapType DomainMapType;
    const DomainMapType domainMap = domainMapFilter->GetDomainMap();
    typename DomainMapType::const_iterator mapIt   = domainMap.begin();
    typename DomainMapType::const_iterator mapEnd  = domainMap.end();

    const ThreadIdType maximumNumberOfThreads = this->m_SplitDomainMapComputeIterationThreader->GetMaximumNumberOfThreads();
    typedef typename SplitDomainMapComputeIterationThreaderType::DomainType DomainMapDomainType;
    DomainMapDomainType subdomain;
    DomainMapDomainType completeDomain( mapIt, mapEnd );
    const typename SplitDomainMapComputeIterationThreaderType::DomainPartitionerType * domainParitioner = this->m_SplitDomainMapComputeIterationThreader->GetDomainPartitioner();
    const ThreadIdType numberOfThreadsThatWillBeUsed = domainParitioner->PartitionDomain( 0, maximumNumberOfThreads, completeDomain, subdomain );
    // If we do not have enough domains to process one per thread.
    if( numberOfThreadsThatWillBeUsed < maximumNumberOfThreads )
      {
      // split up each level set individually
      while( mapIt != mapEnd )
        {
        typedef typename DomainMapImageFilterType::LevelSetDomain LevelSetListImageDomainType;
        const LevelSetListImageDomainType & levelSetListImageDomain = mapIt->second;
        this->m_IdListToProcessWhenThreading = levelSetListImageDomain.GetIdList();
        this->m_SplitLevelSetComputeIterationThreader->Execute( this, *(levelSetListImageDomain.GetRegion()) );
        ++mapIt;
        }
      }
    else
      {
      // process a level set domain in every thread
      this->m_SplitDomainMapComputeIterationThreader->Execute( this, completeDomain );
      }
    }
  else // assume there is one level set that covers the RequestedRegion of the InputImage
    {
    this->m_SplitLevelSetComputeIterationThreader->Execute( this, inputImage->GetRequestedRegion() );
    }
}

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImage< TImage > >
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
LevelSetEvolution< TEquationContainer, LevelSetDenseImage< TImage > >
::UpdateLevelSets()
{
  this->m_LevelSetContainerIteratorToProcessWhenThreading = this->m_LevelSetContainer->Begin();
  this->m_LevelSetUpdateContainerIteratorToProcessWhenThreading = this->m_UpdateBuffer->Begin();

  while( this->m_LevelSetContainerIteratorToProcessWhenThreading != this->m_LevelSetContainer->End() )
    {
    typename LevelSetType::Pointer levelSet = this->m_LevelSetContainerIteratorToProcessWhenThreading->GetLevelSet();
    typename LevelSetImageType::ConstPointer levelSetImage = levelSet->GetImage();
    this->m_SplitLevelSetUpdateLevelSetsThreader->Execute( this, levelSetImage->GetRequestedRegion() );

    ++(this->m_LevelSetContainerIteratorToProcessWhenThreading);
    ++(this->m_LevelSetUpdateContainerIteratorToProcessWhenThreading);
    }

  this->ReinitializeToSignedDistance();
}

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImage< TImage > >
::UpdateEquations()
{
  this->InitializeIteration();
}

template< class TEquationContainer, class TImage >
void
LevelSetEvolution< TEquationContainer, LevelSetDenseImage< TImage > >
::ReinitializeToSignedDistance()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();

  while( it != this->m_LevelSetContainer->End() )
    {
    typename LevelSetImageType::Pointer image = it->GetLevelSet()->GetModifiableImage();

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
  this->m_SplitLevelSetComputeIterationThreader = SplitLevelSetComputeIterationThreaderType::New();
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
::ComputeIteration()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();

  this->m_LevelSetContainerIteratorToProcessWhenThreading = this->m_LevelSetContainer->Begin();

  while( this->m_LevelSetContainerIteratorToProcessWhenThreading != this->m_LevelSetContainer->End() )
    {
    typename LevelSetType::ConstPointer levelSet = this->m_LevelSetContainerIteratorToProcessWhenThreading->GetLevelSet();
    const LevelSetLayerType zeroLayer = levelSet->GetLayer( 0 );
    typename LevelSetType::LayerConstIterator layerBegin = zeroLayer.begin();
    typename LevelSetType::LayerConstIterator layerEnd = zeroLayer.end();
    typename SplitLevelSetPartitionerType::DomainType completeDomain( layerBegin, layerEnd );
    this->m_SplitLevelSetComputeIterationThreader->Execute( this, completeDomain );

    ++(this->m_LevelSetContainerIteratorToProcessWhenThreading);
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
    typename LevelSetType::Pointer levelSet = it->GetLevelSet();

    UpdateLevelSetFilterPointer updateLevelSet = UpdateLevelSetFilterType::New();
    updateLevelSet->SetInputLevelSet( levelSet );
    updateLevelSet->SetUpdate( * this->m_UpdateBuffer[it->GetIdentifier()] );
    updateLevelSet->SetEquationContainer( this->m_EquationContainer );
    updateLevelSet->SetTimeStep( this->m_Dt );
    updateLevelSet->SetCurrentLevelSetId( it->GetIdentifier() );
    updateLevelSet->Update();

    levelSet->Graft( updateLevelSet->GetOutputLevelSet() );

    this->m_RMSChangeAccumulator = updateLevelSet->GetRMSChangeAccumulator();

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
::UpdateLevelSets()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();

  while( it != this->m_LevelSetContainer->End() )
    {
    typename LevelSetType::Pointer levelSet = it->GetLevelSet();

    UpdateLevelSetFilterPointer updateLevelSet = UpdateLevelSetFilterType::New();
    updateLevelSet->SetInputLevelSet( levelSet );
    updateLevelSet->SetCurrentLevelSetId( it->GetIdentifier() );
    updateLevelSet->SetEquationContainer( this->m_EquationContainer );
    updateLevelSet->Update();

    levelSet->Graft( updateLevelSet->GetOutputLevelSet() );

    this->m_RMSChangeAccumulator = updateLevelSet->GetRMSChangeAccumulator();

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
::UpdateLevelSets()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();

  while( it != this->m_LevelSetContainer->End() )
    {
    typename LevelSetType::Pointer levelSet = it->GetLevelSet();
    LevelSetIdentifierType       levelSetId = it->GetIdentifier();

    UpdateLevelSetFilterPointer updateLevelSet = UpdateLevelSetFilterType::New();
    updateLevelSet->SetInputLevelSet( levelSet );
    updateLevelSet->SetCurrentLevelSetId( levelSetId );
    updateLevelSet->SetEquationContainer( this->m_EquationContainer );
    updateLevelSet->Update();

    levelSet->Graft( updateLevelSet->GetOutputLevelSet() );

    this->m_RMSChangeAccumulator = updateLevelSet->GetRMSChangeAccumulator();

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
