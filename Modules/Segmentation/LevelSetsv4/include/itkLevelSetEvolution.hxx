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
{
  this->m_SingleLevelSetComputeIterationThreader = SingleLevelSetComputeIterationThreaderType::New();
  this->m_SingleLevelSetUpdateLevelSetsThreader = SingleLevelSetUpdateLevelSetsThreaderType::New();
}

template< class TEquationContainer, class TImage >
LevelSetEvolution< TEquationContainer, LevelSetDenseImageBase< TImage > >
::~LevelSetEvolution()
{}

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
          typename LevelSetType::Pointer levelSetUpdate = this->m_UpdateBuffer->GetLevelSet( *lIt - 1);

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
    this->m_SingleLevelSetComputeIterationThreader->Execute( this, inputImage->GetRequestedRegion() );
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
  typename LevelSetContainerType::Iterator levelSetContainerIt = this->m_LevelSetContainer->Begin();
  typename LevelSetContainerType::ConstIterator levelSetUpdateContainerIt = this->m_UpdateBuffer->Begin();

  DomainMapImageFilterPointer domainMapFilter = this->m_LevelSetContainer->GetDomainMapFilter();

  LevelSetOutputRealType p;

  if( !domainMapFilter.IsNull() && domainMapFilter->GetDomainMap().size() > 0 )
    {
    while( levelSetContainerIt != this->m_LevelSetContainer->End() )
      {
      typename LevelSetType::Pointer levelSet = levelSetContainerIt->GetLevelSet();
      typename LevelSetType::Pointer levelSetUpdate = levelSetUpdateContainerIt->GetLevelSet();

      typename LevelSetImageType::Pointer levelSetImage = levelSet->GetImage();
      typename LevelSetImageType::Pointer levelSetUpdateImage = levelSetUpdate->GetImage();

      ImageRegionIterator< LevelSetImageType > levelSetImageIt( levelSetImage, levelSetImage->GetBufferedRegion() );
      ImageRegionConstIterator< LevelSetImageType > levelSetUpdateImageIt( levelSetUpdateImage, levelSetUpdateImage->GetBufferedRegion() );
      levelSetImageIt.GoToBegin();
      levelSetUpdateImageIt.GoToBegin();

      while( !levelSetImageIt.IsAtEnd() )
        {
        p = this->m_Dt * levelSetUpdateImageIt.Get();
        levelSetImageIt.Set( levelSetImageIt.Get() + p );

        this->m_RMSChangeAccumulator += p*p;

        ++levelSetImageIt;
        ++levelSetUpdateImageIt;
        }

      ++levelSetContainerIt;
      ++levelSetUpdateContainerIt;
      }
    }
  else // assume there is one level set that covers the RequestedRegion of the InputImage
    {
    typename LevelSetType::Pointer levelSet = levelSetContainerIt->GetLevelSet();
    typename LevelSetImageType::Pointer levelSetImage = levelSet->GetImage();
    this->m_SingleLevelSetUpdateLevelSetsThreader->Execute( this, levelSetImage->GetRequestedRegion() );
    }

  this->ReinitializeToSignedDistance();
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
::ReinitializeToSignedDistance()
{
  typename LevelSetContainerType::Iterator it = this->m_LevelSetContainer->Begin();

  while( it != this->m_LevelSetContainer->End() )
    {
    typename LevelSetImageType::Pointer image = it->GetLevelSet()->GetImage();

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
    LevelSetPointer levelSet = it->GetLevelSet();

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
    LevelSetPointer         levelSet    = it->GetLevelSet();
    LevelSetIdentifierType  levelSetId  = it->GetIdentifier();

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
