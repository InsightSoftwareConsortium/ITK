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

#ifndef __itkUpdateMalcolmSparseLevelSet_hxx
#define __itkUpdateMalcolmSparseLevelSet_hxx

#include "itkUpdateMalcolmSparseLevelSet.h"


namespace itk
{
template< unsigned int VDimension, class TEquationContainer >
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::UpdateMalcolmSparseLevelSet() :
  m_CurrentLevelSetId( NumericTraits< IdentifierType >::Zero ),
  m_RMSChangeAccumulator( NumericTraits< LevelSetOutputRealType >::Zero ),
  m_IsUsingUnPhasedPropagation( true )
{
  this->m_OutputLevelSet = LevelSetType::New();
}

template< unsigned int VDimension, class TEquationContainer >
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::~UpdateMalcolmSparseLevelSet()
{}

template< unsigned int VDimension, class TEquationContainer >
void
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::Update()
{
  if( this->m_InputLevelSet.IsNull() )
    {
    itkGenericExceptionMacro( <<"m_InputLevelSet is NULL" );
    }

  this->m_OutputLevelSet->SetLayer( LevelSetType::ZeroLayer(), this->m_InputLevelSet->GetLayer( LevelSetType::ZeroLayer() ) );
  this->m_OutputLevelSet->SetLabelMap( this->m_InputLevelSet->GetModifiableLabelMap() );

  typedef LabelMapToLabelImageFilter<LevelSetLabelMapType, LabelImageType> LabelMapToLabelImageFilterType;
  typename LabelMapToLabelImageFilterType::Pointer labelMapToLabelImageFilter = LabelMapToLabelImageFilterType::New();
  labelMapToLabelImageFilter->SetInput( this->m_InputLevelSet->GetLabelMap() );
  labelMapToLabelImageFilter->Update();

  this->m_InternalImage = labelMapToLabelImageFilter->GetOutput();
  this->m_InternalImage->DisconnectPipeline();

  this->FillUpdateContainer();

  if( this->m_IsUsingUnPhasedPropagation )
    {
    EvolveWithUnPhasedPropagation();
    CompactLayersToSinglePixelThickness();
    }
  else
    {
    LevelSetLayerType& list_0 = this->m_OutputLevelSet->GetLayer( LevelSetType::ZeroLayer() );

    LevelSetLayerType list_pos;
    LevelSetLayerType update_pos;

    LevelSetLayerType list_neg;
    LevelSetLayerType update_neg;

    LevelSetLayerIterator nodeIt = list_0.begin();
    LevelSetLayerIterator nodeEnd = list_0.end();

    LevelSetLayerIterator upIt = this->m_Update.begin();

    while( nodeIt != nodeEnd )
      {
      itkAssertInDebugAndIgnoreInReleaseMacro( nodeIt->first == upIt->first );

      const LevelSetInputType currentIdx = nodeIt->first;
      const LevelSetOutputType update = upIt->second;

      if( update > 0 )
        {
        list_pos.insert( NodePairType( currentIdx, LevelSetType::ZeroLayer() ) );
        update_pos.insert( NodePairType( currentIdx, LevelSetType::PlusOneLayer() ) );
        }
      else
        {
        list_neg.insert( NodePairType( currentIdx, LevelSetType::ZeroLayer() ) );
        update_neg.insert( NodePairType( currentIdx, LevelSetType::MinusOneLayer() ) );
        }
      ++nodeIt;
      ++upIt;
      }

    // contraction
    this->EvolveWithPhasedPropagation( list_pos, update_pos, true );
    this->CompactLayersToSinglePixelThickness();

    // dilation
    this->EvolveWithPhasedPropagation( list_neg, update_neg, false );
    this->CompactLayersToSinglePixelThickness();
    }

  typedef LabelImageToLabelMapFilter< LabelImageType, LevelSetLabelMapType> LabelImageToLabelMapFilterType;
  typename LabelImageToLabelMapFilterType::Pointer labelImageToLabelMapFilter = LabelImageToLabelMapFilterType::New();
  labelImageToLabelMapFilter->SetInput( this->m_InternalImage );
  labelImageToLabelMapFilter->SetBackgroundValue( LevelSetType::PlusOneLayer() );
  labelImageToLabelMapFilter->Update();

  LevelSetLabelMapPointer outputLabelMap = this->m_OutputLevelSet->GetModifiableLabelMap( );
  outputLabelMap->Graft( labelImageToLabelMapFilter->GetOutput() );
}

template< unsigned int VDimension,
          class TEquationContainer >
void
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::FillUpdateContainer()
{
  LevelSetLayerType level0 = this->m_OutputLevelSet->GetLayer( LevelSetType::ZeroLayer() );

  LevelSetLayerIterator nodeIt = level0.begin();
  LevelSetLayerIterator nodeEnd = level0.end();

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  while( nodeIt != nodeEnd )
    {
    const LevelSetInputType currentIndex = nodeIt->first;

    const LevelSetOutputRealType update = termContainer->Evaluate( currentIndex );

    LevelSetOutputType value = NumericTraits< LevelSetOutputType >::Zero;

    if( update > NumericTraits< LevelSetOutputRealType >::Zero )
      {
      value = NumericTraits< LevelSetOutputType >::One;
      }
    if( update < NumericTraits< LevelSetOutputRealType >::Zero )
      {
      value = - NumericTraits< LevelSetOutputType >::One;
      }

    this->m_Update.insert( NodePairType( currentIndex, value ) );

    ++nodeIt;
    }
}

template< unsigned int VDimension,
          class TEquationContainer >
void
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::EvolveWithUnPhasedPropagation()
{
  LevelSetOutputType oldValue;
  LevelSetOutputType newValue;
  LevelSetLayerType & level0 = this->m_OutputLevelSet->GetLayer( LevelSetType::ZeroLayer() );

  // neighborhood iterator
  ZeroFluxNeumannBoundaryCondition< LabelImageType > sp_nbc;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &sp_nbc );

  typename NeighborhoodIteratorType::OffsetType sparse_offset;
  sparse_offset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    sparse_offset[dim] = -1;
    neighIt.ActivateOffset( sparse_offset );
    sparse_offset[dim] = 1;
    neighIt.ActivateOffset( sparse_offset );
    sparse_offset[dim] = 0;
    }

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType insertList;

  LevelSetLayerIterator nodeIt = level0.begin();
  LevelSetLayerIterator nodeEnd = level0.end();

  LevelSetLayerIterator upIt = this->m_Update.begin();

  while( nodeIt != nodeEnd )
    {
    const LevelSetInputType currentIdx = nodeIt->first;

    itkAssertInDebugAndIgnoreInReleaseMacro( currentIdx == upIt->first );

    const LevelSetOutputType update = upIt->second;

    if( update != NumericTraits< LevelSetOutputType >::Zero )
      {
      oldValue = LevelSetType::ZeroLayer();

      if( update > NumericTraits< LevelSetOutputType >::Zero )
        {
        newValue = LevelSetType::PlusOneLayer();
        }
      else
        {
        newValue = LevelSetType::MinusOneLayer();
        }

      LevelSetLayerIterator tempIt = nodeIt;
      ++nodeIt;
      ++upIt;
      level0.erase( tempIt );

      this->m_InternalImage->SetPixel( currentIdx, newValue );
      termContainer->UpdatePixel( currentIdx, oldValue, newValue );

      neighIt.SetLocation( currentIdx );

      for( typename NeighborhoodIteratorType::Iterator
           i = neighIt.Begin();
           !i.IsAtEnd(); ++i )
        {
        LevelSetOutputType tempValue = i.Get();
        if( tempValue * newValue == -1 )
          {
          LevelSetInputType tempIndex =
              neighIt.GetIndex( i.GetNeighborhoodOffset() );

          insertList.insert( NodePairType( tempIndex, tempValue ) );
          }
        }
      }
    else
      {
      ++nodeIt;
      ++upIt;
      }
    }

  nodeIt = insertList.begin();
  nodeEnd = insertList.end();
  while( nodeIt != nodeEnd )
    {
    level0.insert( NodePairType( nodeIt->first, LevelSetType::ZeroLayer() ) );

    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::ZeroLayer() );
    termContainer->UpdatePixel( nodeIt->first, nodeIt->second, LevelSetType::ZeroLayer() );
    ++nodeIt;
    }
}

template< unsigned int VDimension,
          class TEquationContainer >
void
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::EvolveWithPhasedPropagation( LevelSetLayerType& ioList,
                        LevelSetLayerType& ioUpdate,
                        const bool& iContraction )
  {
  itkAssertInDebugAndIgnoreInReleaseMacro( ioList.size() == ioUpdate.size() );

  ZeroFluxNeumannBoundaryCondition< LabelImageType > sp_nbc;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &sp_nbc );

  typename NeighborhoodIteratorType::OffsetType sparse_offset;
  sparse_offset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    sparse_offset[dim] = -1;
    neighIt.ActivateOffset( sparse_offset );
    sparse_offset[dim] = 1;
    neighIt.ActivateOffset( sparse_offset );
    sparse_offset[dim] = 0;
    }

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType insertList;

  LevelSetLayerIterator nodeIt = ioList.begin();
  LevelSetLayerIterator nodeEnd = ioList.end();

  LevelSetLayerIterator upIt = ioUpdate.begin();

  LevelSetLayerType outputLayerZero = this->m_OutputLevelSet->GetLayer( LevelSetType::ZeroLayer() );

  while( nodeIt != nodeEnd )
    {
    itkAssertInDebugAndIgnoreInReleaseMacro( nodeIt->first == upIt->first );

    LevelSetOutputType oldValue = LevelSetType::ZeroLayer();
    LevelSetOutputType newValue;

    LevelSetOutputType update = upIt->second;
    LevelSetInputType currentIdx = nodeIt->first;

    if( update != NumericTraits< LevelSetOutputRealType >::Zero )
      {
      // only allow positiveUpdate forces
      if( iContraction )
        {
        newValue = LevelSetType::PlusOneLayer();
        }
      else
        {
        newValue = LevelSetType::MinusOneLayer();
        }

      LevelSetLayerIterator tempIt = nodeIt;
      ++nodeIt;
      ++upIt;
      ioList.erase( tempIt );
      outputLayerZero.erase( currentIdx );

      this->m_InternalImage->SetPixel( currentIdx, newValue );

      termContainer->UpdatePixel( currentIdx, oldValue , newValue );

      neighIt.SetLocation( currentIdx );

      for( typename NeighborhoodIteratorType::Iterator
          i = neighIt.Begin();
          !i.IsAtEnd(); ++i )
        {
        LevelSetOutputType tempValue = i.Get();

        if( tempValue * newValue == -1 )
          {
          LevelSetInputType tempIdx =
            neighIt.GetIndex( i.GetNeighborhoodOffset() );

          insertList.insert( NodePairType( tempIdx, tempValue ) );
          }
        }
      }
    else
      {
      ++nodeIt;
      ++upIt;
      }
    }

  nodeIt = insertList.begin();
  nodeEnd = insertList.end();

  while( nodeIt != nodeEnd )
    {
    outputLayerZero.insert( NodePairType( nodeIt->first, LevelSetType::ZeroLayer() ) );

    termContainer->UpdatePixel( nodeIt->first, nodeIt->second, LevelSetType::ZeroLayer() );
    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::ZeroLayer() );

    ++nodeIt;
    }
  }

template< unsigned int VDimension,
          class TEquationContainer >
void
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::CompactLayersToSinglePixelThickness()
{
  LevelSetLayerType & list_0 = this->m_OutputLevelSet->GetLayer( LevelSetType::ZeroLayer() );

  ZeroFluxNeumannBoundaryCondition< LabelImageType > sp_nbc;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &sp_nbc );

  typename NeighborhoodIteratorType::OffsetType sparse_offset;
  sparse_offset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    sparse_offset[dim] = -1;
    neighIt.ActivateOffset( sparse_offset );
    sparse_offset[dim] = 1;
    neighIt.ActivateOffset( sparse_offset );
    sparse_offset[dim] = 0;
    }

  LevelSetLayerIterator nodeIt   = list_0.begin();
  LevelSetLayerIterator nodeEnd  = list_0.end();

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType currentIdx = nodeIt->first;

    neighIt.SetLocation( currentIdx );

    bool positiveUpdate = false;
    bool negativeUpdate = false;

    LevelSetOutputRealType oldValue = LevelSetType::ZeroLayer();
    for( typename NeighborhoodIteratorType::Iterator
        i = neighIt.Begin();
        !i.IsAtEnd(); ++i )
      {
      LevelSetOutputType tempValue = i.Get();
      if( tempValue == LevelSetType::MinusOneLayer() )
        {
        negativeUpdate = true;
        }
      if ( tempValue == LevelSetType::PlusOneLayer() )
        {
        positiveUpdate = true;
        }
      }

    if( negativeUpdate && !positiveUpdate )
      {
      const LevelSetOutputRealType newValue = LevelSetType::MinusOneLayer();
      LevelSetLayerIterator tempIt = nodeIt;
      ++nodeIt;
      list_0.erase( tempIt );

      this->m_InternalImage->SetPixel( currentIdx, static_cast<typename LabelImageType::PixelType>(newValue) );
      termContainer->UpdatePixel( currentIdx, oldValue , newValue );
      }
    else
      {
      if( positiveUpdate && !negativeUpdate )
        {
        const LevelSetOutputRealType newValue = LevelSetType::PlusOneLayer();
        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        list_0.erase( tempIt );

        this->m_InternalImage->SetPixel( currentIdx, static_cast<typename LabelImageType::PixelType>(newValue) );

        termContainer->UpdatePixel( currentIdx, oldValue , newValue );
        }
      else
        {
        ++nodeIt;
        }
      }
    }
}

}
#endif // __itkUpdateMalcolmSparseLevelSet_hxx
