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

#ifndef itkUpdateMalcolmSparseLevelSet_hxx
#define itkUpdateMalcolmSparseLevelSet_hxx

#include "itkMath.h"
#include "itkUpdateMalcolmSparseLevelSet.h"


namespace itk
{

template< unsigned int VDimension, typename TEquationContainer >
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::UpdateMalcolmSparseLevelSet() :
  m_CurrentLevelSetId( NumericTraits< IdentifierType >::ZeroValue() ),
  m_RMSChangeAccumulator( NumericTraits< LevelSetOutputRealType >::ZeroValue() ),
  m_IsUsingUnPhasedPropagation( true )
{
  this->m_Offset.Fill( 0 );
  this->m_OutputLevelSet = LevelSetType::New();
}

template< unsigned int VDimension, typename TEquationContainer >
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::~UpdateMalcolmSparseLevelSet()
{}


template< unsigned int VDimension, typename TEquationContainer >
void
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::Update()
{
  if( this->m_InputLevelSet.IsNull() )
    {
    itkGenericExceptionMacro( <<"m_InputLevelSet is ITK_NULLPTR" );
    }

  this->m_Offset = this->m_InputLevelSet->GetDomainOffset();

  this->m_OutputLevelSet->SetLayer( LevelSetType::ZeroLayer(), this->m_InputLevelSet->GetLayer( LevelSetType::ZeroLayer() ) );
  this->m_OutputLevelSet->SetLabelMap( this->m_InputLevelSet->GetModifiableLabelMap() );
  this->m_OutputLevelSet->SetDomainOffset( this->m_Offset );

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
    LevelSetLayerType& listZero = this->m_OutputLevelSet->GetLayer( LevelSetType::ZeroLayer() );

    LevelSetLayerType listPos;
    LevelSetLayerType updatePos;

    LevelSetLayerType listNeg;
    LevelSetLayerType updateNeg;

    LevelSetLayerIterator nodeIt = listZero.begin();
    LevelSetLayerIterator nodeEnd = listZero.end();

    LevelSetLayerIterator upIt = this->m_Update.begin();

    while( nodeIt != nodeEnd )
      {
      itkAssertInDebugAndIgnoreInReleaseMacro( nodeIt->first == upIt->first );

      const LevelSetInputType currentIdx = nodeIt->first;
      const LevelSetOutputType update = upIt->second;

      if( update > 0 )
        {
        listPos.insert( NodePairType( currentIdx, LevelSetType::ZeroLayer() ) );
        updatePos.insert( NodePairType( currentIdx, LevelSetType::PlusOneLayer() ) );
        }
      else
        {
        listNeg.insert( NodePairType( currentIdx, LevelSetType::ZeroLayer() ) );
        updateNeg.insert( NodePairType( currentIdx, LevelSetType::MinusOneLayer() ) );
        }
      ++nodeIt;
      ++upIt;
      }

    // contraction
    this->EvolveWithPhasedPropagation( listPos, updatePos, true );
    this->CompactLayersToSinglePixelThickness();

    // dilation
    this->EvolveWithPhasedPropagation( listNeg, updateNeg, false );
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
          typename TEquationContainer >
void
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::FillUpdateContainer()
{
  LevelSetLayerType levelZero = this->m_OutputLevelSet->GetLayer( LevelSetType::ZeroLayer() );

  LevelSetLayerIterator nodeIt = levelZero.begin();
  LevelSetLayerIterator nodeEnd = levelZero.end();

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetInputType inputIndex;
  while( nodeIt != nodeEnd )
    {
    const LevelSetInputType currentIndex = nodeIt->first;
    inputIndex = currentIndex + this->m_Offset;

    const LevelSetOutputRealType update = termContainer->Evaluate( inputIndex );

    LevelSetOutputType value = NumericTraits< LevelSetOutputType >::ZeroValue();

    if( update > NumericTraits< LevelSetOutputRealType >::ZeroValue() )
      {
      value = NumericTraits< LevelSetOutputType >::OneValue();
      }
    if( update < NumericTraits< LevelSetOutputRealType >::ZeroValue() )
      {
      value = - NumericTraits< LevelSetOutputType >::OneValue();
      }

    this->m_Update.insert( NodePairType( currentIndex, value ) );

    ++nodeIt;
    }
}

template< unsigned int VDimension,
          typename TEquationContainer >
void
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::EvolveWithUnPhasedPropagation()
{
  LevelSetOutputType oldValue;
  LevelSetOutputType newValue;
  LevelSetLayerType & levelZero = this->m_OutputLevelSet->GetLayer( LevelSetType::ZeroLayer() );

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

  LevelSetLayerIterator nodeIt = levelZero.begin();
  LevelSetLayerIterator nodeEnd = levelZero.end();

  LevelSetLayerIterator upIt = this->m_Update.begin();

  LevelSetInputType inputIndex;
  while( nodeIt != nodeEnd )
    {
    const LevelSetInputType currentIdx = nodeIt->first;
    inputIndex = currentIdx + this->m_Offset;

    itkAssertInDebugAndIgnoreInReleaseMacro( currentIdx == upIt->first );

    const LevelSetOutputType update = upIt->second;

    if( update != NumericTraits< LevelSetOutputType >::ZeroValue() )
      {
      oldValue = LevelSetType::ZeroLayer();

      if( update > NumericTraits< LevelSetOutputType >::ZeroValue() )
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
      levelZero.erase( tempIt );

      this->m_InternalImage->SetPixel( currentIdx, newValue );
      termContainer->UpdatePixel( inputIndex, oldValue, newValue );

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
    levelZero.insert( NodePairType( nodeIt->first, LevelSetType::ZeroLayer() ) );

    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::ZeroLayer() );
    termContainer->UpdatePixel( nodeIt->first + this->m_Offset, nodeIt->second, LevelSetType::ZeroLayer() );
    ++nodeIt;
    }
}

template< unsigned int VDimension,
          typename TEquationContainer >
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
    LevelSetInputType inputIndex = currentIdx + this->m_Offset;

    if( Math::NotAlmostEquals( update, NumericTraits< LevelSetOutputRealType >::ZeroValue() ) )
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

      termContainer->UpdatePixel( inputIndex, oldValue , newValue );

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

    termContainer->UpdatePixel( nodeIt->first + this->m_Offset, nodeIt->second, LevelSetType::ZeroLayer() );
    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::ZeroLayer() );

    ++nodeIt;
    }
}

template< unsigned int VDimension,
          typename TEquationContainer >
void
UpdateMalcolmSparseLevelSet< VDimension, TEquationContainer >
::CompactLayersToSinglePixelThickness()
{
  LevelSetLayerType & listZero = this->m_OutputLevelSet->GetLayer( LevelSetType::ZeroLayer() );

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

  LevelSetLayerIterator nodeIt   = listZero.begin();
  LevelSetLayerIterator nodeEnd  = listZero.end();

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetInputType inputIndex;
  while( nodeIt != nodeEnd )
    {
    LevelSetInputType currentIdx = nodeIt->first;
    inputIndex = currentIdx + this->m_Offset;

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
      listZero.erase( tempIt );

      this->m_InternalImage->SetPixel( currentIdx, static_cast<typename LabelImageType::PixelType>(newValue) );
      termContainer->UpdatePixel( inputIndex, oldValue , newValue );
      }
    else
      {
      if( positiveUpdate && !negativeUpdate )
        {
        const LevelSetOutputRealType newValue = LevelSetType::PlusOneLayer();
        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        listZero.erase( tempIt );

        this->m_InternalImage->SetPixel( currentIdx, static_cast<typename LabelImageType::PixelType>(newValue) );

        termContainer->UpdatePixel( inputIndex, oldValue , newValue );
        }
      else
        {
        ++nodeIt;
        }
      }
    }
}

}
#endif // itkUpdateMalcolmSparseLevelSet_hxx
