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

#ifndef __itkUpdateShiSparseLevelSet_hxx
#define __itkUpdateShiSparseLevelSet_hxx

#include "itkUpdateShiSparseLevelSet.h"

namespace itk
{
template< unsigned int VDimension,
          class TEquationContainer >
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::UpdateShiSparseLevelSet() :
  m_CurrentLevelSetId( NumericTraits< IdentifierType >::Zero ),
  m_RMSChangeAccumulator( NumericTraits< LevelSetOutputRealType >::Zero )
{
  this->m_OutputLevelSet = LevelSetType::New();
}

template< unsigned int VDimension,
          class TEquationContainer >
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::~UpdateShiSparseLevelSet()
{}

template< unsigned int VDimension,
          class TEquationContainer >
void
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::Update()
{
  if( this->m_InputLevelSet.IsNull() )
    {
    itkGenericExceptionMacro( <<"m_InputLevelSet is NULL" );
    }

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  this->m_OutputLevelSet->SetLayer( LevelSetType::MinusOneLayer(), this->m_InputLevelSet->GetLayer( LevelSetType::MinusOneLayer() ) );
  this->m_OutputLevelSet->SetLayer( LevelSetType::PlusOneLayer(), this->m_InputLevelSet->GetLayer( LevelSetType::PlusOneLayer() ) );

  this->m_OutputLevelSet->SetLabelMap( this->m_InputLevelSet->GetModifiableLabelMap() );

  typedef LabelMapToLabelImageFilter<LevelSetLabelMapType, LabelImageType> LabelMapToLabelImageFilterType;
  typename LabelMapToLabelImageFilterType::Pointer labelMapToLabelImageFilter = LabelMapToLabelImageFilterType::New();
  labelMapToLabelImageFilter->SetInput( this->m_InputLevelSet->GetLabelMap() );
  labelMapToLabelImageFilter->Update();

  this->m_InternalImage = labelMapToLabelImageFilter->GetOutput();
  this->m_InternalImage->DisconnectPipeline();

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

  // Step 2.1.1
  this->UpdateLayerPlusOne();

 // Step 2.1.2 - for each point x in L_out
  LevelSetLayerType & list_in = this->m_OutputLevelSet->GetLayer( LevelSetType::MinusOneLayer() );

  LevelSetLayerIterator nodeIt = list_in.begin();
  LevelSetLayerIterator nodeEnd = list_in.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType currentIndex = nodeIt->first;

    neighIt.SetLocation( currentIndex );

    bool to_be_deleted = true;

    for( typename NeighborhoodIteratorType::Iterator
         i = neighIt.Begin();
         !i.IsAtEnd(); ++i )
      {
      LevelSetOutputType tempValue = i.Get();
      if ( tempValue > NumericTraits< LevelSetOutputType >::Zero )
        {
        to_be_deleted = false;
        break;
        }
      }
    if( to_be_deleted )
      {
      LevelSetOutputType oldValue = LevelSetType::MinusOneLayer();
      LevelSetOutputType newValue = LevelSetType::MinusThreeLayer();

      this->m_InternalImage->SetPixel( currentIndex, newValue );

      LevelSetLayerIterator tempIt = nodeIt;
      ++nodeIt;
      list_in.erase( tempIt );

      termContainer->UpdatePixel( currentIndex, oldValue, newValue );
      }
    else
      {
      ++nodeIt;
      }
    }

//     Step 2.1.3 - for each point x in L_in
  this->UpdateLayerMinusOne();

//     Step 2.1.4
  LevelSetLayerType & list_out = this->m_OutputLevelSet->GetLayer( LevelSetType::PlusOneLayer() );

  nodeIt = list_out.begin();
  nodeEnd = list_out.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType currentIndex = nodeIt->first;

    neighIt.SetLocation( currentIndex );

    bool to_be_deleted = true;

    for( typename NeighborhoodIteratorType::Iterator
            i = neighIt.Begin();
        !i.IsAtEnd(); ++i )
      {
      LevelSetOutputType tempValue = i.Get();
      if ( tempValue < NumericTraits< LevelSetOutputType >::Zero )
        {
        to_be_deleted = false;
        break;
        }
      }
    if( to_be_deleted )
      {
      LevelSetOutputType oldValue = LevelSetType::PlusOneLayer();
      LevelSetOutputType newValue = LevelSetType::PlusThreeLayer();
      this->m_InternalImage->SetPixel( currentIndex, newValue );

      LevelSetLayerIterator tempIt = nodeIt;
      ++nodeIt;
      list_out.erase( tempIt );

      termContainer->UpdatePixel( currentIndex, oldValue, newValue );
      }
    else
      {
      ++nodeIt;
      }
    }

  typedef LabelImageToLabelMapFilter< LabelImageType, LevelSetLabelMapType> LabelImageToLabelMapFilterType;
  typename LabelImageToLabelMapFilterType::Pointer labelImageToLabelMapFilter = LabelImageToLabelMapFilterType::New();
  labelImageToLabelMapFilter->SetInput( this->m_InternalImage );
  labelImageToLabelMapFilter->SetBackgroundValue( LevelSetType::PlusThreeLayer() );
  labelImageToLabelMapFilter->Update();

  LevelSetLabelMapPointer outputLabelMap = this->m_OutputLevelSet->GetModifiableLabelMap( );
  outputLabelMap->Graft( labelImageToLabelMapFilter->GetOutput() );
}

template< unsigned int VDimension,
          class TEquationContainer >
void
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::UpdateLayerPlusOne()
{
  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType & list_out  = this->m_OutputLevelSet->GetLayer( LevelSetType::PlusOneLayer() );
  LevelSetLayerType & list_in   = this->m_OutputLevelSet->GetLayer( LevelSetType::MinusOneLayer() );

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

  LevelSetLayerType insertListIn;
  LevelSetLayerType insertListOut;

  LevelSetLayerIterator nodeIt   = list_out.begin();
  LevelSetLayerIterator nodeEnd  = list_out.end();

  // for each point in Lz
  while( nodeIt != nodeEnd )
    {
    bool erased = false;
    const LevelSetInputType   currentIndex = nodeIt->first;
    const LevelSetOutputType  currentValue = nodeIt->second;

    // update the level set
    LevelSetOutputRealType update = termContainer->Evaluate( currentIndex );

    if( update < NumericTraits< LevelSetOutputRealType >::Zero )
      {
      if( Con( currentIndex, currentValue , update ) )
        {
        // CheckIn
        insertListIn.insert(
              NodePairType( currentIndex, LevelSetType::MinusOneLayer() ) );

        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        list_out.erase( tempIt );
        erased = true;

        neighIt.SetLocation( currentIndex );

        for( typename NeighborhoodIteratorType::Iterator
            i = neighIt.Begin();
            !i.IsAtEnd(); ++i )
          {
          LevelSetOutputType tempValue = i.Get();

          if ( tempValue == LevelSetType::PlusThreeLayer() )
            {
            LevelSetInputType tempIndex =
                neighIt.GetIndex( i.GetNeighborhoodOffset() );

            insertListOut.insert(
                  NodePairType( tempIndex, LevelSetType::PlusOneLayer() ) );
            }
          }
        }
      }
    if( !erased )
      {
      ++nodeIt;
      }
    }

  nodeIt   = insertListOut.begin();
  nodeEnd  = insertListOut.end();

  // for each point in Lz
  while( nodeIt != nodeEnd )
    {
    list_out.insert( *nodeIt );

    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::PlusOneLayer() );
    termContainer->UpdatePixel( nodeIt->first, LevelSetType::PlusThreeLayer(), LevelSetType::PlusOneLayer() );

    ++nodeIt;
    }

  nodeIt = insertListIn.begin();
  nodeEnd = insertListIn.end();

  while( nodeIt != nodeEnd )
    {
    list_in.insert( *nodeIt );

    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::MinusOneLayer() );
    termContainer->UpdatePixel( nodeIt->first, LevelSetType::PlusOneLayer(), LevelSetType::MinusOneLayer() );
    ++nodeIt;
    }
}

template< unsigned int VDimension,
          class TEquationContainer >
void
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::UpdateLayerMinusOne()
{
  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType & list_out  = this->m_OutputLevelSet->GetLayer( LevelSetType::PlusOneLayer() );
  LevelSetLayerType & list_in   = this->m_OutputLevelSet->GetLayer( LevelSetType::MinusOneLayer() );

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

  LevelSetLayerType insertListIn;
  LevelSetLayerType insertListOut;

  LevelSetLayerIterator nodeIt   = list_in.begin();
  LevelSetLayerIterator nodeEnd  = list_in.end();

  // for each point in Lz
  while( nodeIt != nodeEnd )
    {
    bool erased = false;
    const LevelSetInputType   currentIndex = nodeIt->first;
    const LevelSetOutputType  currentValue = nodeIt->second;

    // update for the current level set
    LevelSetOutputRealType update = termContainer->Evaluate( currentIndex );

    if( update > NumericTraits< LevelSetOutputRealType >::Zero )
      {
      if( Con( currentIndex, currentValue , update ) )
        {
        // CheckOut
        insertListOut.insert(
              NodePairType( currentIndex, LevelSetType::PlusOneLayer() ) );

        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        list_in.erase( tempIt );

        erased = true;

        neighIt.SetLocation( currentIndex );

        for( typename NeighborhoodIteratorType::Iterator
            i = neighIt.Begin();
            !i.IsAtEnd(); ++i )
          {
          LevelSetOutputType tempValue = i.Get();

          if ( tempValue == LevelSetType::MinusThreeLayer() )
            {
            LevelSetInputType tempIndex = neighIt.GetIndex( i.GetNeighborhoodOffset() );

            insertListIn.insert( NodePairType( tempIndex, LevelSetType::MinusOneLayer() ) );
            }
          }
        }
      }
    if( !erased )
      {
      ++nodeIt;
      }
    }

  nodeIt   = insertListIn.begin();
  nodeEnd  = insertListIn.end();

  // for each point in insertListIn
  while( nodeIt != nodeEnd )
    {
    list_in.insert( *nodeIt );
    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::MinusOneLayer() );
    termContainer->UpdatePixel( nodeIt->first, LevelSetType::MinusThreeLayer(), LevelSetType::MinusOneLayer() );
    ++nodeIt;
    }

  nodeIt   = insertListOut.begin();
  nodeEnd  = insertListOut.end();

  // for each point in insertListOut
  while( nodeIt != nodeEnd )
    {
    list_out.insert( *nodeIt );
    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::PlusOneLayer() );
    termContainer->UpdatePixel( nodeIt->first, LevelSetType::MinusOneLayer(), LevelSetType::PlusOneLayer() );
    ++nodeIt;
    }
}

template< unsigned int VDimension,
          class TEquationContainer >
bool
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::Con( const LevelSetInputType& iIdx,
            const LevelSetOutputType& iCurrentStatus,
            const LevelSetOutputRealType& iCurrentUpdate ) const
{
  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

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

  neighIt.SetLocation( iIdx );

  const LevelSetOutputType opposite_status = ( iCurrentStatus == LevelSetType::PlusOneLayer() ) ?
        LevelSetType::MinusOneLayer() : LevelSetType::PlusOneLayer();

  for( typename NeighborhoodIteratorType::Iterator
            i = neighIt.Begin();
        !i.IsAtEnd(); ++i )
    {
    LevelSetOutputType tempValue = i.Get();

    if ( tempValue == opposite_status )
      {
      LevelSetInputType tempIdx = neighIt.GetIndex( i.GetNeighborhoodOffset() );

      LevelSetOutputRealType neighborUpdate = termContainer->Evaluate( tempIdx );

      if ( neighborUpdate * iCurrentUpdate > NumericTraits< LevelSetOutputType >::Zero )
        {
        return true;
        }
      }
    }
 return false;
}
}
#endif // __itkUpdateShiSparseLevelSet_hxx
