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

#ifndef itkUpdateShiSparseLevelSet_hxx
#define itkUpdateShiSparseLevelSet_hxx

#include "itkUpdateShiSparseLevelSet.h"

namespace itk
{

template< unsigned int VDimension, typename TEquationContainer >
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::UpdateShiSparseLevelSet() :
  m_CurrentLevelSetId( NumericTraits< IdentifierType >::ZeroValue() ),
  m_RMSChangeAccumulator( NumericTraits< LevelSetOutputRealType >::ZeroValue() )
{
  this->m_Offset.Fill( 0 );
  this->m_OutputLevelSet = LevelSetType::New();
}

template< unsigned int VDimension,
          typename TEquationContainer >
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::~UpdateShiSparseLevelSet()
{}


template< unsigned int VDimension, typename TEquationContainer >
void
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::Update()
{
  if( this->m_InputLevelSet.IsNull() )
    {
    itkGenericExceptionMacro( <<"m_InputLevelSet is ITK_NULLPTR" );
    }

  this->m_Offset = this->m_InputLevelSet->GetDomainOffset();

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  this->m_OutputLevelSet->SetLayer( LevelSetType::MinusOneLayer(), this->m_InputLevelSet->GetLayer( LevelSetType::MinusOneLayer() ) );
  this->m_OutputLevelSet->SetLayer( LevelSetType::PlusOneLayer(), this->m_InputLevelSet->GetLayer( LevelSetType::PlusOneLayer() ) );

  this->m_OutputLevelSet->SetLabelMap( this->m_InputLevelSet->GetModifiableLabelMap() );
  this->m_OutputLevelSet->SetDomainOffset( this->m_Offset );

  typedef LabelMapToLabelImageFilter<LevelSetLabelMapType, LabelImageType> LabelMapToLabelImageFilterType;
  typename LabelMapToLabelImageFilterType::Pointer labelMapToLabelImageFilter = LabelMapToLabelImageFilterType::New();
  labelMapToLabelImageFilter->SetInput( this->m_InputLevelSet->GetLabelMap() );
  labelMapToLabelImageFilter->Update();

  this->m_InternalImage = labelMapToLabelImageFilter->GetOutput();
  this->m_InternalImage->DisconnectPipeline();

  // neighborhood iterator
  ZeroFluxNeumannBoundaryCondition< LabelImageType > spNBC;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius, this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &spNBC );

  typename NeighborhoodIteratorType::OffsetType sparseOffset;
  sparseOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    sparseOffset[dim] = -1;
    neighIt.ActivateOffset( sparseOffset );
    sparseOffset[dim] = 1;
    neighIt.ActivateOffset( sparseOffset );
    sparseOffset[dim] = 0;
    }

  // Step 2.1.1
  this->UpdateLayerPlusOne();

 // Step 2.1.2 - for each point x in L_out
  LevelSetLayerType & listIn = this->m_OutputLevelSet->GetLayer( LevelSetType::MinusOneLayer() );

  LevelSetLayerIterator nodeIt = listIn.begin();
  LevelSetLayerIterator nodeEnd = listIn.end();

  LevelSetInputType inputIndex;
  while( nodeIt != nodeEnd )
    {
    const LevelSetInputType currentIndex = nodeIt->first;
    inputIndex = currentIndex + this->m_Offset;

    neighIt.SetLocation( currentIndex );

    bool toBeDeleted = true;

    for( typename NeighborhoodIteratorType::Iterator
         i = neighIt.Begin(); !i.IsAtEnd(); ++i )
      {
      LevelSetOutputType tempValue = i.Get();
      if ( tempValue > NumericTraits< LevelSetOutputType >::ZeroValue() )
        {
        toBeDeleted = false;
        break;
        }
      }
    if( toBeDeleted )
      {
      const LevelSetOutputType oldValue = LevelSetType::MinusOneLayer();
      const LevelSetOutputType newValue = LevelSetType::MinusThreeLayer();

      this->m_InternalImage->SetPixel( currentIndex, newValue );

      LevelSetLayerIterator tempIt = nodeIt;
      ++nodeIt;
      listIn.erase( tempIt );

      termContainer->UpdatePixel( inputIndex, oldValue, newValue );
      }
    else
      {
      ++nodeIt;
      }
    }

//     Step 2.1.3 - for each point x in L_in
  this->UpdateLayerMinusOne();

//     Step 2.1.4
  LevelSetLayerType & listOut = this->m_OutputLevelSet->GetLayer( LevelSetType::PlusOneLayer() );

  nodeIt = listOut.begin();
  nodeEnd = listOut.end();

  while( nodeIt != nodeEnd )
    {
    const LevelSetInputType currentIndex = nodeIt->first;

    neighIt.SetLocation( currentIndex );

    bool toBeDeleted = true;

    for( typename NeighborhoodIteratorType::Iterator i = neighIt.Begin(); !i.IsAtEnd(); ++i )
      {
      LevelSetOutputType tempValue = i.Get();
      if ( tempValue < NumericTraits< LevelSetOutputType >::ZeroValue() )
        {
        toBeDeleted = false;
        break;
        }
      }
    if( toBeDeleted )
      {
      const LevelSetOutputType oldValue = LevelSetType::PlusOneLayer();
      const LevelSetOutputType newValue = LevelSetType::PlusThreeLayer();
      this->m_InternalImage->SetPixel( currentIndex, newValue );

      LevelSetLayerIterator tempIt = nodeIt;
      ++nodeIt;
      listOut.erase( tempIt );

      termContainer->UpdatePixel( inputIndex, oldValue, newValue );
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

template< unsigned int VDimension, typename TEquationContainer >
void
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::UpdateLayerPlusOne()
{
  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType & listOut  = this->m_OutputLevelSet->GetLayer( LevelSetType::PlusOneLayer() );
  LevelSetLayerType & listIn   = this->m_OutputLevelSet->GetLayer( LevelSetType::MinusOneLayer() );

  ZeroFluxNeumannBoundaryCondition< LabelImageType > spNBC;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius, this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &spNBC );

  typename NeighborhoodIteratorType::OffsetType sparseOffset;
  sparseOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    sparseOffset[dim] = -1;
    neighIt.ActivateOffset( sparseOffset );
    sparseOffset[dim] = 1;
    neighIt.ActivateOffset( sparseOffset );
    sparseOffset[dim] = 0;
    }

  LevelSetLayerType insertListIn;
  LevelSetLayerType insertListOut;

  LevelSetLayerIterator nodeIt   = listOut.begin();
  LevelSetLayerIterator nodeEnd  = listOut.end();

  LevelSetInputType inputIndex;

  // for each point in Lz
  while( nodeIt != nodeEnd )
    {
    bool erased = false;
    const LevelSetInputType   currentIndex = nodeIt->first;
    const LevelSetOutputType  currentValue = nodeIt->second;
    inputIndex = currentIndex + this->m_Offset;

    // update the level set
    LevelSetOutputRealType update = termContainer->Evaluate( inputIndex );

    if( update < NumericTraits< LevelSetOutputRealType >::ZeroValue() )
      {
      if( Con( currentIndex, currentValue , update ) )
        {
        // CheckIn
        insertListIn.insert(
              NodePairType( currentIndex, LevelSetType::MinusOneLayer() ) );

        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        listOut.erase( tempIt );
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
    listOut.insert( *nodeIt );

    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::PlusOneLayer() );
    termContainer->UpdatePixel( nodeIt->first + this->m_Offset , LevelSetType::PlusThreeLayer(), LevelSetType::PlusOneLayer() );

    ++nodeIt;
    }

  nodeIt = insertListIn.begin();
  nodeEnd = insertListIn.end();

  while( nodeIt != nodeEnd )
    {
    listIn.insert( *nodeIt );

    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::MinusOneLayer() );
    termContainer->UpdatePixel( nodeIt->first + this->m_Offset, LevelSetType::PlusOneLayer(), LevelSetType::MinusOneLayer() );
    ++nodeIt;
    }
}

template< unsigned int VDimension, typename TEquationContainer >
void
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::UpdateLayerMinusOne()
{
  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType & listOut  = this->m_OutputLevelSet->GetLayer( LevelSetType::PlusOneLayer() );
  LevelSetLayerType & listIn   = this->m_OutputLevelSet->GetLayer( LevelSetType::MinusOneLayer() );

  ZeroFluxNeumannBoundaryCondition< LabelImageType > spNBC;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &spNBC );

  typename NeighborhoodIteratorType::OffsetType sparseOffset;
  sparseOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    sparseOffset[dim] = -1;
    neighIt.ActivateOffset( sparseOffset );
    sparseOffset[dim] = 1;
    neighIt.ActivateOffset( sparseOffset );
    sparseOffset[dim] = 0;
    }

  LevelSetLayerType insertListIn;
  LevelSetLayerType insertListOut;

  LevelSetLayerIterator nodeIt   = listIn.begin();
  LevelSetLayerIterator nodeEnd  = listIn.end();

  // for each point in Lz
  while( nodeIt != nodeEnd )
    {
    bool erased = false;
    const LevelSetInputType   currentIndex = nodeIt->first;
    const LevelSetOutputType  currentValue = nodeIt->second;
    const LevelSetInputType inputIndex = currentIndex + this->m_Offset;

    // update for the current level set
    LevelSetOutputRealType update = termContainer->Evaluate( inputIndex );

    if( update > NumericTraits< LevelSetOutputRealType >::ZeroValue() )
      {
      if( Con( currentIndex, currentValue , update ) )
        {
        // CheckOut
        insertListOut.insert(
              NodePairType( currentIndex, LevelSetType::PlusOneLayer() ) );

        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        listIn.erase( tempIt );

        erased = true;

        neighIt.SetLocation( currentIndex );

        for( typename NeighborhoodIteratorType::Iterator
            i = neighIt.Begin(); !i.IsAtEnd(); ++i )
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
    listIn.insert( *nodeIt );
    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::MinusOneLayer() );
    termContainer->UpdatePixel( nodeIt->first + this->m_Offset, LevelSetType::MinusThreeLayer(), LevelSetType::MinusOneLayer() );
    ++nodeIt;
    }

  nodeIt   = insertListOut.begin();
  nodeEnd  = insertListOut.end();

  // for each point in insertListOut
  while( nodeIt != nodeEnd )
    {
    listOut.insert( *nodeIt );
    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::PlusOneLayer() );
    termContainer->UpdatePixel( nodeIt->first + this->m_Offset, LevelSetType::MinusOneLayer(), LevelSetType::PlusOneLayer() );
    ++nodeIt;
    }
}


template< unsigned int VDimension, typename TEquationContainer >
bool
UpdateShiSparseLevelSet< VDimension, TEquationContainer >
::Con( const LevelSetInputType& idx, const LevelSetOutputType& currentStatus,
       const LevelSetOutputRealType& currentUpdate ) const
{
  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  ZeroFluxNeumannBoundaryCondition< LabelImageType > spNBC;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius, this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &spNBC );

  typename NeighborhoodIteratorType::OffsetType sparseOffset;
  sparseOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    sparseOffset[dim] = -1;
    neighIt.ActivateOffset( sparseOffset );
    sparseOffset[dim] = 1;
    neighIt.ActivateOffset( sparseOffset );
    sparseOffset[dim] = 0;
    }

  neighIt.SetLocation( idx );

  const LevelSetOutputType oppositeStatus = ( currentStatus == LevelSetType::PlusOneLayer() ) ?
        LevelSetType::MinusOneLayer() : LevelSetType::PlusOneLayer();

  for( typename NeighborhoodIteratorType::Iterator i = neighIt.Begin(); !i.IsAtEnd(); ++i )
    {
    LevelSetOutputType tempValue = i.Get();

    if ( tempValue == oppositeStatus )
      {
      LevelSetInputType tempIdx = neighIt.GetIndex( i.GetNeighborhoodOffset() );

      LevelSetOutputRealType neighborUpdate = termContainer->Evaluate( tempIdx + this->m_Offset );

      if ( neighborUpdate * currentUpdate > NumericTraits< LevelSetOutputType >::ZeroValue() )
        {
        return true;
        }
      }
    }
 return false;
}

} // end namespace itk

#endif // itkUpdateShiSparseLevelSet_hxx
