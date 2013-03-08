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

#ifndef __itkUpdateWhitakerSparseLevelSet_hxx
#define __itkUpdateWhitakerSparseLevelSet_hxx

#include "itkUpdateWhitakerSparseLevelSet.h"

namespace itk
{
template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::UpdateWhitakerSparseLevelSet() : m_TimeStep( NumericTraits< LevelSetOutputType >::One ),
m_RMSChangeAccumulator( NumericTraits< LevelSetOutputType >::Zero ),
  m_CurrentLevelSetId( NumericTraits< IdentifierType >::Zero ),
m_MinStatus( LevelSetType::MinusThreeLayer() ),
m_MaxStatus( LevelSetType::PlusThreeLayer() )
{
  this->m_TempLevelSet = LevelSetType::New();
  this->m_OutputLevelSet = LevelSetType::New();
}

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::~UpdateWhitakerSparseLevelSet()
{}

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::SetUpdate( const LevelSetLayerType& iUpdate )
{
  this->m_Update = iUpdate;
}

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::Update()
{
  if( this->m_InputLevelSet.IsNull() )
    {
    itkGenericExceptionMacro( <<"m_InputLevelSet is NULL" );
    }
  if( this->m_Update.empty() )
    {
    itkGenericExceptionMacro( <<"m_Update is empty" );
    }

  // copy input to output. Will not use input again
  // store modified output in this->m_TempLevelSet
  this->m_OutputLevelSet->SetLayer( LevelSetType::MinusTwoLayer(), this->m_InputLevelSet->GetLayer( LevelSetType::MinusTwoLayer() ) );
  this->m_OutputLevelSet->SetLayer( LevelSetType::MinusOneLayer(), this->m_InputLevelSet->GetLayer( LevelSetType::MinusOneLayer() ) );
  this->m_OutputLevelSet->SetLayer( LevelSetType::ZeroLayer(), this->m_InputLevelSet->GetLayer( LevelSetType::ZeroLayer() ) );
  this->m_OutputLevelSet->SetLayer( LevelSetType::PlusOneLayer(), this->m_InputLevelSet->GetLayer( LevelSetType::PlusOneLayer() ) );
  this->m_OutputLevelSet->SetLayer( LevelSetType::PlusTwoLayer(), this->m_InputLevelSet->GetLayer( LevelSetType::PlusTwoLayer() ) );

  this->m_OutputLevelSet->SetLabelMap( this->m_InputLevelSet->GetModifiableLabelMap() );

  typename LabelMapToLabelImageFilterType::Pointer labelMapToLabelImageFilter = LabelMapToLabelImageFilterType::New();
  labelMapToLabelImageFilter->SetInput( this->m_InputLevelSet->GetModifiableLabelMap() );
  labelMapToLabelImageFilter->Update();

  this->m_InternalImage = labelMapToLabelImageFilter->GetOutput();
  this->m_InternalImage->DisconnectPipeline();

  this->m_TempPhi.clear();

  // TODO: ARNAUD: Why is 2 not included here?
  // Arnaud: Being iterated upon later, so no need to do it here.
  // Here, we are adding all pairs of indices and levelset values to a map
  for( LevelSetLayerIdType status = LevelSetType::MinusOneLayer();
       status < LevelSetType::PlusTwoLayer(); status++ )
    {
    LevelSetLayerType layer = this->m_InputLevelSet->GetLayer( status );

    LevelSetLayerConstIterator it = layer.begin();
    while( it != layer.end() )
      {
      this->m_TempPhi[ it->first ] = it->second;
      ++it;
      }
    }

  ZeroFluxNeumannBoundaryCondition< LabelImageType > sp_nbc;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &sp_nbc );

  typename NeighborhoodIteratorType::OffsetType neighOffset;
  neighOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    neighOffset[dim] = -1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 0;
    }

  LevelSetLayerType layerMinus2 = this->m_InputLevelSet->GetLayer( LevelSetType::MinusTwoLayer() );

  LevelSetLayerConstIterator it = layerMinus2.begin();
  while( it != layerMinus2.end() )
    {
    LevelSetInputType currentIndex = it->first;
    this->m_TempPhi[ currentIndex ] = LevelSetType::MinusTwoLayer();
    neighIt.SetLocation( currentIndex );

    for( typename NeighborhoodIteratorType::Iterator nIt = neighIt.Begin();
         !nIt.IsAtEnd();
         ++nIt )
      {
      if( nIt.Get() == LevelSetType::MinusThreeLayer() )
        {
        LevelSetInputType tempIndex =
            neighIt.GetIndex( nIt.GetNeighborhoodOffset() );

        this->m_TempPhi[ tempIndex ] = LevelSetType::MinusThreeLayer();
        }
      }

    ++it;
    }

  LevelSetLayerType layerPlus2 = this->m_InputLevelSet->GetLayer( LevelSetType::PlusTwoLayer() );

  it = layerPlus2.begin();
  while( it != layerPlus2.end() )
    {
    LevelSetInputType currentIndex = it->first;
    this->m_TempPhi[ currentIndex ] = LevelSetType::PlusTwoLayer();
    neighIt.SetLocation( currentIndex );

    for( typename NeighborhoodIteratorType::Iterator nIt = neighIt.Begin();
         !nIt.IsAtEnd();
         ++nIt )
      {
      if( nIt.Get() == LevelSetType::PlusThreeLayer() )
        {
        LevelSetInputType tempIndex =
            neighIt.GetIndex( nIt.GetNeighborhoodOffset() );

        this->m_TempPhi[ tempIndex ] = LevelSetType::PlusThreeLayer();
        }
      }

    ++it;
    }

  this->UpdateLayerZero();

  this->UpdateLayerMinus1();

  this->UpdateLayerPlus1();

  this->UpdateLayerMinus2();

  this->UpdateLayerPlus2();

  this->MovePointIntoZeroLevelSet();

  this->MovePointFromMinus1();

  this->MovePointFromPlus1();

  this->MovePointFromMinus2();

  this->MovePointFromPlus2();

  typename LabelImageToLabelMapFilterType::Pointer labelImageToLabelMapFilter = LabelImageToLabelMapFilterType::New();
  labelImageToLabelMapFilter->SetInput( this->m_InternalImage );
  labelImageToLabelMapFilter->SetBackgroundValue( LevelSetType::PlusThreeLayer() );
  labelImageToLabelMapFilter->Update();

  this->m_OutputLevelSet->GetModifiableLabelMap( )->Graft( labelImageToLabelMapFilter->GetOutput() );
  this->m_TempPhi.clear();
}

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::UpdateLayerZero()
{
  TermContainerPointer termContainer =  this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType& outputLayer0 = this->m_OutputLevelSet->GetLayer( LevelSetType::ZeroLayer() );

  LevelSetLayerType& layerMinus1 =  this->m_TempLevelSet->GetLayer( LevelSetType::MinusOneLayer() );
  LevelSetLayerType& layerPlus1 =  this->m_TempLevelSet->GetLayer( LevelSetType::PlusOneLayer() );

  itkAssertInDebugAndIgnoreInReleaseMacro( this->m_Update.size() == outputLayer0.size() );

  LevelSetLayerIterator nodeIt   = outputLayer0.begin();
  LevelSetLayerIterator nodeEnd  = outputLayer0.end();

  LevelSetLayerIterator upIt     = this->m_Update.begin();

  ZeroFluxNeumannBoundaryCondition< LabelImageType > sp_nbc;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &sp_nbc );

  typename NeighborhoodIteratorType::OffsetType neighOffset;
  neighOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    neighOffset[dim] = -1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 0;
    }

  while( nodeIt != nodeEnd )
    {
    itkAssertInDebugAndIgnoreInReleaseMacro( nodeIt->first == upIt->first );

    LevelSetInputType   currentIndex = nodeIt->first;
    LevelSetOutputType  currentValue = nodeIt->second;
    LevelSetOutputType  tempUpdate = this->m_TimeStep * static_cast< LevelSetOutputType >( upIt->second );

    if( tempUpdate > 0.5 )
      {
      // what about 0.5 - itk::NumericTraits< LevelSetOutputType >::epsilon(); ?
      tempUpdate = 0.499;
      }
    else if( tempUpdate < - 0.5 )
      {
      // what about - ( 0.5 - itk::NumericTraits< LevelSetOutputType >::epsilon(); ) ?
      tempUpdate = - 0.499;
      }

    LevelSetOutputType tempValue = currentValue + tempUpdate;
    this->m_RMSChangeAccumulator += tempUpdate*tempUpdate;

    if( tempValue > 0.5 )
      {
      // is there any point moving in the opposite direction?
      bool samedirection = true;

      neighIt.SetLocation( currentIndex );

      for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
           !it.IsAtEnd();
           ++it )
        {
        if( it.Get() == LevelSetType::ZeroLayer() )
          {
          LevelSetInputType tempIndex = neighIt.GetIndex( it.GetNeighborhoodOffset() );

          LevelSetLayerIterator tit = this->m_TempPhi.find( tempIndex );

          if( tit != this->m_TempPhi.end() )
            {
            if( tit->second < -0.5 )
              {
              samedirection = false;
              }
            }
          }
        }

      if( samedirection )
        {
        LevelSetLayerIterator tit = this->m_TempPhi.find( currentIndex );

        if( tit != this->m_TempPhi.end() )
          {
          termContainer->UpdatePixel( currentIndex, tit->second, tempValue );
          tit->second = tempValue;
          }
        else
          {
          // Kishore: Never comes here?
          this->m_TempPhi.insert( NodePairType( currentIndex, tempValue ) );
          }

        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        ++upIt;
        // remove p from Lz
        outputLayer0.erase( tempIt );

        // add p to Sp1
        layerPlus1.insert( NodePairType( currentIndex, tempValue ) );
        }
      else // samedirection == false
        {
        ++nodeIt;
        ++upIt;
        }
      } // end of if( tempValue > 0.5 )
    else if( tempValue < -0.5 )
      {
        bool samedirection = true;

        neighIt.SetLocation( currentIndex );

        for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
             !it.IsAtEnd();
             ++it )
          {
          if( it.Get() == LevelSetType::ZeroLayer() )
            {
            LevelSetInputType tempIndex =
                neighIt.GetIndex( it.GetNeighborhoodOffset() );

            LevelSetLayerIterator tit = this->m_TempPhi.find( tempIndex );
            if( tit != this->m_TempPhi.end() )
              {
              if( tit->second > 0.5 )
                {
                samedirection = false;
                }
              }
            }
          }

        if( samedirection )
          {
          LevelSetLayerIterator tit = this->m_TempPhi.find( currentIndex );

          if( tit != this->m_TempPhi.end() )
            { // change values
            termContainer->UpdatePixel( currentIndex, tit->second, tempValue );
            tit->second = tempValue;
            }
          else
            {// Kishore: Can this happen?
            this->m_TempPhi.insert( NodePairType( currentIndex, tempValue ) );
            }

          LevelSetLayerIterator tempIt = nodeIt;
          ++nodeIt;
          ++upIt;
          outputLayer0.erase( tempIt );

          layerMinus1.insert( NodePairType( currentIndex, tempValue ) );
          }
        else // samedirection == false
          {
          ++nodeIt;
          ++upIt;
          }
      }
    else // -0.5 <= temp <= 0.5
      {
      LevelSetLayerIterator it = this->m_TempPhi.find( currentIndex );

      if( it != this->m_TempPhi.end() )
        { // change values
        termContainer->UpdatePixel( currentIndex, it->second, tempValue );
        it->second = tempValue;
        }
      nodeIt->second = tempValue;
      ++nodeIt;
      ++upIt;
      }
    } // while( nodeIt != nodeEnd )
}

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::UpdateLayerMinus1()
  {
  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  ZeroFluxNeumannBoundaryCondition< LabelImageType > sp_nbc;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &sp_nbc );

  typename NeighborhoodIteratorType::OffsetType neighOffset;
  neighOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    neighOffset[dim] = -1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 0;
    }

  LevelSetLayerType& outputLayerMinus1 = this->m_OutputLevelSet->GetLayer( LevelSetType::MinusOneLayer() );

  LevelSetLayerType& layerMinusTwo = this->m_TempLevelSet->GetLayer( LevelSetType::MinusTwoLayer() );
  LevelSetLayerType& layerZero = this->m_TempLevelSet->GetLayer( LevelSetType::ZeroLayer() );

  LevelSetLayerIterator nodeIt   = outputLayerMinus1.begin();
  LevelSetLayerIterator nodeEnd  = outputLayerMinus1.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType currentIndex = nodeIt->first;

    neighIt.SetLocation( currentIndex );

    bool IsThereAPointWithLabelEqualTo0 = false;

    LevelSetOutputType M = NumericTraits< LevelSetOutputType >::NonpositiveMin();

    // compute M and check if point with label 0 exists in the neighborhood
    for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
         !it.IsAtEnd();
         ++it )
      {
      LevelSetInputType tempIndex = neighIt.GetIndex( it.GetNeighborhoodOffset() );

      LevelSetLayerIdType label = it.Get();

      if( label >= LevelSetType::ZeroLayer() )
        {
        if( label == LevelSetType::ZeroLayer() )
          {
          IsThereAPointWithLabelEqualTo0 = true;
          }

        LevelSetLayerIterator phiIt = this->m_TempPhi.find( tempIndex );
        itkAssertInDebugAndIgnoreInReleaseMacro( phiIt != this->m_TempPhi.end() );

        M = vnl_math_max( M, phiIt->second );
        }
      } // end for

    if( IsThereAPointWithLabelEqualTo0 )
      {
      LevelSetLayerIterator phiIt = this->m_TempPhi.find( currentIndex );

      M = M - 1.;

      if( phiIt != this->m_TempPhi.end() )
        {// change value
        termContainer->UpdatePixel( currentIndex, phiIt->second, M );
        phiIt->second = M;
        nodeIt->second = M;
        }
      else
        { // Kishore: Can this happen?
        this->m_TempPhi.insert( NodePairType( currentIndex, M ) );
        }

      if( M >= -0.5 )
        { // change layers only
        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        outputLayerMinus1.erase( tempIt );

        layerZero.insert( NodePairType( currentIndex, M) );
        }
      else if( M < -1.5 )
        { // change layers only
        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        outputLayerMinus1.erase( tempIt );

        layerMinusTwo.insert( NodePairType( currentIndex, M) );
        }
      else
        {
        ++nodeIt;
        }
      }
    else // !IsThereAPointWithLabelEqualTo0
      { // change layers only
      LevelSetLayerIterator tempIt = nodeIt;
      LevelSetOutputType t = tempIt->second;
      ++nodeIt;
      outputLayerMinus1.erase( tempIt );

      layerMinusTwo.insert( NodePairType( currentIndex, t ) );
      }
    }
  }

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::UpdateLayerPlus1()
  {
  ZeroFluxNeumannBoundaryCondition< LabelImageType > sp_nbc;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &sp_nbc );

  typename NeighborhoodIteratorType::OffsetType neighOffset;
  neighOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    neighOffset[dim] = -1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 0;
    }

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType& layerPlus2 = this->m_TempLevelSet->GetLayer( LevelSetType::PlusTwoLayer() );
  LevelSetLayerType& layerZero = this->m_TempLevelSet->GetLayer( LevelSetType::ZeroLayer() );

  LevelSetLayerType& outputLayerPlus1 = this->m_OutputLevelSet->GetLayer( LevelSetType::PlusOneLayer() );

  LevelSetLayerIterator nodeIt   = outputLayerPlus1.begin();
  LevelSetLayerIterator nodeEnd  = outputLayerPlus1.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType currentIndex = nodeIt->first;

    neighIt.SetLocation( currentIndex );

    bool IsThereAPointWithLabelEqualTo0 = false;

    LevelSetOutputType M = NumericTraits< LevelSetOutputType >::max();

    for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
         !it.IsAtEnd();
         ++it )
      {
      LevelSetLayerIdType label = it.Get();

      if( label <= LevelSetType::ZeroLayer() )
        {
        if( label == LevelSetType::ZeroLayer() )
          {
          IsThereAPointWithLabelEqualTo0 = true;
          }
        LevelSetInputType tempIndex =
            neighIt.GetIndex( it.GetNeighborhoodOffset() );

        LevelSetLayerIterator phiIt = this->m_TempPhi.find( tempIndex );
        if( phiIt != this->m_TempPhi.end() )
          {
          M = vnl_math_min( M, phiIt->second );
          }
        else
          {
          itkDebugMacro( << tempIndex << "is not in this->m_TempPhi" <<std::endl );
          }
        }
      } // end for

    if( IsThereAPointWithLabelEqualTo0 )
      {
      LevelSetLayerIterator phiIt = this->m_TempPhi.find( currentIndex );

      M = M + 1.;

      if( phiIt != this->m_TempPhi.end() )
        {// change in value
        termContainer->UpdatePixel( currentIndex, phiIt->second, M );
        phiIt->second = M;
        nodeIt->second = M;
        }
      else
        {// Kishore: can this happen?
        this->m_TempPhi.insert( NodePairType( currentIndex, M ) );
        }

      if( M <= 0.5 )
        {//change layers only
        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        outputLayerPlus1.erase( tempIt );
        layerZero.insert( NodePairType( currentIndex, M) );
        }
      else if( M > 1.5 )
        {//change layers only
        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        outputLayerPlus1.erase( tempIt );
        layerPlus2.insert( NodePairType( currentIndex, M) );
        }
      else
        {
        ++nodeIt;
        }
      }
    else
      { // change layers only
      LevelSetLayerIterator tempIt = nodeIt;
      LevelSetOutputType t = tempIt->second;
      ++nodeIt;
      outputLayerPlus1.erase( tempIt );
      layerPlus2.insert( NodePairType( currentIndex, t ) );
      }
    }
  }

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::UpdateLayerMinus2()
{
  ZeroFluxNeumannBoundaryCondition< LabelImageType > sp_nbc;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &sp_nbc );

  typename NeighborhoodIteratorType::OffsetType neighOffset;
  neighOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    neighOffset[dim] = -1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 0;
    }

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType& outputLayerMinus2 = this->m_OutputLevelSet->GetLayer( LevelSetType::MinusTwoLayer() );
  LevelSetLayerType& LayerMinus1 = this->m_TempLevelSet->GetLayer( LevelSetType::MinusOneLayer() );

  LevelSetLayerIterator nodeIt   = outputLayerMinus2.begin();
  LevelSetLayerIterator nodeEnd  = outputLayerMinus2.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType currentIndex = nodeIt->first;

    neighIt.SetLocation( currentIndex );

    bool IsThereAPointWithLabelEqualToMinus1 = false;

    LevelSetOutputType M = NumericTraits< LevelSetOutputType >::NonpositiveMin();

    for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
         !it.IsAtEnd();
         ++it )
      {
      LevelSetLayerIdType label = it.Get();

      if( label >= LevelSetType::MinusOneLayer() )
        {
        if( label == LevelSetType::MinusOneLayer() )
          {
          IsThereAPointWithLabelEqualToMinus1 = true;
          }
        LevelSetInputType tempIndex =
            neighIt.GetIndex( it.GetNeighborhoodOffset() );

        LevelSetLayerIterator phiIt = this->m_TempPhi.find( tempIndex );
        itkAssertInDebugAndIgnoreInReleaseMacro( phiIt != this->m_TempPhi.end() );

        M = vnl_math_max( M, phiIt->second );
        }
      } // end for

    if( IsThereAPointWithLabelEqualToMinus1 )
      {
      LevelSetLayerIterator phiIt = this->m_TempPhi.find( currentIndex );

      M = M - 1.;

      if( phiIt != this->m_TempPhi.end() )
        {//change values
        termContainer->UpdatePixel( currentIndex, phiIt->second, M );
        phiIt->second = M;
        nodeIt->second = M;
        }
      else
        {//Kishore: can this happen?
        this->m_TempPhi.insert(
              NodePairType( currentIndex, M ) );
        }

      if( M >= -1.5 )
        {//change layers only
        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        outputLayerMinus2.erase( tempIt );
        LayerMinus1.insert( NodePairType( currentIndex, M) );
        }
      else if( M < -2.5 )
        {//change layers only
        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        outputLayerMinus2.erase( tempIt );

        this->m_InternalImage->SetPixel( currentIndex, LevelSetType::MinusThreeLayer() );

        termContainer->UpdatePixel( currentIndex, M, LevelSetType::MinusThreeLayer() );

        this->m_TempPhi.erase( currentIndex );
        }
      else
        {
        ++nodeIt;
        }
      }
    else
      {// change value
      LevelSetLayerIterator tempIt = nodeIt;
      ++nodeIt;
      this->m_InternalImage->SetPixel( currentIndex, LevelSetType::MinusThreeLayer() );
      termContainer->UpdatePixel( currentIndex, tempIt->second, LevelSetType::MinusThreeLayer() );
      outputLayerMinus2.erase( tempIt );
      this->m_TempPhi.erase( currentIndex );
      }
    }
  }

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::UpdateLayerPlus2()
  {
  ZeroFluxNeumannBoundaryCondition< LabelImageType > sp_nbc;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &sp_nbc );

  typename NeighborhoodIteratorType::OffsetType neighOffset;
  neighOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    neighOffset[dim] = -1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 0;
    }

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType& outputLayerPlus2 = this->m_OutputLevelSet->GetLayer( LevelSetType::PlusTwoLayer() );
  LevelSetLayerType& layerPlusOne = this->m_TempLevelSet->GetLayer( LevelSetType::PlusOneLayer() );

  LevelSetLayerIterator nodeIt   = outputLayerPlus2.begin();
  LevelSetLayerIterator nodeEnd  = outputLayerPlus2.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType currentIndex = nodeIt->first;

    neighIt.SetLocation( currentIndex );

    bool IsThereAPointWithLabelEqualToPlus1 = false;

    LevelSetOutputType M = NumericTraits< LevelSetOutputType >::max();

    for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
         !it.IsAtEnd();
         ++it )
      {
      LevelSetLayerIdType label = it.Get();

      if( label <= LevelSetType::PlusOneLayer() )
        {
        if( label == LevelSetType::PlusOneLayer() )
          {
          IsThereAPointWithLabelEqualToPlus1 = true;
          }
        LevelSetInputType tempIndex = neighIt.GetIndex( it.GetNeighborhoodOffset() );

        LevelSetLayerIterator phiIt = this->m_TempPhi.find( tempIndex );
        if( phiIt != this->m_TempPhi.end() )
          {
          M = vnl_math_min( M, phiIt->second );
          }
        else
          {
          itkDebugMacro( << tempIndex << " is not in this->m_TempPhi" << std::endl );
          }
        }
      } // end for

    if( IsThereAPointWithLabelEqualToPlus1 )
      {
      LevelSetLayerIterator phiIt = this->m_TempPhi.find( currentIndex );

      M = M + 1.;

      if( phiIt != this->m_TempPhi.end() )
        {//change values
        termContainer->UpdatePixel( currentIndex, phiIt->second, M );
        phiIt->second = M;
        nodeIt->second = M;
        }
      else
        {//Kishore: can this happen?
        this->m_TempPhi.insert( NodePairType( currentIndex, M ) );
        }

      if( M <= 1.5 )
        {//change layers
        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        outputLayerPlus2.erase( tempIt );
        layerPlusOne.insert( NodePairType( currentIndex, M) );
        }
      else if( M > 2.5 )
        {//change layers
        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        outputLayerPlus2.erase( tempIt );
        this->m_InternalImage->SetPixel( currentIndex, LevelSetType::PlusThreeLayer() );

        termContainer->UpdatePixel( currentIndex, M, LevelSetType::PlusThreeLayer() );

        this->m_TempPhi.erase( currentIndex );
        }
      else
        {
        ++nodeIt;
        }
      }
    else
      {//change values
      LevelSetLayerIterator tempIt = nodeIt;
      ++nodeIt;
      this->m_InternalImage->SetPixel( currentIndex, LevelSetType::PlusThreeLayer() );
      termContainer->UpdatePixel( currentIndex, tempIt->second, LevelSetType::PlusThreeLayer() );
      outputLayerPlus2.erase( tempIt );
      this->m_TempPhi.erase( currentIndex );
      }
    }
  }

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::MovePointIntoZeroLevelSet()
  {
  LevelSetLayerType& layer0 = this->m_TempLevelSet->GetLayer( LevelSetType::ZeroLayer() );
  LevelSetLayerType& outputLayer0 = this->m_OutputLevelSet->GetLayer( LevelSetType::ZeroLayer() );

  LevelSetLayerIterator nodeIt = layer0.begin();
  LevelSetLayerIterator nodeEnd = layer0.end();

  while( nodeIt != nodeEnd )
    {
    outputLayer0.insert( NodePairType( nodeIt->first, nodeIt->second ) );
    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::ZeroLayer() );

    LevelSetLayerIterator tempIt = nodeIt;
    ++nodeIt;
    layer0.erase( tempIt );
    }
  }

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::MovePointFromMinus1()
  {
  ZeroFluxNeumannBoundaryCondition< LabelImageType > sp_nbc;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &sp_nbc );

  typename NeighborhoodIteratorType::OffsetType neighOffset;
  neighOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    neighOffset[dim] = -1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 0;
    }

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType& layerMinus1 = this->m_TempLevelSet->GetLayer( LevelSetType::MinusOneLayer() );
  LevelSetLayerType& layerMinus2 = this->m_TempLevelSet->GetLayer( LevelSetType::MinusTwoLayer() );

  LevelSetLayerType& outputLayerMinus1 = this->m_OutputLevelSet->GetLayer( LevelSetType::MinusOneLayer() );

  LevelSetLayerIterator nodeIt = layerMinus1.begin();
  LevelSetLayerIterator nodeEnd = layerMinus1.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType   currentIndex = nodeIt->first;
    LevelSetOutputType  currentValue = nodeIt->second;

    outputLayerMinus1.insert( NodePairType( currentIndex, currentValue ) );

    this->m_InternalImage->SetPixel( currentIndex, LevelSetType::MinusOneLayer() );

    LevelSetLayerIterator tempIt = nodeIt;
    ++nodeIt;
    layerMinus1.erase( tempIt );

    neighIt.SetLocation( currentIndex );

    for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
         !it.IsAtEnd();
         ++it )
      {
      LevelSetInputType tempIndex = neighIt.GetIndex( it.GetNeighborhoodOffset() );

      LevelSetLayerIterator phiIt = this->m_TempPhi.find( tempIndex );
      if( phiIt != this->m_TempPhi.end() )
        {
        if( phiIt->second == -3. )
          {//change values
          phiIt->second = currentValue - 1;
          layerMinus2.insert( NodePairType( tempIndex, currentValue - 1 ) );

          termContainer->UpdatePixel( tempIndex, LevelSetType::MinusThreeLayer(), phiIt->second );
          }
        }
      }
    }
}

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::MovePointFromPlus1()
{
  ZeroFluxNeumannBoundaryCondition< LabelImageType > sp_nbc;

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &sp_nbc );

  typename NeighborhoodIteratorType::OffsetType neighOffset;
  neighOffset.Fill( 0 );

  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    neighOffset[dim] = -1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 1;
    neighIt.ActivateOffset( neighOffset );
    neighOffset[dim] = 0;
    }

  TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( this->m_CurrentLevelSetId );

  LevelSetLayerType& layerPlus1 = this->m_TempLevelSet->GetLayer( LevelSetType::PlusOneLayer() );
  LevelSetLayerType& layerPlus2 = this->m_TempLevelSet->GetLayer( LevelSetType::PlusTwoLayer() );

  LevelSetLayerType& outputLayerPlus1 = this->m_OutputLevelSet->GetLayer( LevelSetType::PlusOneLayer() );

  LevelSetLayerIterator nodeIt = layerPlus1.begin();
  LevelSetLayerIterator nodeEnd = layerPlus1.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType   currentIndex = nodeIt->first;
    LevelSetOutputType  currentValue = nodeIt->second;

    outputLayerPlus1.insert( NodePairType( currentIndex, currentValue ) );
    this->m_InternalImage->SetPixel( currentIndex, LevelSetType::PlusOneLayer() );

    LevelSetLayerIterator tempIt = nodeIt;
    ++nodeIt;
    layerPlus1.erase( tempIt );

    neighIt.SetLocation( currentIndex );

    for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
         !it.IsAtEnd();
         ++it )
      {
      LevelSetInputType tempIndex = neighIt.GetIndex( it.GetNeighborhoodOffset() );

      LevelSetLayerIterator phiIt = this->m_TempPhi.find( tempIndex );
      if( phiIt != this->m_TempPhi.end() )
        {
        if( phiIt->second == 3. )
          {// change values here
          phiIt->second = currentValue + 1;

          layerPlus2.insert( NodePairType( tempIndex, currentValue + 1 ) );

          termContainer->UpdatePixel( tempIndex, 3, phiIt->second );
          }
        }
      }
    }
}

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::MovePointFromMinus2()
{
  LevelSetLayerType& layerMinus2 = this->m_TempLevelSet->GetLayer( LevelSetType::MinusTwoLayer() );
  LevelSetLayerType& outputLayerMinus2 = this->m_OutputLevelSet->GetLayer( LevelSetType::MinusTwoLayer() );

  LevelSetLayerIterator nodeIt = layerMinus2.begin();
  LevelSetLayerIterator nodeEnd = layerMinus2.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType   currentIndex = nodeIt->first;

    outputLayerMinus2.insert( NodePairType( currentIndex, nodeIt->second ) );

    this->m_InternalImage->SetPixel( currentIndex, LevelSetType::MinusTwoLayer() );

    LevelSetLayerIterator tempIt = nodeIt;
    ++nodeIt;
    layerMinus2.erase( tempIt );
    }
}

template< unsigned int VDimension,
          typename TLevelSetValueType,
          class TEquationContainer >
void UpdateWhitakerSparseLevelSet< VDimension, TLevelSetValueType, TEquationContainer >
::MovePointFromPlus2()
{
  LevelSetLayerType& layerPlus2 = this->m_TempLevelSet->GetLayer( LevelSetType::PlusTwoLayer() );
  LevelSetLayerType& outputLayerPlus2 = this->m_OutputLevelSet->GetLayer( LevelSetType::PlusTwoLayer() );

  LevelSetLayerIterator nodeIt = layerPlus2.begin();
  LevelSetLayerIterator nodeEnd = layerPlus2.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType   currentIndex = nodeIt->first;

    outputLayerPlus2.insert( NodePairType( currentIndex, nodeIt->second ) );
    this->m_InternalImage->SetPixel( currentIndex, LevelSetType::PlusTwoLayer() );

    LevelSetLayerIterator tempIt = nodeIt;
    ++nodeIt;
    layerPlus2.erase( tempIt );
    }
}
}
#endif // __itkUpdateWhitakerSparseLevelSet_hxx
