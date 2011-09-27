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

#ifndef __itkBinaryImageToWhitakerSparseLevelSetAdaptor_hxx
#define __itkBinaryImageToWhitakerSparseLevelSetAdaptor_hxx

#include "itkBinaryImageToWhitakerSparseLevelSetAdaptor.h"


namespace itk
{
template< class TInputImage, typename TLevelSetValueType >
BinaryImageToWhitakerSparseLevelSetAdaptor< TInputImage, TLevelSetValueType >
::BinaryImageToWhitakerSparseLevelSetAdaptor() : m_InputImage( NULL )
{
  this->m_SparseLevelSet = LevelSetType::New();
}

template< class TInputImage, typename TLevelSetValueType >
BinaryImageToWhitakerSparseLevelSetAdaptor< TInputImage, TLevelSetValueType >
::~BinaryImageToWhitakerSparseLevelSetAdaptor()
{
}

template< class TInputImage, typename TLevelSetValueType >
void BinaryImageToWhitakerSparseLevelSetAdaptor< TInputImage, TLevelSetValueType >
::Initialize()
{
  if( this->m_InputImage.IsNull() )
    {
    itkGenericExceptionMacro( << "m_InputImage is NULL" );
    }

  this->m_LabelMap = LevelSetLabelMapType::New();
  this->m_LabelMap->SetBackgroundValue( LevelSetType::PlusThreeLayer() );
  this->m_LabelMap->CopyInformation( this->m_InputImage );

  this->m_InternalImage = InternalImageType::New();
  this->m_InternalImage->CopyInformation( this->m_InputImage );
  this->m_InternalImage->SetRegions( this->m_InputImage->GetBufferedRegion() );
  this->m_InternalImage->Allocate();
  this->m_InternalImage->FillBuffer( LevelSetType::PlusThreeLayer() );

  LevelSetLabelObjectPointer innerPart = LevelSetLabelObjectType::New();
  innerPart->SetLabel( LevelSetType::MinusThreeLayer() );

  // Precondition labelmap and phi
  InputIteratorType inputIt( this->m_InputImage, this->m_InputImage->GetLargestPossibleRegion() );
  inputIt.GoToBegin();

  InternalIteratorType internalIt( this->m_InternalImage,
                                this->m_InternalImage->GetLargestPossibleRegion() );
  internalIt.GoToBegin();

  while( !inputIt.IsAtEnd() )
    {
    if ( inputIt.Get() != NumericTraits< InputImagePixelType >::Zero )
      {
      innerPart->AddIndex( inputIt.GetIndex() );
      internalIt.Set( LevelSetType::MinusThreeLayer() );
      }
    ++internalIt;
    ++inputIt;
    }

  innerPart->Optimize();
  this->m_LabelMap->AddLabelObject( innerPart );

  FindActiveLayer();

  FindPlusOneMinusOneLayer();

  PropagateToOuterLayers( LevelSetType::MinusOneLayer(),
                          LevelSetType::MinusTwoLayer(),
                          LevelSetType::MinusThreeLayer() );
  PropagateToOuterLayers( LevelSetType::PlusOneLayer(),
                          LevelSetType::PlusTwoLayer(),
                          LevelSetType::PlusThreeLayer() );

  this->m_LabelMap->Optimize();

  this->m_SparseLevelSet->SetLabelMap( this->m_LabelMap );

  // release the memory
  this->m_InternalImage = NULL;
}

template< class TInputImage, typename TLevelSetValueType >
void BinaryImageToWhitakerSparseLevelSetAdaptor< TInputImage, TLevelSetValueType >
::PropagateToOuterLayers( LayerIdType layerToBeScanned, LayerIdType outputLayer, LayerIdType testValue )
{
  const LevelSetLayerType layerPlus1 = this->m_SparseLevelSet->GetLayer( layerToBeScanned );

  LevelSetLayerType & layerPlus2 = this->m_SparseLevelSet->GetLayer( outputLayer );
  const LevelSetOutputType plus2 = static_cast< LevelSetOutputType >( outputLayer );

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  ZeroFluxNeumannBoundaryCondition< InternalImageType > im_nbc;

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &im_nbc );

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


  // iterate on the layer to be scanned
  LevelSetLayerConstIterator nodeIt = layerPlus1.begin();
  LevelSetLayerConstIterator nodeEnd = layerPlus1.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType idx = nodeIt->first;
    neighIt.SetLocation( idx );

    for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
         !it.IsAtEnd();
         ++it )
      {
      // check if in the neighborhood there are values equal to testValue
      if( it.Get() == testValue )
        {
        LevelSetInputType tempIndex = neighIt.GetIndex( it.GetNeighborhoodOffset() );

        layerPlus2.insert( LayerPairType( tempIndex, plus2 ) );
        }
      }
    ++nodeIt;
    }

  LevelSetLabelObjectPointer ObjectPlus2 = LevelSetLabelObjectType::New();
  ObjectPlus2->SetLabel( int(outputLayer) );

  nodeIt = layerPlus2.begin();
  nodeEnd = layerPlus2.end();

  while( nodeIt != nodeEnd )
    {
    if ( testValue != this->m_LabelMap->GetBackgroundValue() )
      {
      this->m_LabelMap->GetLabelObject( testValue )->RemoveIndex( nodeIt->first );
      }
    ObjectPlus2->AddIndex( nodeIt->first );
    this->m_InternalImage->SetPixel( nodeIt->first, outputLayer );
    ++nodeIt;
    }

    ObjectPlus2->Optimize();
    this->m_LabelMap->AddLabelObject( ObjectPlus2 );
}

template< class TInputImage, typename TLevelSetValueType >
void BinaryImageToWhitakerSparseLevelSetAdaptor< TInputImage, TLevelSetValueType >
::FindActiveLayer()
{
  LevelSetLabelObjectPointer labelObject = this->m_LabelMap->GetLabelObject( LevelSetType::MinusThreeLayer() );

  const LevelSetOutputType zero = NumericTraits< LevelSetOutputType >::Zero;

  LevelSetLayerType& layer0 = this->m_SparseLevelSet->GetLayer( LevelSetType::ZeroLayer() );

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  ZeroFluxNeumannBoundaryCondition< InternalImageType > im_nbc;

  NeighborhoodIteratorType neighIt( radius,
    this->m_InternalImage, this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &im_nbc );

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

  typename LevelSetLabelObjectType::ConstIndexIterator lineIt( labelObject );
  lineIt.GoToBegin();

  while( !lineIt.IsAtEnd() )
    {
    const LevelSetInputType & idx = lineIt.GetIndex();

    neighIt.SetLocation( idx );

    for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
         !it.IsAtEnd();
         ++it )
      {
      if( it.Get() == LevelSetType::PlusThreeLayer() )
        {
        layer0.insert( LayerPairType( idx, zero ) );
        break;
        }
      }

    ++lineIt;
    }

  if( !layer0.empty() )
    {
    LevelSetLabelObjectPointer ZeroSet = LevelSetLabelObjectType::New();
    ZeroSet->SetLabel( LevelSetType::ZeroLayer() );

    LevelSetLayerConstIterator nodeIt = layer0.begin();
    LevelSetLayerConstIterator nodeEnd = layer0.end();

    while( nodeIt != nodeEnd )
      {
      this->m_LabelMap->GetLabelObject( LevelSetType::MinusThreeLayer() )->RemoveIndex( nodeIt->first );
      ZeroSet->AddIndex( nodeIt->first );
      this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::ZeroLayer() );
      ++nodeIt;
      }

    ZeroSet->Optimize();
    this->m_LabelMap->AddLabelObject( ZeroSet );
    }
}

template< class TInputImage, typename TLevelSetValueType >
void BinaryImageToWhitakerSparseLevelSetAdaptor< TInputImage, TLevelSetValueType >
::FindPlusOneMinusOneLayer()
{
  const LevelSetOutputType minus1 = - NumericTraits< LevelSetOutputType >::One;
  const LevelSetOutputType plus1 = NumericTraits< LevelSetOutputType >::One;

  const LevelSetLayerType layer0 = this->m_SparseLevelSet->GetLayer( LevelSetType::ZeroLayer() );
  LevelSetLayerType & layerMinus1 = this->m_SparseLevelSet->GetLayer( LevelSetType::MinusOneLayer() );
  LevelSetLayerType & layerPlus1 = this->m_SparseLevelSet->GetLayer( LevelSetType::PlusOneLayer() );

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  ZeroFluxNeumannBoundaryCondition< InternalImageType > im_nbc;

  NeighborhoodIteratorType neighIt( radius,
                                    this->m_InternalImage,
                                    this->m_InternalImage->GetLargestPossibleRegion() );

  neighIt.OverrideBoundaryCondition( &im_nbc );

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

  LevelSetLayerConstIterator nodeIt   = layer0.begin();
  LevelSetLayerConstIterator nodeEnd  = layer0.end();

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType idx = nodeIt->first;

    neighIt.SetLocation( idx );
    for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
         !it.IsAtEnd();
         ++it )
      {
      if( it.Get() == LevelSetType::PlusThreeLayer() )
        {
        LevelSetInputType tempIndex =
          neighIt.GetIndex( it.GetNeighborhoodOffset() );

        layerPlus1.insert( LayerPairType( tempIndex, plus1 ) );
        }
      if( it.Get() == LevelSetType::MinusThreeLayer() )
        {
        LevelSetInputType tempIndex =
          neighIt.GetIndex( it.GetNeighborhoodOffset() );

        layerMinus1.insert( LayerPairType( tempIndex, minus1 ) );
        }
      }
    ++nodeIt;
    }

  LevelSetLabelObjectPointer ObjectMinus1 = LevelSetLabelObjectType::New();
  ObjectMinus1->SetLabel( LevelSetType::MinusOneLayer() );

  nodeIt = layerMinus1.begin();
  nodeEnd = layerMinus1.end();

  while( nodeIt != nodeEnd )
    {
    this->m_LabelMap->GetLabelObject( LevelSetType::MinusThreeLayer() )->RemoveIndex( nodeIt->first );
    ObjectMinus1->AddIndex( nodeIt->first );
    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::MinusOneLayer() );
    ++nodeIt;
    }

  ObjectMinus1->Optimize();
  this->m_LabelMap->AddLabelObject( ObjectMinus1 );

  LevelSetLabelObjectPointer ObjectPlus1 = LevelSetLabelObjectType::New();
  ObjectPlus1->SetLabel( LevelSetType::PlusOneLayer() );

  nodeIt = layerPlus1.begin();
  nodeEnd = layerPlus1.end();

  while( nodeIt != nodeEnd )
    {
    ObjectPlus1->AddIndex( nodeIt->first );
    this->m_InternalImage->SetPixel( nodeIt->first, LevelSetType::PlusOneLayer() );
    ++nodeIt;
    }

  ObjectPlus1->Optimize();
  this->m_LabelMap->AddLabelObject( ObjectPlus1 );
}
}

#endif // __itkBinaryImageToWhitakerSparseLevelSetAdaptor_hxx
