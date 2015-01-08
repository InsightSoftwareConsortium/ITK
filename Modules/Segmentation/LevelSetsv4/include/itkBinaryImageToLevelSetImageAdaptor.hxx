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

#ifndef itkBinaryImageToLevelSetImageAdaptor_hxx
#define itkBinaryImageToLevelSetImageAdaptor_hxx

#include "itkBinaryImageToLevelSetImageAdaptor.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TLevelSetImage >
BinaryImageToLevelSetImageAdaptor< TInputImage, LevelSetDenseImage< TLevelSetImage > >
::BinaryImageToLevelSetImageAdaptor()
{
  this->m_SignedDistanceTransformFilter = SignedMaurerDistanceMapImageFilter< InputImageType, LevelSetImageType >::New();
}

template< typename TInputImage, typename TLevelSetImage >
BinaryImageToLevelSetImageAdaptor< TInputImage, LevelSetDenseImage< TLevelSetImage > >
::~BinaryImageToLevelSetImageAdaptor()
{}

template< typename TInputImage, typename TLevelSetImage >
void
BinaryImageToLevelSetImageAdaptor< TInputImage, LevelSetDenseImage< TLevelSetImage > >
::Initialize()
{
  if( this->m_InputImage.IsNull() )
    {
    itkGenericExceptionMacro( "m_InputImage is ITK_NULLPTR" );
    }

  if( m_SignedDistanceTransformFilter.IsNull() )
    {
    itkGenericExceptionMacro( "m_SignedDistanceTransformFilter is ITK_NULLPTR" );
    }
  m_SignedDistanceTransformFilter->SetInput( this->m_InputImage );
  m_SignedDistanceTransformFilter->Update();

  typename LevelSetImageType::Pointer tempImage = LevelSetImageType::New();
  tempImage->Graft( m_SignedDistanceTransformFilter->GetOutput() );

  this->m_LevelSet = LevelSetType::New();
  this->m_LevelSet->SetImage( tempImage );
}


template< typename TInput, typename TOutput >
BinaryImageToLevelSetImageAdaptor<
  TInput,
  WhitakerSparseLevelSetImage< TOutput, TInput::ImageDimension > >
::BinaryImageToLevelSetImageAdaptor()
{}

template< typename TInput, typename TOutput >
BinaryImageToLevelSetImageAdaptor<
  TInput,
  WhitakerSparseLevelSetImage< TOutput, TInput::ImageDimension > >
::~BinaryImageToLevelSetImageAdaptor()
{
}

template< typename TInput, typename TOutput >
void
BinaryImageToLevelSetImageAdaptor<
  TInput,
  WhitakerSparseLevelSetImage< TOutput, TInput::ImageDimension > >
::Initialize()
{
  if( this->m_InputImage.IsNull() )
    {
    itkGenericExceptionMacro( << "m_InputImage is ITK_NULLPTR" );
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
    if ( inputIt.Get() != NumericTraits< InputImagePixelType >::ZeroValue() )
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

  this->m_LevelSet->SetLabelMap( this->m_LabelMap );

  // release the memory
  this->m_InternalImage = ITK_NULLPTR;
}

template< typename TInput, typename TOutput >
void
BinaryImageToLevelSetImageAdaptor<
  TInput,
  WhitakerSparseLevelSetImage< TOutput, TInput::ImageDimension > >
::PropagateToOuterLayers( LayerIdType layerToBeScanned, LayerIdType outputLayer, LayerIdType testValue )
{
  const LevelSetLayerType layerPlus1 = this->m_LevelSet->GetLayer( layerToBeScanned );

  LevelSetLayerType & layerPlus2 = this->m_LevelSet->GetLayer( outputLayer );
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

template< typename TInput, typename TOutput >
void
BinaryImageToLevelSetImageAdaptor<
  TInput,
  WhitakerSparseLevelSetImage< TOutput, TInput::ImageDimension > >
::FindActiveLayer()
{
  LevelSetLabelObjectPointer labelObject = this->m_LabelMap->GetLabelObject( LevelSetType::MinusThreeLayer() );

  const LevelSetOutputType zero = NumericTraits< LevelSetOutputType >::ZeroValue();

  LevelSetLayerType& layer0 = this->m_LevelSet->GetLayer( LevelSetType::ZeroLayer() );

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

template< typename TInput, typename TOutput >
void
BinaryImageToLevelSetImageAdaptor<
  TInput,
  WhitakerSparseLevelSetImage< TOutput, TInput::ImageDimension > >
::FindPlusOneMinusOneLayer()
{
  const LevelSetOutputType minus1 = - NumericTraits< LevelSetOutputType >::OneValue();
  const LevelSetOutputType plus1 = NumericTraits< LevelSetOutputType >::OneValue();

  const LevelSetLayerType layer0 = this->m_LevelSet->GetLayer( LevelSetType::ZeroLayer() );
  LevelSetLayerType & layerMinus1 = this->m_LevelSet->GetLayer( LevelSetType::MinusOneLayer() );
  LevelSetLayerType & layerPlus1 = this->m_LevelSet->GetLayer( LevelSetType::PlusOneLayer() );

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

////////////////////////////////////////////////////////////////////////////////

template< typename TInput >
BinaryImageToLevelSetImageAdaptor< TInput, ShiSparseLevelSetImage< TInput::ImageDimension > >
::BinaryImageToLevelSetImageAdaptor()
{}

template< typename TInput >
BinaryImageToLevelSetImageAdaptor< TInput, ShiSparseLevelSetImage< TInput::ImageDimension > >
::~BinaryImageToLevelSetImageAdaptor()
{
}

template< typename TInput >
void BinaryImageToLevelSetImageAdaptor< TInput, ShiSparseLevelSetImage< TInput::ImageDimension > >
::Initialize()
{
  if( this->m_InputImage.IsNull() )
    {
    itkGenericExceptionMacro( << "m_InputImage is ITK_NULLPTR" );
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
  InputIteratorType iIt( this->m_InputImage, this->m_InputImage->GetLargestPossibleRegion() );
  iIt.GoToBegin();

  InternalIteratorType labelIt( this->m_InternalImage,
                                this->m_InternalImage->GetLargestPossibleRegion() );
  labelIt.GoToBegin();

  while( !iIt.IsAtEnd() )
    {
    if ( iIt.Get() != NumericTraits< InputImagePixelType >::ZeroValue() )
      {
      innerPart->AddIndex( iIt.GetIndex() );
      labelIt.Set( LevelSetType::MinusThreeLayer() );
      }
    ++labelIt;
    ++iIt;
    }

  innerPart->Optimize();
  this->m_LabelMap->AddLabelObject( innerPart );

  FindActiveLayer();

  this->m_LevelSet->SetLabelMap( this->m_LabelMap );
  this->m_InternalImage = ITK_NULLPTR;
}


template< typename TInput >
void BinaryImageToLevelSetImageAdaptor< TInput, ShiSparseLevelSetImage< TInput::ImageDimension > >
::FindActiveLayer()
{
  LevelSetLabelObjectPointer labelObject = this->m_LabelMap->GetLabelObject( LevelSetType::MinusThreeLayer() );

  LevelSetLayerType & layerMinus1 = this->m_LevelSet->GetLayer( LevelSetType::MinusOneLayer() );
  LevelSetLayerType & layerPlus1  = this->m_LevelSet->GetLayer( LevelSetType::PlusOneLayer() );

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  ZeroFluxNeumannBoundaryCondition< InternalImageType > im_nbc;

  NeighborhoodIteratorType neighIt( radius, this->m_InternalImage, this->m_InternalImage->GetLargestPossibleRegion() );

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

    bool boundary = false;

    for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
         !it.IsAtEnd(); ++it )
      {
      if( it.Get() == LevelSetType::PlusThreeLayer() )
        {
        LevelSetInputType tempIndex = neighIt.GetIndex( it.GetNeighborhoodOffset() );

        layerPlus1.insert( LayerPairType( tempIndex, LevelSetOutputType(1.0) ) );
        boundary = true;
        }
      }

    if( boundary )
      {
      layerMinus1.insert( LayerPairType( idx, LevelSetOutputType(-1.0) ) );
      }

    ++lineIt;
    }

  LevelSetLabelObjectPointer ObjectMinus1 = LevelSetLabelObjectType::New();
  ObjectMinus1->SetLabel( LevelSetType::MinusOneLayer() );

  LevelSetLayerConstIterator nodeIt = layerMinus1.begin();
  LevelSetLayerConstIterator nodeEnd = layerMinus1.end();

  while( nodeIt != nodeEnd )
    {
    this->m_LabelMap->GetLabelObject( LevelSetType::MinusThreeLayer() )->RemoveIndex( nodeIt->first );
    ObjectMinus1->AddIndex( nodeIt->first );
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
    ++nodeIt;
    }

  ObjectPlus1->Optimize();
  this->m_LabelMap->AddLabelObject( ObjectPlus1 );
}

////////////////////////////////////////////////////////////////////////////////

template< typename TInput >
BinaryImageToLevelSetImageAdaptor< TInput,MalcolmSparseLevelSetImage< TInput::ImageDimension > >
::BinaryImageToLevelSetImageAdaptor() : Superclass()
{}

template< typename TInput >
BinaryImageToLevelSetImageAdaptor< TInput,MalcolmSparseLevelSetImage< TInput::ImageDimension > >
::~BinaryImageToLevelSetImageAdaptor()
{
}

template< typename TInput >
void BinaryImageToLevelSetImageAdaptor< TInput,MalcolmSparseLevelSetImage< TInput::ImageDimension > >
::Initialize()
{
  if( this->m_InputImage.IsNull() )
    {
    itkGenericExceptionMacro( << "m_InputImage is ITK_NULLPTR" );
    }

  this->m_LabelMap = LevelSetLabelMapType::New();
  this->m_LabelMap->SetBackgroundValue( LevelSetType::PlusOneLayer() );
  this->m_LabelMap->CopyInformation( this->m_InputImage );

  this->m_InternalImage = InternalImageType::New();
  this->m_InternalImage->CopyInformation( this->m_InputImage );
  this->m_InternalImage->SetRegions( this->m_InputImage->GetBufferedRegion() );
  this->m_InternalImage->Allocate();
  this->m_InternalImage->FillBuffer( LevelSetType::PlusOneLayer() );

  LevelSetLabelObjectPointer innerPart = LevelSetLabelObjectType::New();
  innerPart->SetLabel( LevelSetType::MinusOneLayer() );

  // Precondition labelmap and phi
  InputIteratorType inputIt( this->m_InputImage, this->m_InputImage->GetLargestPossibleRegion() );
  inputIt.GoToBegin();

  InternalIteratorType internalIt( this->m_InternalImage,
                                this->m_InternalImage->GetLargestPossibleRegion() );
  internalIt.GoToBegin();

  while( !inputIt.IsAtEnd() )
    {
    if ( inputIt.Get() != NumericTraits< InputImagePixelType >::ZeroValue() )
      {
      innerPart->AddIndex( inputIt.GetIndex() );
      internalIt.Set( LevelSetType::MinusOneLayer() );
      }
    ++internalIt;
    ++inputIt;
    }

  innerPart->Optimize();
  this->m_LabelMap->AddLabelObject( innerPart );

  this->FindActiveLayer();

  this->CreateMinimalInterface();

  this->m_LevelSet->SetLabelMap( this->m_LabelMap );
  this->m_InternalImage = ITK_NULLPTR;
}

template< typename TInput >
void BinaryImageToLevelSetImageAdaptor< TInput,MalcolmSparseLevelSetImage< TInput::ImageDimension > >
::FindActiveLayer()
{
  LevelSetLabelObjectPointer labelObject = this->m_LabelMap->GetLabelObject( LevelSetType::MinusOneLayer() );

  LevelSetLayerType & layer = this->m_LevelSet->GetLayer( LevelSetType::ZeroLayer() );

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

    bool ZeroSet = false;

    for( typename NeighborhoodIteratorType::Iterator it = neighIt.Begin();
         !it.IsAtEnd();
         ++it )
      {
      if( it.Get() == LevelSetType::PlusOneLayer() )
        {
        ZeroSet = true;
        }
      }

    if( ZeroSet )
      {
      layer.insert( LayerPairType( idx, NumericTraits< LevelSetOutputType >::ZeroValue() ) );
      this->m_InternalImage->SetPixel( idx, LevelSetType::ZeroLayer() );
      }

    ++lineIt;
    }

  LevelSetLabelObjectPointer ObjectZero = LevelSetLabelObjectType::New();
  ObjectZero->SetLabel( LevelSetType::ZeroLayer() );

  LevelSetLayerIterator nodeIt = layer.begin();
  LevelSetLayerIterator nodeEnd = layer.end();

  while( nodeIt != nodeEnd )
    {
    labelObject->RemoveIndex( nodeIt->first );
    ObjectZero->AddIndex( nodeIt->first );
    ++nodeIt;
    }

  ObjectZero->Optimize();
  this->m_LabelMap->AddLabelObject( ObjectZero );
}

template< typename TInput >
void BinaryImageToLevelSetImageAdaptor< TInput,MalcolmSparseLevelSetImage< TInput::ImageDimension > >
::CreateMinimalInterface()
{
  LevelSetLayerType & list_0 = this->m_LevelSet->GetLayer( LevelSetType::ZeroLayer() );

  ZeroFluxNeumannBoundaryCondition< InternalImageType > sp_nbc;

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

  while( nodeIt != nodeEnd )
    {
    LevelSetInputType currentIdx = nodeIt->first;

    neighIt.SetLocation( currentIdx );

    bool hasPositiveLayerNeighbor = false;
    bool hasNegativeLayerNeighbor = false;

    for( typename NeighborhoodIteratorType::Iterator
        i = neighIt.Begin(); !i.IsAtEnd(); ++i )
      {
      LayerIdType tempValue = i.Get();

      if( tempValue != NumericTraits< LayerIdType >::ZeroValue() )
        {
        if( tempValue == LevelSetType::MinusOneLayer() )
          {
          hasNegativeLayerNeighbor = true;
          if( hasPositiveLayerNeighbor )
            {
            break;
            }
          }
        else // ( tempValue == NumericTraits< LevelSetOutputType >::OneValue() )
          {
          hasPositiveLayerNeighbor = true;
          if( hasNegativeLayerNeighbor )
            {
            break;
            }
          }
        }
      }

    if( hasNegativeLayerNeighbor && !hasPositiveLayerNeighbor )
      {
      LevelSetLayerIterator tempIt = nodeIt;
      ++nodeIt;
      list_0.erase( tempIt );

      this->m_LabelMap->GetLabelObject( LevelSetType::ZeroLayer() )->RemoveIndex( currentIdx );
      this->m_LabelMap->GetLabelObject( LevelSetType::MinusOneLayer() )->AddIndex( currentIdx );
      }
    else
      {
      if( hasPositiveLayerNeighbor && !hasNegativeLayerNeighbor )
        {
        LevelSetLayerIterator tempIt = nodeIt;
        ++nodeIt;
        list_0.erase( tempIt );

        this->m_LabelMap->GetLabelObject( LevelSetType::ZeroLayer() )->RemoveIndex( currentIdx );
        }
      else
        {
        ++nodeIt;
        }
      }
    }
  }
}

#endif // itkBinaryImageToLevelSetImageAdaptor_hxx
