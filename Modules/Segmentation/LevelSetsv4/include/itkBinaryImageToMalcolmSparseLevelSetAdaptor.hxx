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

#ifndef __itkBinaryImageToMalcolmSparseLevelSetAdaptor_hxx
#define __itkBinaryImageToMalcolmSparseLevelSetAdaptor_hxx

#include "itkBinaryImageToMalcolmSparseLevelSetAdaptor.h"

namespace itk
{
template< class TInputImage >
BinaryImageToMalcolmSparseLevelSetAdaptor< TInputImage >
::BinaryImageToMalcolmSparseLevelSetAdaptor() : m_InputImage( NULL )
{
  this->m_SparseLevelSet = LevelSetType::New();
}

template< class TInputImage >
BinaryImageToMalcolmSparseLevelSetAdaptor< TInputImage >
::~BinaryImageToMalcolmSparseLevelSetAdaptor()
{
}

template< class TInputImage >
void BinaryImageToMalcolmSparseLevelSetAdaptor< TInputImage >
::Initialize()
{
  if( this->m_InputImage.IsNull() )
    {
    itkGenericExceptionMacro( << "m_InputImage is NULL" );
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
    if ( inputIt.Get() != NumericTraits< InputImagePixelType >::Zero )
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

  this->m_SparseLevelSet->SetLabelMap( this->m_LabelMap );
  this->m_InternalImage = NULL;
}

template< class TInputImage >
void BinaryImageToMalcolmSparseLevelSetAdaptor< TInputImage >
::FindActiveLayer()
{
  LevelSetLabelObjectPointer labelObject = this->m_LabelMap->GetLabelObject( LevelSetType::MinusOneLayer() );

  LevelSetLayerType & layer = this->m_SparseLevelSet->GetLayer( LevelSetType::ZeroLayer() );

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
      layer.insert( LayerPairType( idx, NumericTraits< LevelSetOutputType >::Zero ) );
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

template< class TInputImage >
void BinaryImageToMalcolmSparseLevelSetAdaptor< TInputImage >
::CreateMinimalInterface()
{
  LevelSetLayerType & list_0 = this->m_SparseLevelSet->GetLayer( LevelSetType::ZeroLayer() );

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

      if( tempValue != NumericTraits< LayerIdType >::Zero )
        {
        if( tempValue == LevelSetType::MinusOneLayer() )
          {
          hasNegativeLayerNeighbor = true;
          if( hasPositiveLayerNeighbor )
            {
            break;
            }
          }
        else // ( tempValue == NumericTraits< LevelSetOutputType >::One )
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
#endif // __itkBinaryImageToMalcolmSparseLevelSetAdaptor_hxx
