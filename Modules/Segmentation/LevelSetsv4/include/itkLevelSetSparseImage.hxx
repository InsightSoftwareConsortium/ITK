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

#ifndef itkLevelSetSparseImage_hxx
#define itkLevelSetSparseImage_hxx

#include "itkLevelSetSparseImage.h"

namespace itk
{

template< typename TOutput, unsigned int VDimension >
LevelSetSparseImage< TOutput, VDimension >
::LevelSetSparseImage()
{}


template< typename TOutput, unsigned int VDimension >
LevelSetSparseImage< TOutput, VDimension >
::~LevelSetSparseImage()
{}


template< typename TOutput, unsigned int VDimension >
typename LevelSetSparseImage< TOutput, VDimension >::LayerIdType
LevelSetSparseImage< TOutput, VDimension >
::Status( const InputType& inputIndex ) const
{
  InputType mapIndex = inputIndex - this->m_DomainOffset;
  return this->m_LabelMap->GetPixel( mapIndex );
}


template< typename TOutput, unsigned int VDimension >
void
LevelSetSparseImage< TOutput, VDimension >
::SetLabelMap( LabelMapType* labelMap )
{
  this->m_LabelMap = labelMap;

  typedef typename LabelMapType::SpacingType SpacingType;

  const SpacingType spacing = m_LabelMap->GetSpacing();

  for( unsigned int dim = 0; dim < Dimension; ++dim )
    {
    this->m_NeighborhoodScales[dim] =
        NumericTraits< OutputRealType >::OneValue() / static_cast< OutputRealType >( spacing[dim] );
    }
  this->Modified();
}


template< typename TOutput, unsigned int VDimension >
bool
LevelSetSparseImage< TOutput, VDimension >
::IsInsideDomain( const InputType& inputIndex ) const
{
  const RegionType largestRegion = this->m_LabelMap->GetLargestPossibleRegion();

  InputType mapIndex = inputIndex - this->m_DomainOffset;

  return largestRegion.IsInside( mapIndex );
}


template< typename TOutput, unsigned int VDimension >
void
LevelSetSparseImage< TOutput, VDimension >
::Graft( const DataObject* data )
{
  Superclass::Graft( data );
  const Self *levelSet = dynamic_cast< const Self* >( data );

  if ( !levelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "LevelSetSparseImage::Graft() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  this->m_LabelMap->Graft( levelSet->m_LabelMap );
  if( &m_Layers != &(levelSet->m_Layers) )
    {
    m_Layers.clear();
    LayerMapType newLayers( levelSet->m_Layers );
    std::swap( m_Layers, newLayers );
    }
}


template< typename TOutput, unsigned int VDimension >
const typename LevelSetSparseImage< TOutput, VDimension >::LayerType&
LevelSetSparseImage< TOutput, VDimension >::GetLayer( LayerIdType value ) const
{
  LayerMapConstIterator it = m_Layers.find( value );
  if( it == m_Layers.end() )
    {
    itkGenericExceptionMacro( <<"This layer does not exist" );
    }
  return it->second;
}


template< typename TOutput, unsigned int VDimension >
typename LevelSetSparseImage< TOutput, VDimension >::LayerType&
LevelSetSparseImage< TOutput, VDimension >::GetLayer( LayerIdType value )
{
  LayerMapIterator it = m_Layers.find( value );
  if( it == m_Layers.end() )
    {
    itkGenericExceptionMacro( <<"This layer does not exist" );
    }
  return it->second;
}


template< typename TOutput, unsigned int VDimension >
void
LevelSetSparseImage< TOutput, VDimension >
::SetLayer( LayerIdType value, const LayerType& layer )
{
  const LayerMapIterator it = m_Layers.find( value );
  if( it != m_Layers.end() )
    {
    it->second = layer;
    }
  else
    {
    itkGenericExceptionMacro( << value << "is out of bounds" );
    }
}


template< typename TOutput, unsigned int VDimension >
void
LevelSetSparseImage< TOutput, VDimension >
::Initialize()
{
  Superclass::Initialize();

  this->m_LabelMap = ITK_NULLPTR;
  this->InitializeLayers();
  this->InitializeInternalLabelList();
}


template< typename TOutput, unsigned int VDimension >
void
LevelSetSparseImage< TOutput, VDimension >
::CopyInformation( const DataObject* data )
{
  Superclass::CopyInformation( data );

  const Self *LevelSet = dynamic_cast< const Self* >( data );

  if ( !LevelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::MalcolmSparseLevelSet::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }
}


template< typename TOutput, unsigned int VDimension >
template< typename TLabel >
typename LabelObject< TLabel, VDimension >::Pointer
LevelSetSparseImage< TOutput, VDimension >
::GetAsLabelObject()
{
  typedef LabelObject< TLabel, Dimension > OutputLabelObjectType;
  typename OutputLabelObjectType::Pointer object = OutputLabelObjectType::New();

  if( this->m_InternalLabelList.empty() )
    {
    itkGenericExceptionMacro( << "this->m_InternalLabelList empty" );
    return object;
    }

  typename LayerIdListType::iterator lIt = this->m_InternalLabelList.begin();
  typename LayerIdListType::iterator lEnd = this->m_InternalLabelList.end();

  while( lIt != lEnd )
    {
    LayerIdType id = *lIt;
    LabelObjectPointer labelObject = this->m_LabelMap->GetLabelObject( id );
    SizeValueType numberOfLines = labelObject->GetNumberOfLines();

    for( SizeValueType i = 0; i < numberOfLines; ++i )
      {
      object->AddLine( labelObject->GetLine( i ) );
      }
    ++lIt;
    }
  object->Optimize();

  return object;
}

} // end namespace itk

#endif // itkLevelSetSparseImage_h
