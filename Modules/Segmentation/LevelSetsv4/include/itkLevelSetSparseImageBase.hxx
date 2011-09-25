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

#ifndef __itkLevelSetSparseImageBase_hxx
#define __itkLevelSetSparseImageBase_hxx

#include "itkLevelSetSparseImageBase.h"

namespace itk
{
template< typename TOutput, unsigned int VDimension >
LevelSetSparseImageBase< TOutput, VDimension >
::LevelSetSparseImageBase()
{}

template< typename TOutput, unsigned int VDimension >
LevelSetSparseImageBase< TOutput, VDimension >
::~LevelSetSparseImageBase()
{}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename LevelSetSparseImageBase< TOutput, VDimension >::LayerIdType
LevelSetSparseImageBase< TOutput, VDimension >
::Status( const InputType& iP ) const
{
  return this->m_LabelMap->GetPixel( iP );
}


template< typename TOutput, unsigned int VDimension >
void
LevelSetSparseImageBase< TOutput, VDimension >
::SetLabelMap( LabelMapType* iLabelMap )
{
  this->m_LabelMap = iLabelMap;

  typedef typename LabelMapType::SpacingType SpacingType;

  const SpacingType spacing = m_LabelMap->GetSpacing();

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    this->m_NeighborhoodScales[dim] =
        NumericTraits< OutputRealType >::One / static_cast< OutputRealType >( spacing[dim ] );
    }
  this->Modified();
}

template< typename TOutput, unsigned int VDimension >
bool
LevelSetSparseImageBase< TOutput, VDimension >
::IsInside( const InputType& iP ) const
{
  const RegionType largestRegion = this->m_LabelMap->GetLargestPossibleRegion();

  return largestRegion.IsInside( iP );
}

template< typename TOutput, unsigned int VDimension >
void
LevelSetSparseImageBase< TOutput, VDimension >
::Graft( const DataObject* data )
{
  Superclass::Graft( data );
  const Self *LevelSet = 0;

  try
    {
    LevelSet = dynamic_cast< const Self* >( data );
    }
  catch( ... )
    {
    // mesh could not be cast back down
    itkExceptionMacro( << "itk::MalcolmSparseLevelSetBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  if ( !LevelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::MalcolmSparseLevelSetBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  this->m_LabelMap->Graft( LevelSet->m_LabelMap );
  this->m_Layers = LevelSet->m_Layers;
}
// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
const typename LevelSetSparseImageBase< TOutput, VDimension >::LayerType&
LevelSetSparseImageBase< TOutput, VDimension >::GetLayer( LayerIdType iVal ) const
{
  LayerMapConstIterator it = m_Layers.find( iVal );
  if( it == m_Layers.end() )
    {
    itkGenericExceptionMacro( <<"This layer does not exist" );
    }
  return it->second;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename LevelSetSparseImageBase< TOutput, VDimension >::LayerType&
LevelSetSparseImageBase< TOutput, VDimension >::GetLayer( LayerIdType iVal )
{
  LayerMapIterator it = m_Layers.find( iVal );
  if( it == m_Layers.end() )
    {
    itkGenericExceptionMacro( <<"This layer does not exist" );
    }
  return it->second;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
LevelSetSparseImageBase< TOutput, VDimension >
::SetLayer( LayerIdType iVal, const LayerType& iLayer )
{
  LayerMapIterator it = m_Layers.find( iVal );
  if( it != m_Layers.end() )
    {
    it->second = iLayer;
    }
  else
    {
    itkGenericExceptionMacro( <<iVal << "is out of bounds" );
    }
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
LevelSetSparseImageBase< TOutput, VDimension >
::Initialize()
{
  Superclass::Initialize();

  this->m_LabelMap = 0;
  this->InitializeLayers();
  this->InitializeInternalLabelList();
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
LevelSetSparseImageBase< TOutput, VDimension >
::CopyInformation( const DataObject* data )
{
  Superclass::CopyInformation( data );

  const Self *LevelSet = NULL;
  try
    {
    LevelSet = dynamic_cast< const Self* >( data );
    }
  catch( ... )
    {
    // LevelSet could not be cast back down
    itkExceptionMacro( << "itk::MalcolmSparseLevelSetBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  if ( !LevelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::MalcolmSparseLevelSetBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }
}

template< typename TOutput, unsigned int VDimension >
template< class TLabel >
typename LabelObject< TLabel, VDimension >::Pointer
LevelSetSparseImageBase< TOutput, VDimension >
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


#endif // __itkLevelSetSparseImageBase_h
