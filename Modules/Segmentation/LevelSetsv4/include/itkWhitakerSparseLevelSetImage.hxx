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

#ifndef itkWhitakerSparseLevelSetImage_hxx
#define itkWhitakerSparseLevelSetImage_hxx

#include "itkWhitakerSparseLevelSetImage.h"

namespace itk
{

template< typename TOutput, unsigned int VDimension >
WhitakerSparseLevelSetImage< TOutput, VDimension >
::WhitakerSparseLevelSetImage()
{
  this->InitializeLayers();
  this->InitializeInternalLabelList();
}


template< typename TOutput, unsigned int VDimension >
WhitakerSparseLevelSetImage< TOutput, VDimension >
::~WhitakerSparseLevelSetImage()
{
}


template< typename TOutput, unsigned int VDimension >
typename WhitakerSparseLevelSetImage< TOutput, VDimension >::OutputType
WhitakerSparseLevelSetImage< TOutput, VDimension >
::Evaluate( const InputType& inputIndex ) const
{
  InputType mapIndex = inputIndex - this->m_DomainOffset;
  LayerMapConstIterator layerIt = this->m_Layers.begin();

  OutputType rval = static_cast<OutputType>(ZeroLayer());

  while( layerIt != this->m_Layers.end() )
    {
    LayerConstIterator it = ( layerIt->second ).find( mapIndex );
    if( it != ( layerIt->second ).end() )
      {
      rval = it->second;
      break;
      }

    ++layerIt;
    }
  // if layer not found, look using label map
  if(layerIt == this->m_Layers.end())
    {
    if( this->m_LabelMap.IsNotNull() )
      {
      if( this->m_LabelMap->GetLabelObject( MinusThreeLayer() )->HasIndex( mapIndex ) )
        {
        rval = static_cast<OutputType>( MinusThreeLayer() );
        }
      else
        {
        char status = this->m_LabelMap->GetPixel( mapIndex );
        if( status == this->PlusThreeLayer() )
          {
          rval = static_cast<OutputType>( this->PlusThreeLayer() );
          }
        else
          {
          itkGenericExceptionMacro( <<"status "
                                    << static_cast< int >( status )
                                    << " should be 3 or -3" );
          }
        }
      }
    else
      {
      itkGenericExceptionMacro( <<"Note: m_LabelMap is ITK_NULLPTR"  );
      }
    }
  return rval;
}


template< typename TOutput, unsigned int VDimension >
void
WhitakerSparseLevelSetImage< TOutput, VDimension >
::InitializeLayers()
{
  this->m_Layers.clear();
  this->m_Layers[ MinusTwoLayer() ] = LayerType();
  this->m_Layers[ MinusOneLayer() ] = LayerType();
  this->m_Layers[ ZeroLayer()     ] = LayerType();
  this->m_Layers[ PlusOneLayer()  ] = LayerType();
  this->m_Layers[ PlusTwoLayer()  ] = LayerType();
}


template< typename TOutput, unsigned int VDimension >
void
WhitakerSparseLevelSetImage< TOutput, VDimension >
::InitializeInternalLabelList()
{
  this->m_InternalLabelList.clear();
  this->m_InternalLabelList.push_back( MinusThreeLayer() );
  this->m_InternalLabelList.push_back( MinusTwoLayer() );
  this->m_InternalLabelList.push_back( MinusOneLayer() );
  this->m_InternalLabelList.push_back( ZeroLayer() );
}
}

#endif // itkWhitakerSparseLevelSetImage_hxx
