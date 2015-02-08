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

#ifndef itkVTKVisualize2DSparseLevelSetLayers_hxx
#define itkVTKVisualize2DSparseLevelSetLayers_hxx

#include "itkVTKVisualize2DSparseLevelSetLayers.h"

namespace itk
{
template< typename TInputImage, typename TOutput, unsigned int VDimension >
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::WhitakerSparseLevelSetImage< TOutput, VDimension > >
::VTKVisualize2DSparseLevelSetLayers() : Superclass()
{}

template< typename TInputImage, typename TOutput, unsigned int VDimension >
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::WhitakerSparseLevelSetImage< TOutput, VDimension > >
::~VTKVisualize2DSparseLevelSetLayers()
{}

template< typename TInputImage, typename TOutput, unsigned int VDimension >
void
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::WhitakerSparseLevelSetImage< TOutput, VDimension > >
::AddLayers()
{
  typedef typename LevelSetType::LayerType          LayerType;
  typedef typename LevelSetType::LayerConstIterator LayerConstIterator;
  LayerType layer = this->m_LevelSet->GetLayer( LevelSetType::MinusTwoLayer() );

  LayerConstIterator it = layer.begin();

  while( it != layer.end() )
    {
    typename InputImageType::IndexType idx = it->first;
    InputPixelType* vtkpixel =
        static_cast< InputPixelType* >( this->m_VTKImage->GetScalarPointer( idx[0], idx[1], 0 ) );
    vtkpixel[0] = 0;
    vtkpixel[1] = 255;
    vtkpixel[2] = 0;
    ++it;
    }

  layer = this->m_LevelSet->GetLayer( LevelSetType::MinusOneLayer() );

  it = layer.begin();

  while( it != layer.end() )
    {
    typename InputImageType::IndexType idx = it->first;
    InputPixelType* vtkpixel =
        static_cast< InputPixelType* >( this->m_VTKImage->GetScalarPointer( idx[0], idx[1], 0 ) );
    vtkpixel[0] = 255;
    vtkpixel[1] = 255;
    vtkpixel[2] = 0;
    ++it;
    }

  layer = this->m_LevelSet->GetLayer( LevelSetType::ZeroLayer() );

  it = layer.begin();

  while( it != layer.end() )
    {
    typename InputImageType::IndexType idx = it->first;
    InputPixelType* vtkpixel =
        static_cast< InputPixelType* >( this->m_VTKImage->GetScalarPointer( idx[0], idx[1], 0 ) );
    vtkpixel[0] = 255;
    vtkpixel[1] = 0;
    vtkpixel[2] = 0;
    ++it;
    }

  layer = this->m_LevelSet->GetLayer( LevelSetType::PlusOneLayer() );

  it = layer.begin();

  while( it != layer.end() )
    {
    typename InputImageType::IndexType idx = it->first;
    InputPixelType* vtkpixel =
      static_cast< InputPixelType* >( this->m_VTKImage->GetScalarPointer( idx[0], idx[1], 0 ) );
    vtkpixel[0] = 0;
    vtkpixel[1] = 255;
    vtkpixel[2] = 255;
    ++it;
    }

  layer = this->m_LevelSet->GetLayer( LevelSetType::PlusTwoLayer() );

  it = layer.begin();

  while( it != layer.end() )
    {
    typename InputImageType::IndexType idx = it->first;
    InputPixelType* vtkpixel =
        static_cast< InputPixelType* >( this->m_VTKImage->GetScalarPointer( idx[0], idx[1], 0 ) );
    vtkpixel[0] = 0;
    vtkpixel[1] = 0;
    vtkpixel[2] = 255;
    ++it;
    }
  }

template< typename TInputImage, typename TOutput, unsigned int VDimension >
std::string
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::WhitakerSparseLevelSetImage< TOutput, VDimension > >
::GetLevelSetRepresentationName() const
  {
  return std::string( "Whitaker" );
  }

// -----------------------------------------------------------------------------
template< typename TInputImage, unsigned int VDimension >
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::ShiSparseLevelSetImage< VDimension > >
::VTKVisualize2DSparseLevelSetLayers() : Superclass()
  {
  }

template< typename TInputImage, unsigned int VDimension >
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::ShiSparseLevelSetImage< VDimension > >
::~VTKVisualize2DSparseLevelSetLayers()
  {
  }

template< typename TInputImage, unsigned int VDimension >
void
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::ShiSparseLevelSetImage< VDimension > >
::AddLayers()
  {
  typedef typename LevelSetType::LayerType          LayerType;
  typedef typename LevelSetType::LayerConstIterator LayerConstIterator;

  LayerType layer = this->m_LevelSet->GetLayer( LevelSetType::MinusOneLayer() );

  LayerConstIterator it = layer.begin();

  while( it != layer.end() )
    {
    typename InputImageType::IndexType idx = it->first;
    InputPixelType* vtkpixel =
        static_cast< InputPixelType* >( this->m_VTKImage->GetScalarPointer( idx[0], idx[1], 0 ) );
    vtkpixel[0] = 0;
    vtkpixel[1] = 255;
    vtkpixel[2] = 0;
    ++it;
    }

  layer = this->m_LevelSet->GetLayer( LevelSetType::PlusOneLayer() );

  it = layer.begin();

  while( it != layer.end() )
    {
    typename InputImageType::IndexType idx = it->first;
    InputPixelType* vtkpixel =
        static_cast< InputPixelType* >( this->m_VTKImage->GetScalarPointer( idx[0], idx[1], 0 ) );
    vtkpixel[0] = 255;
    vtkpixel[1] = 0;
    vtkpixel[2] = 0;
    ++it;
    }
  }

template< typename TInputImage, unsigned int VDimension >
std::string
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::ShiSparseLevelSetImage< VDimension > >
::GetLevelSetRepresentationName() const
  {
  return std::string( "Shi" );
  }

// -----------------------------------------------------------------------------
template< typename TInputImage, unsigned int VDimension >
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::MalcolmSparseLevelSetImage< VDimension > >
::VTKVisualize2DSparseLevelSetLayers() : Superclass()
{}

template< typename TInputImage, unsigned int VDimension >
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::MalcolmSparseLevelSetImage< VDimension > >
::~VTKVisualize2DSparseLevelSetLayers()
{}

template< typename TInputImage, unsigned int VDimension >
void
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::MalcolmSparseLevelSetImage< VDimension > >
::AddLayers()
{


  typedef typename LevelSetType::LayerType          LayerType;
  typedef typename LevelSetType::LayerConstIterator LayerConstIterator;

  LayerType layer = this->m_LevelSet->GetLayer( LevelSetType::ZeroLayer() );

  LayerConstIterator it = layer.begin();

  while( it != layer.end() )
    {
    typename InputImageType::IndexType idx = it->first;
    InputPixelType* vtkpixel =
      static_cast< InputPixelType* >( this->m_VTKImage->GetScalarPointer( idx[0], idx[1], 0 ) );
    vtkpixel[0] = 255;
    vtkpixel[1] = 0;
    vtkpixel[2] = 0;
    ++it;
    }
}

template< typename TInputImage, unsigned int VDimension >
std::string
VTKVisualize2DSparseLevelSetLayers<
  TInputImage,
  itk::MalcolmSparseLevelSetImage< VDimension > >
::GetLevelSetRepresentationName() const
  {
  return std::string( "Malcolm" );
  }
}
#endif
