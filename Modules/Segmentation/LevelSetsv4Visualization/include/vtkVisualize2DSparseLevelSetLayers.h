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

#ifndef __vtkVisualize2DSparseLevelSetLayers_h
#define __vtkVisualize2DSparseLevelSetLayers_h

#include "vtkVisualize2DSparseLevelSetLayersBase.h"

#include "itkWhitakerSparseLevelSetImage.h"
#include "itkShiSparseLevelSetImage.h"
#include "itkMalcolmSparseLevelSetImage.h"

/**
 *  \class vtkVisualize2DSparseLevelSetLayers
 *  \tparam TInputImage Input Image Type
 *  \tparam TLevelSetImage  Level Set type
 *
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< class TInputImage, class TLevelSet >
class vtkVisualize2DSparseLevelSetLayers
{};

// -----------------------------------------------------------------------------
/**
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< class TInputImage, typename TOutput, unsigned int VDimension >
class vtkVisualize2DSparseLevelSetLayers<
    TInputImage,
    itk::WhitakerSparseLevelSetImage< TOutput, VDimension > > :
public vtkVisualize2DSparseLevelSetLayersBase<
    TInputImage,
    itk::WhitakerSparseLevelSetImage< TOutput, VDimension > >
{
public:
  typedef itk::WhitakerSparseLevelSetImage< TOutput, VDimension > LevelSetType;

  typedef vtkVisualize2DSparseLevelSetLayers                                    Self;
  typedef vtkVisualize2DSparseLevelSetLayersBase< TInputImage, LevelSetType >   Superclass;
  typedef itk::SmartPointer< Self >                                             Pointer;
  typedef itk::SmartPointer< const Self >                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(vtkVisualize2DSparseLevelSetLayers,
               vtkVisualize2DSparseLevelSetLayersBase );

  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::InputPixelType InputPixelType;

  typedef typename Superclass::LevelSetPointer  LevelSetPointer;

protected:
  vtkVisualize2DSparseLevelSetLayers() : Superclass() {}
  virtual ~vtkVisualize2DSparseLevelSetLayers() {}

  void AddLayers()
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

  std::string GetLevelSetRepresentationName() const
    {
    return std::string( "Whitaker" );
    }

private:
  vtkVisualize2DSparseLevelSetLayers ( const Self& );
  void operator = ( const Self& );
};

// -----------------------------------------------------------------------------
/**
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< class TInputImage, unsigned int VDimension >
class vtkVisualize2DSparseLevelSetLayers<
    TInputImage,
    itk::ShiSparseLevelSetImage< VDimension > > :
public vtkVisualize2DSparseLevelSetLayersBase<
    TInputImage,
    itk::ShiSparseLevelSetImage< VDimension > >
{
public:
  typedef itk::ShiSparseLevelSetImage< VDimension > LevelSetType;

  typedef vtkVisualize2DSparseLevelSetLayers                                    Self;
  typedef vtkVisualize2DSparseLevelSetLayersBase< TInputImage, LevelSetType >   Superclass;
  typedef itk::SmartPointer< Self >                                             Pointer;
  typedef itk::SmartPointer< const Self >                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(vtkVisualize2DSparseLevelSetLayers,
               vtkVisualize2DSparseLevelSetLayersBase );

  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::InputPixelType InputPixelType;

  typedef typename Superclass::LevelSetPointer  LevelSetPointer;

protected:
  vtkVisualize2DSparseLevelSetLayers() : Superclass() {}
  virtual ~vtkVisualize2DSparseLevelSetLayers() {}

  void AddLayers()
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

  std::string GetLevelSetRepresentationName() const
    {
    return std::string( "Shi" );
    }

private:
  vtkVisualize2DSparseLevelSetLayers ( const Self& );
  void operator = ( const Self& );
};

// -----------------------------------------------------------------------------

/**
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< class TInputImage, unsigned int VDimension >
class vtkVisualize2DSparseLevelSetLayers<
    TInputImage,
    itk::MalcolmSparseLevelSetImage< VDimension > > :
public vtkVisualize2DSparseLevelSetLayersBase<
    TInputImage,
    itk::MalcolmSparseLevelSetImage< VDimension > >
{
public:
  typedef itk::MalcolmSparseLevelSetImage< VDimension > LevelSetType;

  typedef vtkVisualize2DSparseLevelSetLayers                                    Self;
  typedef vtkVisualize2DSparseLevelSetLayersBase< TInputImage, LevelSetType >   Superclass;
  typedef itk::SmartPointer< Self >                                             Pointer;
  typedef itk::SmartPointer< const Self >                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(vtkVisualize2DSparseLevelSetLayers,
               vtkVisualize2DSparseLevelSetLayersBase );

  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::InputPixelType InputPixelType;

  typedef typename Superclass::LevelSetPointer  LevelSetPointer;

protected:
  vtkVisualize2DSparseLevelSetLayers() : Superclass() {}
  virtual ~vtkVisualize2DSparseLevelSetLayers() {}

  void AddLayers()
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

  std::string GetLevelSetRepresentationName() const
    {
    return std::string( "Malcolm" );
    }

private:
  vtkVisualize2DSparseLevelSetLayers ( const Self& );
  void operator = ( const Self& );
};

#endif
