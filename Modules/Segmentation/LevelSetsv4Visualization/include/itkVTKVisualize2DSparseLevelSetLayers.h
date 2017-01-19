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

#ifndef itkVTKVisualize2DSparseLevelSetLayers_h
#define itkVTKVisualize2DSparseLevelSetLayers_h

#include "itkVTKVisualize2DSparseLevelSetLayersBase.h"

#include "itkWhitakerSparseLevelSetImage.h"
#include "itkShiSparseLevelSetImage.h"
#include "itkMalcolmSparseLevelSetImage.h"

namespace itk
{
/**
 *  \class VTKVisualize2DSparseLevelSetLayers
 *  \tparam TInputImage Input Image Type
 *  \tparam TLevelSetImage  Level Set type
 *
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< typename TInputImage, typename TLevelSet >
class ITK_TEMPLATE_EXPORT VTKVisualize2DSparseLevelSetLayers
{};

// -----------------------------------------------------------------------------
/**
 *  \class VTKVisualize2DSparseLevelSetLayers
 *
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< typename TInputImage, typename TOutput, unsigned int VDimension >
class ITK_TEMPLATE_EXPORT VTKVisualize2DSparseLevelSetLayers<
    TInputImage,
    itk::WhitakerSparseLevelSetImage< TOutput, VDimension > > :
public VTKVisualize2DSparseLevelSetLayersBase<
    TInputImage,
    itk::WhitakerSparseLevelSetImage< TOutput, VDimension > >
{
public:
  typedef itk::WhitakerSparseLevelSetImage< TOutput, VDimension > LevelSetType;

  typedef VTKVisualize2DSparseLevelSetLayers                                    Self;
  typedef VTKVisualize2DSparseLevelSetLayersBase< TInputImage, LevelSetType >   Superclass;
  typedef itk::SmartPointer< Self >                                             Pointer;
  typedef itk::SmartPointer< const Self >                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKVisualize2DSparseLevelSetLayers,
               VTKVisualize2DSparseLevelSetLayersBase );

  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::InputPixelType InputPixelType;

  typedef typename Superclass::LevelSetPointer  LevelSetPointer;

protected:
  VTKVisualize2DSparseLevelSetLayers();
  virtual ~VTKVisualize2DSparseLevelSetLayers();

  void AddLayers();

  std::string GetLevelSetRepresentationName() const;

private:
  VTKVisualize2DSparseLevelSetLayers ( const Self& );
  void operator = ( const Self& );
};

// -----------------------------------------------------------------------------
/**
 *  \class VTKVisualize2DSparseLevelSetLayers
 *
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< typename TInputImage, unsigned int VDimension >
class ITK_TEMPLATE_EXPORT VTKVisualize2DSparseLevelSetLayers<
    TInputImage,
    itk::ShiSparseLevelSetImage< VDimension > > :
public VTKVisualize2DSparseLevelSetLayersBase<
    TInputImage,
    itk::ShiSparseLevelSetImage< VDimension > >
{
public:
  typedef itk::ShiSparseLevelSetImage< VDimension > LevelSetType;

  typedef VTKVisualize2DSparseLevelSetLayers                                    Self;
  typedef VTKVisualize2DSparseLevelSetLayersBase< TInputImage, LevelSetType >   Superclass;
  typedef itk::SmartPointer< Self >                                             Pointer;
  typedef itk::SmartPointer< const Self >                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKVisualize2DSparseLevelSetLayers,
               VTKVisualize2DSparseLevelSetLayersBase );

  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::InputPixelType InputPixelType;

  typedef typename Superclass::LevelSetPointer  LevelSetPointer;

protected:
  VTKVisualize2DSparseLevelSetLayers();
  virtual ~VTKVisualize2DSparseLevelSetLayers();

  void AddLayers();

  std::string GetLevelSetRepresentationName() const;

private:
  VTKVisualize2DSparseLevelSetLayers ( const Self& );
  void operator = ( const Self& );
};

// -----------------------------------------------------------------------------

/**
 *  \class VTKVisualize2DSparseLevelSetLayers
 *
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< typename TInputImage, unsigned int VDimension >
class ITK_TEMPLATE_EXPORT VTKVisualize2DSparseLevelSetLayers<
    TInputImage,
    itk::MalcolmSparseLevelSetImage< VDimension > > :
public VTKVisualize2DSparseLevelSetLayersBase<
    TInputImage,
    itk::MalcolmSparseLevelSetImage< VDimension > >
{
public:
  typedef itk::MalcolmSparseLevelSetImage< VDimension > LevelSetType;

  typedef VTKVisualize2DSparseLevelSetLayers                                    Self;
  typedef VTKVisualize2DSparseLevelSetLayersBase< TInputImage, LevelSetType >   Superclass;
  typedef itk::SmartPointer< Self >                                             Pointer;
  typedef itk::SmartPointer< const Self >                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKVisualize2DSparseLevelSetLayers,
               VTKVisualize2DSparseLevelSetLayersBase );

  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::InputPixelType InputPixelType;

  typedef typename Superclass::LevelSetPointer  LevelSetPointer;

protected:
  VTKVisualize2DSparseLevelSetLayers();
  virtual ~VTKVisualize2DSparseLevelSetLayers();

  void AddLayers();

  std::string GetLevelSetRepresentationName() const;

private:
  VTKVisualize2DSparseLevelSetLayers ( const Self& );
  void operator = ( const Self& );
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKVisualize2DSparseLevelSetLayers.hxx"
#endif
#endif
