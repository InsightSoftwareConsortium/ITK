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
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKVisualize2DSparseLevelSetLayers);

  using LevelSetType = itk::WhitakerSparseLevelSetImage< TOutput, VDimension >;

  using Self = VTKVisualize2DSparseLevelSetLayers;
  using Superclass = VTKVisualize2DSparseLevelSetLayersBase< TInputImage, LevelSetType >;
  using Pointer = itk::SmartPointer< Self >;
  using ConstPointer = itk::SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKVisualize2DSparseLevelSetLayers,
               VTKVisualize2DSparseLevelSetLayersBase );

  using InputImageType = typename Superclass::InputImageType;
  using InputPixelType = typename Superclass::InputPixelType;

  using LevelSetPointer = typename Superclass::LevelSetPointer;

protected:
  VTKVisualize2DSparseLevelSetLayers();
  ~VTKVisualize2DSparseLevelSetLayers() override;

  void AddLayers() override;

  std::string GetLevelSetRepresentationName() const override;
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
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKVisualize2DSparseLevelSetLayers);

  using LevelSetType = itk::ShiSparseLevelSetImage< VDimension >;

  using Self = VTKVisualize2DSparseLevelSetLayers;
  using Superclass = VTKVisualize2DSparseLevelSetLayersBase< TInputImage, LevelSetType >;
  using Pointer = itk::SmartPointer< Self >;
  using ConstPointer = itk::SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKVisualize2DSparseLevelSetLayers,
               VTKVisualize2DSparseLevelSetLayersBase );

  using InputImageType = typename Superclass::InputImageType;
  using InputPixelType = typename Superclass::InputPixelType;

  using LevelSetPointer = typename Superclass::LevelSetPointer;

protected:
  VTKVisualize2DSparseLevelSetLayers();
  ~VTKVisualize2DSparseLevelSetLayers() override;

  void AddLayers() override;

  std::string GetLevelSetRepresentationName() const override;
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
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKVisualize2DSparseLevelSetLayers);

  using LevelSetType = itk::MalcolmSparseLevelSetImage< VDimension >;

  using Self = VTKVisualize2DSparseLevelSetLayers;
  using Superclass = VTKVisualize2DSparseLevelSetLayersBase< TInputImage, LevelSetType >;
  using Pointer = itk::SmartPointer< Self >;
  using ConstPointer = itk::SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKVisualize2DSparseLevelSetLayers,
               VTKVisualize2DSparseLevelSetLayersBase );

  using InputImageType = typename Superclass::InputImageType;
  using InputPixelType = typename Superclass::InputPixelType;

  using LevelSetPointer = typename Superclass::LevelSetPointer;

protected:
  VTKVisualize2DSparseLevelSetLayers();
  ~VTKVisualize2DSparseLevelSetLayers() override;

  void AddLayers() override;

  std::string GetLevelSetRepresentationName() const override;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKVisualize2DSparseLevelSetLayers.hxx"
#endif
#endif
