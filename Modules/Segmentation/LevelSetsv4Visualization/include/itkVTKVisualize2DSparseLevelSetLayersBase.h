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

#ifndef itkVTKVisualize2DSparseLevelSetLayersBase_h
#define itkVTKVisualize2DSparseLevelSetLayersBase_h

#include "itkVTKVisualizeImageLevelSet.h"

#include "itkConceptChecking.h"

#include "itkImageToRGBVTKImageFilter.h"

#include "vtkCornerAnnotation.h"
#include "vtkImageData.h"
#include "vtkLookupTable.h"
#include "vtkMarchingSquares.h"
#include "vtkPolyDataMapper.h"
#include "vtkActor.h"
#include "vtkImageActor.h"
#include "vtkScalarBarActor.h"
#include "vtkProperty.h"
#include "vtkRenderer.h"
#include "vtkRenderWindowInteractor.h"
#include "vtkRenderWindow.h"
#include "vtkImageShiftScale.h"

#include "vtkCaptureScreen.h"
#include "vtkPNGWriter.h"

namespace itk
{
/**
 *  \class VTKVisualize2DSparseLevelSetLayersBase
 *  \tparam TInputImage Input Image Type
 *  \tparam TLevelSet   Level Set Type
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< typename TInputImage, typename TLevelSet >
class ITK_TEMPLATE_EXPORT VTKVisualize2DSparseLevelSetLayersBase :
    public VTKVisualizeImageLevelSet< TInputImage, ImageToRGBVTKImageFilter< TInputImage > >
{
public:
  typedef ImageToRGBVTKImageFilter< TInputImage > ConverterType;
  typedef typename ConverterType::Pointer         ConverterPointer;

  typedef VTKVisualize2DSparseLevelSetLayersBase                  Self;
  typedef VTKVisualizeImageLevelSet< TInputImage, ConverterType > Superclass;
  typedef itk::SmartPointer< Self >                               Pointer;
  typedef itk::SmartPointer< const Self >                         ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKVisualize2DSparseLevelSetLayersBase, VTKVisualizeImageLevelSet );

  typedef TInputImage                         InputImageType;
  typedef typename InputImageType::PixelType  InputPixelType;

  typedef TLevelSet                       LevelSetType;
  typedef typename LevelSetType::Pointer  LevelSetPointer;

  virtual void SetInputImage( const InputImageType* image );
  void SetLevelSet( LevelSetType * levelSet );

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro( Is2Dimensional,
                   ( Concept::SameDimension< LevelSetType::Dimension, 2 > ) );
#endif

protected:
  VTKVisualize2DSparseLevelSetLayersBase();
  virtual ~VTKVisualize2DSparseLevelSetLayersBase();

  LevelSetPointer                   m_LevelSet;
  vtkSmartPointer< vtkImageData >   m_VTKImage;
  vtkSmartPointer< vtkImageActor >  m_VTKImageActor;

  virtual void PrepareVTKPipeline();

  virtual std::string GetLevelSetRepresentationName() const = 0;

  virtual void AddLayers() = 0;

private:
  VTKVisualize2DSparseLevelSetLayersBase ( const Self& );
  void operator = ( const Self& );
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKVisualize2DSparseLevelSetLayersBase.hxx"
#endif
#endif
