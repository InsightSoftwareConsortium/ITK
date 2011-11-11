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

#ifndef __vtkVisualize2DSparseLevelSetLayersBase_h
#define __vtkVisualize2DSparseLevelSetLayersBase_h

#include "itkLightObject.h"

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

/**
 *  \class vtkVisualize2DSparseLevelSetLayersBase
 *  \tparam TInputImage Input Image Type
 *  \tparam TLevelSet   Level Set Type
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< class TInputImage, class TLevelSet >
class vtkVisualize2DSparseLevelSetLayersBase : public itk::LightObject
{
public:
  typedef vtkVisualize2DSparseLevelSetLayersBase  Self;
  typedef LightObject                             Superclass;
  typedef itk::SmartPointer< Self >               Pointer;
  typedef itk::SmartPointer< const Self >         ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(vtkVisualize2DSparseLevelSetLayersBase, LightObject);

  typedef TInputImage                         InputImageType;
  typedef typename InputImageType::PixelType  InputPixelType;

  typedef TLevelSet                       LevelSetType;
  typedef typename LevelSetType::Pointer  LevelSetPointer;

  typedef itk::ImageToRGBVTKImageFilter< InputImageType >  ConverterType;
  typedef typename ConverterType::Pointer                  ConverterPointer;

  void SetInputImage( const InputImageType * iImage );

  void SetLevelSet( LevelSetType *f );

  void SetScreenCapture( const bool& iCapture );

  void SetCurrentIteration( const itk::IdentifierType& iIteration );

  void Update();

protected:
  vtkVisualize2DSparseLevelSetLayersBase();

  virtual ~vtkVisualize2DSparseLevelSetLayersBase();

  ConverterPointer  m_ImageConverter;
  LevelSetPointer   m_LevelSet;

  vtkSmartPointer< vtkImageData >               m_VTKImage;
  vtkSmartPointer< vtkImageActor >              m_VTKImageActor;
  vtkSmartPointer< vtkCornerAnnotation >        m_Annotation;
  vtkSmartPointer< vtkRenderer >                m_Renderer;
  vtkSmartPointer< vtkRenderWindow >            m_RenWin;
  vtkSmartPointer< vtkRenderWindowInteractor >  m_Iren;

  itk::IdentifierType m_CurrentIteration;
  bool                m_ScreenCapture;

  virtual std::string GetLevelSetRepresentationName() const = 0;

  virtual void AddLayers() = 0;

private:
  vtkVisualize2DSparseLevelSetLayersBase ( const Self& );
  void operator = ( const Self& );
};
#ifndef ITK_MANUAL_INSTANTIATION
#include "vtkVisualize2DSparseLevelSetLayersBase.hxx"
#endif
#endif
