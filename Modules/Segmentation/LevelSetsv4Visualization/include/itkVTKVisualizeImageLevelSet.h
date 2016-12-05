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
#ifndef itkVTKVisualizeImageLevelSet_h
#define itkVTKVisualizeImageLevelSet_h

#include "itkLightObject.h"
#include "itkIntTypes.h"

#include "vtkRenderer.h"
#include "vtkRenderWindow.h"
#include "vtkSmartPointer.h"
#include "vtkImageShiftScale.h"
#include "vtkImageActor.h"
#include "vtkCornerAnnotation.h"

namespace itk
{

/** \class VTKVisualizeImageLevelSet
 * \brief Base class for visualizing level sets defined on an image with VTK.
 *
 * \tparam TInputImage Input image the level set evolves on.
 * \tparam TInputImageConverter ITK filter to convert the input image to a VTK
 * object.
 *
 * \ingroup ITKLevelSetsv4Visualization
 */
template< typename TInputImage, typename TInputImageConverter >
class ITK_TEMPLATE_EXPORT VTKVisualizeImageLevelSet: public LightObject
{
public:
  typedef VTKVisualizeImageLevelSet  Self;
  typedef LightObject                Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( VTKVisualizeImageLevelSet, LightObject );

  /** Type of the input image the level set evolves on. */
  typedef TInputImage     InputImageType;

  /** Type of the ITK filter to convert the input image to a VTK object. */
  typedef TInputImageConverter InputImageConverterType;

  /** Set the input image the level set is being evolved over. */
  virtual void SetInputImage( const InputImageType * inputImage );

  /** Do a screen capture at every iteration. */
  void SetScreenCapture( const bool iCapture );
  bool GetScreenCapture() const;

  /** Set up the VTK pipeline and render.  Do a screen capture if the option is
   * set. */
  virtual void Update();

  /** Get the renderer. */
  vtkRenderer * GetRenderer();

  /** Set/Get the render window. */
  vtkRenderWindow * GetRenderWindow();
  void SetRenderWindow( vtkRenderWindow * renderWindow );

  /** Set/Get the current iteration.  Used for the filename during screen
   * capture. */
  void SetCurrentIteration( const IdentifierType iteration );
  IdentifierType GetCurrentIteration() const;

  /** Set the filename prefix for screen captures. */
  void SetScreenCapturePrefix( const char * prefix );

protected:
  VTKVisualizeImageLevelSet();
  virtual ~VTKVisualizeImageLevelSet();

  /** Setup the VTK pipeline for a Render() call. */
  virtual void PrepareVTKPipeline() = 0;

  typename InputImageConverterType::Pointer m_InputImageConverter;

  vtkSmartPointer< vtkRenderer >          m_Renderer;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKVisualizeImageLevelSet);

  bool                               m_ScreenCapture;
  IdentifierType                     m_CurrentIteration;
  std::string                        m_ScreenCapturePrefix;

  vtkSmartPointer< vtkRenderWindow >      m_RenderWindow;
  vtkSmartPointer< vtkCornerAnnotation >  m_Annotation;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKVisualizeImageLevelSet.hxx"
#endif

#endif
