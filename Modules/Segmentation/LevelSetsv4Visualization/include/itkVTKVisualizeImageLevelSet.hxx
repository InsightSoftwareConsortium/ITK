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
#ifndef __itkVTKVisualizeImageLevelSet_hxx
#define __itkVTKVisualizeImageLevelSet_hxx

#include "itkVTKVisualizeImageLevelSet.h"

#include "vtkCaptureScreen.h"
#include "vtkPNGWriter.h"

namespace itk
{

template < class TInputImage, class TInputImageConverter >
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::VTKVisualizeImageLevelSet():
  m_ScreenCapture( false ),
  m_ScreenCapturePrefix( "levelset_" )
{
  this->m_CurrentIteration = 0;

  this->m_InputImageConverter = InputImageConverterType::New();

  this->m_Renderer = vtkSmartPointer< vtkRenderer >::New();
  this->m_Renderer->SetBackground( 0.5, 0.5, 0.5 );
  this->m_RenderWindow = vtkSmartPointer< vtkRenderWindow >::New();
  this->m_RenderWindow->AddRenderer( this->m_Renderer );

  this->m_Annotation = vtkSmartPointer< vtkCornerAnnotation >::New();
  this->m_Renderer->AddActor2D( this->m_Annotation );
}

template < class TInputImage, class TInputImageConverter >
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::~VTKVisualizeImageLevelSet()
{
}

template < class TInputImage, class TInputImageConverter >
void
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::SetInputImage( const InputImageType * inputImage )
{
  this->m_InputImageConverter->SetInput( inputImage );
  this->m_InputImageConverter->Update();

  vtkSmartPointer< vtkImageShiftScale >   ImageShiftScale = vtkSmartPointer< vtkImageShiftScale >::New();
  ImageShiftScale->SetOutputScalarTypeToUnsignedChar();
  ImageShiftScale->SetInput( this->m_InputImageConverter->GetOutput() );
  ImageShiftScale->Update();

  vtkSmartPointer< vtkImageActor > ImageActor = vtkSmartPointer< vtkImageActor >::New();
  ImageActor->InterpolateOff();
  ImageActor->SetInput( ImageShiftScale->GetOutput() );

  this->m_Renderer->AddActor2D( ImageActor );
}

template < class TInputImage, class TInputImageConverter >
void
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::SetScreenCapture( const bool iCapture )
{
  this->m_ScreenCapture = iCapture;
}

template < class TInputImage, class TInputImageConverter >
bool
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::GetScreenCapture() const
{
  return this->m_ScreenCapture;
}

template < class TInputImage, class TInputImageConverter >
vtkRenderer *
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::GetRenderer()
{
  return this->m_Renderer;
}

template < class TInputImage, class TInputImageConverter >
vtkRenderWindow *
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::GetRenderWindow()
{
  return this->m_RenderWindow;
}

template < class TInputImage, class TInputImageConverter >
void
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::SetRenderWindow( vtkRenderWindow * renderWindow )
{
  this->m_RenderWindow = renderWindow;
  this->m_RenderWindow->AddRenderer( this->m_Renderer );
}

template < class TInputImage, class TInputImageConverter >
void
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::SetCurrentIteration( const IdentifierType iteration )
{
  this->m_CurrentIteration = iteration;
}

template < class TInputImage, class TInputImageConverter >
IdentifierType
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::GetCurrentIteration() const
{
  return this->m_CurrentIteration;
}

template < class TInputImage, class TInputImageConverter >
void
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::SetScreenCapturePrefix( const char * prefix)
{
  this->m_ScreenCapturePrefix = prefix;
}

template < class TInputImage, class TInputImageConverter >
void
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::Update()
{
  this->PrepareVTKPipeline();

  std::stringstream counter;
  counter << this->m_CurrentIteration;

  std::string counterString = counter.str();

  this->m_Annotation->SetText( 0, counterString.c_str() );

  this->m_RenderWindow->Render();

  if( this->m_ScreenCapture )
    {
    std::string filename;
    std::stringstream yo;
    yo << this->m_ScreenCapturePrefix << std::setfill( '0' ) << std::setw( 5 ) << this->m_CurrentIteration;
    filename = yo.str();
    filename.append ( ".png" );

    vtkCaptureScreen< vtkPNGWriter > capture ( this->m_RenderWindow );
    // begin mouse interaction
    capture( filename );
    }
}

} // end namespace itk

#endif
