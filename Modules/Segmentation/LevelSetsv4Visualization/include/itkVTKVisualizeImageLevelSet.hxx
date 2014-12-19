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
#ifndef itkVTKVisualizeImageLevelSet_hxx
#define itkVTKVisualizeImageLevelSet_hxx

#include "itkVTKVisualizeImageLevelSet.h"

#include "vtkVersion.h"

#include "vtkCaptureScreen.h"
#include "vtkPNGWriter.h"
#include "vtkImageMapper3D.h"

namespace itk
{

template < typename TInputImage, typename TInputImageConverter >
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

template < typename TInputImage, typename TInputImageConverter >
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::~VTKVisualizeImageLevelSet()
{
}

template < typename TInputImage, typename TInputImageConverter >
void
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::SetInputImage( const InputImageType * inputImage )
{
  this->m_InputImageConverter->SetInput( inputImage );
  this->m_InputImageConverter->Update();

  vtkSmartPointer< vtkImageShiftScale >   ImageShiftScale = vtkSmartPointer< vtkImageShiftScale >::New();
  ImageShiftScale->SetOutputScalarTypeToUnsignedChar();
#if VTK_MAJOR_VERSION <= 5
  ImageShiftScale->SetInput( this->m_InputImageConverter->GetOutput() );
#else
  this->m_InputImageConverter->Update();
  ImageShiftScale->SetInputData( this->m_InputImageConverter->GetOutput() );
#endif
  ImageShiftScale->Update();

  vtkSmartPointer< vtkImageActor > ImageActor = vtkSmartPointer< vtkImageActor >::New();
  ImageActor->InterpolateOff();
  ImageActor->GetMapper()->SetInputConnection( ImageShiftScale->GetOutputPort() );

  this->m_Renderer->AddActor2D( ImageActor );
}

template < typename TInputImage, typename TInputImageConverter >
void
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::SetScreenCapture( const bool iCapture )
{
  this->m_ScreenCapture = iCapture;
}

template < typename TInputImage, typename TInputImageConverter >
bool
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::GetScreenCapture() const
{
  return this->m_ScreenCapture;
}

template < typename TInputImage, typename TInputImageConverter >
vtkRenderer *
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::GetRenderer()
{
  return this->m_Renderer;
}

template < typename TInputImage, typename TInputImageConverter >
vtkRenderWindow *
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::GetRenderWindow()
{
  return this->m_RenderWindow;
}

template < typename TInputImage, typename TInputImageConverter >
void
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::SetRenderWindow( vtkRenderWindow * renderWindow )
{
  this->m_RenderWindow = renderWindow;
  this->m_RenderWindow->AddRenderer( this->m_Renderer );
}

template < typename TInputImage, typename TInputImageConverter >
void
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::SetCurrentIteration( const IdentifierType iteration )
{
  this->m_CurrentIteration = iteration;
}

template < typename TInputImage, typename TInputImageConverter >
IdentifierType
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::GetCurrentIteration() const
{
  return this->m_CurrentIteration;
}

template < typename TInputImage, typename TInputImageConverter >
void
VTKVisualizeImageLevelSet< TInputImage, TInputImageConverter >
::SetScreenCapturePrefix( const char * prefix)
{
  this->m_ScreenCapturePrefix = prefix;
}

template < typename TInputImage, typename TInputImageConverter >
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
