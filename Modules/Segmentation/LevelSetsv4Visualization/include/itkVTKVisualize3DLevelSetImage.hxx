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

#ifndef __itkVTKVisualize3DLevelSetImage_hxx
#define __itkVTKVisualize3DLevelSetImage_hxx

#include "itkVTKVisualize3DLevelSetImage.h"

#include "vtkMarchingCubes.h"
#include "vtkProperty.h"

namespace itk
{
template< class TInputImage, class TLevelSet >
VTKVisualize3DLevelSetImage< TInputImage, TLevelSet >
::VTKVisualize3DLevelSetImage()
{
  this->m_LevelSetConverter = LevelSetConverterType::New();

  this->m_MeshMapper = vtkSmartPointer< vtkPolyDataMapper >::New();
  this->m_MeshMapper->ScalarVisibilityOff();

  this->m_MeshActor = vtkSmartPointer< vtkActor >::New();
  this->m_MeshActor->SetMapper( this->m_MeshMapper );
  this->m_MeshActor->GetProperty( )->SetLineWidth( 2. );
  this->m_MeshActor->GetProperty()->SetColor( 0, 0, 1 );
  this->m_MeshActor->GetProperty()->SetOpacity( 1.0 );

  this->m_Renderer->AddActor( this->m_MeshActor );
}

template< class TInputImage, class TLevelSet >
VTKVisualize3DLevelSetImage< TInputImage, TLevelSet >
::~VTKVisualize3DLevelSetImage()
{}

template< class TInputImage, class TLevelSet >
void
VTKVisualize3DLevelSetImage< TInputImage, TLevelSet >
::SetInputImage( const InputImageType * iImage )
{
  this->m_InputImageConverter->SetInput( iImage );
  this->m_InputImageConverter->Update();

  vtkSmartPointer< vtkImageShiftScale > ImageShiftScale = vtkSmartPointer< vtkImageShiftScale >::New();
  ImageShiftScale->SetOutputScalarTypeToUnsignedChar();
#if VTK_MAJOR_VERSION <= 5
  ImageShiftScale->SetInput( this->m_InputImageConverter->GetOutput() );
#else
  this->m_InputImageConverter->Update();
  ImageShiftScale->SetInputData( this->m_InputImageConverter->GetOutput() );
#endif
  ImageShiftScale->Update();

  int dimensions[3];
  this->m_InputImageConverter->GetOutput()->GetDimensions( dimensions );

  vtkSmartPointer< vtkImageActor > imageActor = vtkSmartPointer< vtkImageActor >::New();
  imageActor->InterpolateOff();
  imageActor->GetMapper()->SetInputConnection( ImageShiftScale->GetOutputPort() );
  imageActor->SetDisplayExtent( 0, dimensions[0], 0, dimensions[1], dimensions[2] / 2, dimensions[2] / 2 );

  this->m_Renderer->AddActor( imageActor );
}

template< class TInputImage, class TLevelSet >
void
VTKVisualize3DLevelSetImage< TInputImage, TLevelSet >
::SetLevelSet( LevelSetType *f )
{
  m_LevelSetConverter->SetInput( f );
}

template< class TInputImage, class TLevelSet >
void
VTKVisualize3DLevelSetImage< TInputImage, TLevelSet >
::PrepareVTKPipeline()
{
  m_LevelSetConverter->Update();

  const int levelSetId = 0;

  vtkSmartPointer< vtkMarchingCubes > contours = vtkSmartPointer< vtkMarchingCubes >::New();
#if VTK_MAJOR_VERSION <= 5
  contours->SetInput( m_LevelSetConverter->GetOutput() );
#else
  contours->SetInputData( m_LevelSetConverter->GetOutput() );
#endif
  contours->SetValue( levelSetId, 0. );
  contours->ComputeNormalsOff();

  this->m_MeshMapper->SetInputConnection( contours->GetOutputPort() );
}
}
#endif
