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
#ifndef __itkVTKVisualizeDenseImageLevelSet_hxx
#define __itkVTKVisualizeDenseImageLevelSet_hxx

#include "itkVTKVisualizeDenseImageLevelSet.h"

namespace itk
{

template< class TInputPixel, class TLevelSetImage >
VTKVisualizeDenseImageLevelSet< TInputPixel, 2, TLevelSetImage >
::VTKVisualizeDenseImageLevelSet():
  m_NumberOfLevels( 1 ),
  m_LevelLimit( 0 )
{
  this->m_LevelSetConverter = LevelSetConverterType::New();

  this->m_Annotation = vtkSmartPointer< vtkCornerAnnotation >::New();
  this->m_Renderer->AddActor2D( this->m_Annotation );
}

template< class TInputPixel, class TLevelSetImage >
VTKVisualizeDenseImageLevelSet< TInputPixel, 2, TLevelSetImage >
::~VTKVisualizeDenseImageLevelSet()
{
}

template< class TInputPixel, class TLevelSetImage >
void
VTKVisualizeDenseImageLevelSet< TInputPixel, 2, TLevelSetImage >
::SetLevelSet( LevelSetType * levelSet )
{
  this->m_LevelSetConverter->SetInput( levelSet );
}

template< class TInputPixel, class TLevelSetImage >
void
VTKVisualizeDenseImageLevelSet< TInputPixel, 2, TLevelSetImage >
::SetNumberOfLevels( const SizeValueType numLevels )
{
  if( numLevels > 0 )
    {
    this->m_NumberOfLevels = numLevels;
    }
}

template< class TInputPixel, class TLevelSetImage >
SizeValueType
VTKVisualizeDenseImageLevelSet< TInputPixel, 2, TLevelSetImage >
::GetNumberOfLevels() const
{
  return this->m_NumberOfLevels;
}

template< class TInputPixel, class TLevelSetImage >
void
VTKVisualizeDenseImageLevelSet< TInputPixel, 2, TLevelSetImage >
::SetLevelLimit( double levelLimit )
{
  if( levelLimit > 0. )
    {
    this->m_LevelLimit = levelLimit;
    }
}

template< class TInputPixel, class TLevelSetImage >
double
VTKVisualizeDenseImageLevelSet< TInputPixel, 2, TLevelSetImage >
::GetLevelLimit() const
{
  return this->m_LevelLimit;
}

template< class TInputPixel, class TLevelSetImage >
void
VTKVisualizeDenseImageLevelSet< TInputPixel, 2, TLevelSetImage >
::PrepareVTKPipeline()
{
  m_LevelSetConverter->Update();

  vtkSmartPointer< vtkMarchingSquares > contours =
    vtkSmartPointer< vtkMarchingSquares >::New();
  contours->SetInput( m_LevelSetConverter->GetOutput() );
  contours->GenerateValues( m_NumberOfLevels, - m_LevelLimit, m_LevelLimit );
  contours->Update();

  vtkSmartPointer< vtkPolyDataMapper > mapper =
      vtkSmartPointer< vtkPolyDataMapper >::New();
  mapper->SetInputConnection( contours->GetOutputPort() );
  mapper->SetScalarRange( - m_LevelLimit, m_LevelLimit );

  vtkSmartPointer< vtkActor > ContourActor =
      vtkSmartPointer< vtkActor >::New();
  ContourActor->SetMapper( mapper );
  ContourActor->GetProperty()->SetLineWidth( 2. );
  ContourActor->GetProperty()->SetColor( 1, 0, 0 );
  //ContourActor->GetProperty()->SetOpacity( 1.0 );

  vtkSmartPointer< vtkImageShiftScale > shift =
      vtkSmartPointer< vtkImageShiftScale >::New();
  shift->SetInput( this->m_InputImageConverter->GetOutput() );
  shift->SetOutputScalarTypeToUnsignedChar();
  shift->Update();

  vtkSmartPointer< vtkImageActor > input_Actor =
      vtkSmartPointer< vtkImageActor >::New();
  input_Actor->SetInput( shift->GetOutput() );

  vtkSmartPointer< vtkScalarBarActor > scalarbar =
      vtkSmartPointer< vtkScalarBarActor >::New();
  scalarbar->SetLookupTable( mapper->GetLookupTable() );
  scalarbar->SetTitle( "Level Set Values" );
  scalarbar->SetNumberOfLabels( m_NumberOfLevels );

  this->m_Renderer->AddActor ( input_Actor );
  this->m_Renderer->AddActor ( ContourActor );
  this->m_Renderer->AddActor2D( scalarbar );

  std::stringstream counter;
  counter << this->GetCurrentIteration();

  m_Annotation->SetText( 0, counter.str().c_str() );

  this->m_Renderer->AddActor2D( input_Actor );
  //      m_Ren->AddActor2D( scalarbar );
}

} // end namespace itk

#endif
