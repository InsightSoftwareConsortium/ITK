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
#ifndef itkVTKVisualizeImageLevelSetIsoValues_hxx
#define itkVTKVisualizeImageLevelSetIsoValues_hxx

#include "vtkVersion.h"

#include "itkVTKVisualizeImageLevelSetIsoValues.h"

namespace itk
{
template< typename TInputPixel, typename TLevelSet >
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 2 >, TLevelSet >
::VTKVisualizeImageLevelSetIsoValues():
  m_NumberOfLevels( 1 ),
  m_LevelLimit( 0. )
{
  this->m_LevelSetConverter = LevelSetConverterType::New();

  this->m_MarchingSquare = vtkSmartPointer< vtkMarchingSquares >::New();

  this->m_ContourMapper = vtkSmartPointer< vtkPolyDataMapper >::New();

  this->m_Lut = vtkSmartPointer< vtkLookupTable >::New();
  this->m_Lut->SetTableRange( 0, 1 );
  this->m_Lut->SetHueRange( 0, 1 );
  this->m_Lut->SetSaturationRange( 1, 1 );
  this->m_Lut->SetValueRange( 1, 1 );
  this->m_Lut->Build();

  this->m_ScalarBar = vtkSmartPointer< vtkScalarBarActor >::New();
  this->m_ScalarBar->SetLookupTable( this->m_Lut );
  this->m_ScalarBar->SetTitle( "Level Set Values" );
  this->m_ScalarBar->SetNumberOfLabels( this->m_NumberOfLevels );
  this->m_ContourMapper->SetScalarRange( - m_LevelLimit, m_LevelLimit );

  this->m_Renderer->AddActor2D( this->m_ScalarBar );

  this->m_ContourActor = vtkSmartPointer< vtkActor >::New();
  this->m_ContourActor->SetMapper( this->m_ContourMapper );
  this->m_ContourActor->GetProperty()->SetLineWidth( 2. );
  this->m_ContourActor->GetProperty()->SetColor( 1., 0., 0. );
}

template< typename TInputPixel, typename TLevelSet >
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 2 >, TLevelSet >
::~VTKVisualizeImageLevelSetIsoValues()
{
}

template< typename TInputPixel, typename TLevelSet >
void
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 2 >, TLevelSet >
::SetLevelSet( LevelSetType* iLevelSet )
{
  this->m_LevelSetConverter->SetInput( iLevelSet );
}

template< typename TInputPixel, typename TLevelSet >
void
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 2 >, TLevelSet >
::SetNumberOfLevels( const SizeValueType numLevels )
{
  if( numLevels > 0 )
    {
    this->m_NumberOfLevels = numLevels;
    this->m_ScalarBar->SetNumberOfLabels( m_NumberOfLevels );
    }
}

template< typename TInputPixel, typename TLevelSet >
SizeValueType
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 2 >, TLevelSet >
::GetNumberOfLevels() const
{
  return this->m_NumberOfLevels;
}

template< typename TInputPixel, typename TLevelSet >
void
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 2 >, TLevelSet >
::SetLevelLimit( double levelLimit )
{
  if( levelLimit > 0. )
    {
    this->m_LevelLimit = levelLimit;
    this->m_ContourMapper->SetScalarRange( - m_LevelLimit, m_LevelLimit );
    }
}

template< typename TInputPixel, typename TLevelSet >
double
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 2 >, TLevelSet >
::GetLevelLimit() const
{
  return this->m_LevelLimit;
}

template< typename TInputPixel, typename TLevelSet >
void
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 2 >, TLevelSet >
::PrepareVTKPipeline()
{
  this->m_LevelSetConverter->Update();

#if VTK_MAJOR_VERSION <= 5
  this->m_MarchingSquare->SetInput( this->m_LevelSetConverter->GetOutput() );
#else
  this->m_LevelSetConverter->Update();
  this->m_MarchingSquare->SetInputData( this->m_LevelSetConverter->GetOutput() );
#endif
  this->m_MarchingSquare->GenerateValues( m_NumberOfLevels, - m_LevelLimit, m_LevelLimit );
  this->m_MarchingSquare->Modified();
  this->m_MarchingSquare->Update();

  this->m_ContourMapper->SetInputConnection( this->m_MarchingSquare->GetOutputPort() );
  this->m_ContourMapper->Modified();

  this->m_Renderer->AddActor( this->m_ContourActor );

  vtkScalarsToColors * lookupTable = this->m_ContourMapper->GetLookupTable();
  lookupTable->SetRange( - m_LevelLimit, m_LevelLimit );
  lookupTable->Build();

  this->m_ScalarBar->SetLookupTable( lookupTable );
  this->m_ScalarBar->Modified();
}

// --------------------- 3D ---------------------------------------------------
template< typename TInputPixel, typename TLevelSet >
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 3 >, TLevelSet >
::VTKVisualizeImageLevelSetIsoValues():
  m_NumberOfLevels( 1 ),
  m_LevelLimit( 0. )
{
  this->m_LevelSetConverter = LevelSetConverterType::New();

  this->m_MarchingCubes = vtkSmartPointer< vtkMarchingCubes >::New();

  this->m_ContourMapper = vtkSmartPointer< vtkPolyDataMapper >::New();

  this->m_Lut = vtkSmartPointer< vtkLookupTable >::New();
  this->m_Lut->SetTableRange( 0, 1 );
  this->m_Lut->SetHueRange( 0, 1 );
  this->m_Lut->SetSaturationRange( 1, 1 );
  this->m_Lut->SetValueRange( 1, 1 );
  this->m_Lut->Build();

  this->m_ScalarBar = vtkSmartPointer< vtkScalarBarActor >::New();
  this->m_ScalarBar->SetLookupTable( this->m_Lut );
  this->m_ScalarBar->SetTitle( "Level Set Values" );
  this->m_ScalarBar->SetNumberOfLabels( this->m_NumberOfLevels );
  this->m_ContourMapper->SetScalarRange( - m_LevelLimit, m_LevelLimit );

  this->m_Renderer->AddActor2D( this->m_ScalarBar );

  this->m_ContourActor = vtkSmartPointer< vtkActor >::New();
  this->m_ContourActor->SetMapper( this->m_ContourMapper );
  this->m_ContourActor->GetProperty()->SetLineWidth( 2. );
  this->m_ContourActor->GetProperty()->SetColor( 1., 0., 0. );
}

template< typename TInputPixel, typename TLevelSet >
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 3 >, TLevelSet >
::~VTKVisualizeImageLevelSetIsoValues()
{
}

template< typename TInputPixel, typename TLevelSet >
void
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 3 >, TLevelSet >
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

  vtkSmartPointer< vtkImageActor > zImageActor = vtkSmartPointer< vtkImageActor >::New();
  zImageActor->InterpolateOff();
  zImageActor->GetMapper()->SetInputConnection( ImageShiftScale->GetOutputPort() );
  zImageActor->SetDisplayExtent( 0, dimensions[0], 0, dimensions[1], dimensions[2] / 2, dimensions[2] / 2 );

  this->m_Renderer->AddActor( zImageActor );

  vtkSmartPointer< vtkImageActor > yImageActor = vtkSmartPointer< vtkImageActor >::New();
  yImageActor->InterpolateOff();
  yImageActor->GetMapper()->SetInputConnection( ImageShiftScale->GetOutputPort() );
  yImageActor->SetDisplayExtent( 0, dimensions[0], dimensions[1] / 2, dimensions[1] / 2, 0, dimensions[2] );

  this->m_Renderer->AddActor( yImageActor );

  vtkSmartPointer< vtkImageActor > xImageActor = vtkSmartPointer< vtkImageActor >::New();
  xImageActor->InterpolateOff();
  xImageActor->GetMapper()->SetInputConnection( ImageShiftScale->GetOutputPort() );
  xImageActor->SetDisplayExtent( dimensions[0] / 2, dimensions[0] / 2, 0, dimensions[1], 0, dimensions[2] );

  this->m_Renderer->AddActor( xImageActor );
}

template< typename TInputPixel, typename TLevelSet >
void
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 3 >, TLevelSet >
::SetLevelSet( LevelSetType* iLevelSet )
{
  this->m_LevelSetConverter->SetInput( iLevelSet );
}

template< typename TInputPixel, typename TLevelSet >
void
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 3 >, TLevelSet >
::SetNumberOfLevels( const SizeValueType numLevels )
{
  if( numLevels > 0 )
    {
    this->m_NumberOfLevels = numLevels;
    this->m_ScalarBar->SetNumberOfLabels( m_NumberOfLevels );
    }
}

template< typename TInputPixel, typename TLevelSet >
SizeValueType
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 3 >, TLevelSet >
::GetNumberOfLevels() const
{
  return this->m_NumberOfLevels;
}

template< typename TInputPixel, typename TLevelSet >
void
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 3 >, TLevelSet >
::SetLevelLimit( double levelLimit )
{
  if( levelLimit > 0. )
    {
    this->m_LevelLimit = levelLimit;
    this->m_ContourMapper->SetScalarRange( - m_LevelLimit, m_LevelLimit );
    }
}

template< typename TInputPixel, typename TLevelSet >
double
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 3 >, TLevelSet >
::GetLevelLimit() const
{
  return this->m_LevelLimit;
}

template< typename TInputPixel, typename TLevelSet >
void
VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 3 >, TLevelSet >
::PrepareVTKPipeline()
{
  this->m_LevelSetConverter->Update();

#if VTK_MAJOR_VERSION <= 5
  this->m_MarchingCubes->SetInput( this->m_LevelSetConverter->GetOutput() );
#else
  this->m_LevelSetConverter->Update();
  this->m_MarchingCubes->SetInputData( this->m_LevelSetConverter->GetOutput() );
#endif
  this->m_MarchingCubes->GenerateValues( m_NumberOfLevels, - m_LevelLimit, m_LevelLimit );
  this->m_MarchingCubes->ComputeNormalsOff();
  this->m_MarchingCubes->Modified();
  this->m_MarchingCubes->Update();

  this->m_ContourMapper->SetInputConnection( this->m_MarchingCubes->GetOutputPort() );
  this->m_ContourMapper->Modified();

  this->m_Renderer->AddActor( this->m_ContourActor );

  vtkScalarsToColors * lookupTable = this->m_ContourMapper->GetLookupTable();
  lookupTable->SetRange( - m_LevelLimit, m_LevelLimit );
  lookupTable->Build();

  this->m_ScalarBar->SetLookupTable( lookupTable );
  this->m_ScalarBar->Modified();
}
} // end namespace itk

#endif
