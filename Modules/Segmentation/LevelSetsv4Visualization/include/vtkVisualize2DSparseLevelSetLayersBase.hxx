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

#ifndef __vtkVisualize2DSparseLevelSetLayersBase_hxx
#define __vtkVisualize2DSparseLevelSetLayersBase_hxx

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

template< class TInputImage, class TLevelSet >
vtkVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::vtkVisualize2DSparseLevelSetLayersBase(): Superclass(),
  m_Count( 0 ),
  m_Period( 20 ),
  m_ScreenCapture( false )
  {
  m_ImageConverter = ConverterType::New();
  m_Renderer = vtkSmartPointer< vtkRenderer >::New();
  m_Renderer->SetBackground( 0.5, 0.5, 0.5 );

  m_Annotation = vtkSmartPointer< vtkCornerAnnotation >::New();
  m_Renderer->AddActor2D( m_Annotation );

  m_Iren = vtkSmartPointer< vtkRenderWindowInteractor >::New();

  m_RenWin = vtkSmartPointer< vtkRenderWindow >::New();

  m_RenWin->AddRenderer( m_Renderer );
  m_Iren->SetRenderWindow( m_RenWin );
  }

template< class TInputImage, class TLevelSet >
vtkVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::~vtkVisualize2DSparseLevelSetLayersBase()
{
}

template< class TInputImage, class TLevelSet >
void
vtkVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::SetInputImage( const InputImageType * iImage )
  {
  m_ImageConverter->SetInput( iImage );
  try
    {
    m_ImageConverter->Update();
    }
  catch( itk::ExceptionObject& e )
    {
    std::cout << e << std::endl;
    return;
    }
  }

template< class TInputImage, class TLevelSet >
void
vtkVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::SetLevelSet( LevelSetType *f )
  {
  m_LevelSet = f;
  }

template< class TInputImage, class TLevelSet >
void
vtkVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::SetScreenCapture( const bool& iCapture )
  {
  m_ScreenCapture = iCapture;
  }

template< class TInputImage, class TLevelSet >
void
vtkVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::SetPeriod( const itk::IdentifierType& iPeriod )
  {
  m_Period = iPeriod;
  }

template< class TInputImage, class TLevelSet >
void
vtkVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::Update()
  {
  if( m_Count % m_Period == 0 )
    {
    m_VTKImage = m_ImageConverter->GetOutput();

    this->AddLayers();

    //      vtkSmartPointer< vtkLookupTable > lut =
    //          vtkSmartPointer< vtkLookupTable >::New();
    //      lut->SetNumberOfTableValues( 5 );
    //      lut->SetRange( -2., 2. );
    //      lut->SetTableValue( 0, 0., 1., 0. );
    //      lut->SetTableValue( 1, 1., 1., 0. );
    //      lut->SetTableValue( 2, 1., 0., 0. );
    //      lut->SetTableValue( 3, 1., 0., 1. );
    //      lut->SetTableValue( 4, 0., 0., 1. );
    //      lut->Build();


    //      vtkSmartPointer< vtkScalarBarActor > scalarbar =
    //          vtkSmartPointer< vtkScalarBarActor >::New();
    //      scalarbar->SetTitle( "Layers" );
    //      scalarbar->SetNumberOfLabels( 5 );
    //      scalarbar->SetLookupTable( lut );

    vtkSmartPointer< vtkImageActor > input_Actor =
        vtkSmartPointer< vtkImageActor >::New();
    input_Actor->SetInput( this->m_VTKImage );
    input_Actor->InterpolateOff();

    std::stringstream counter;
    counter << m_Count;

    m_Annotation->SetText( 0, counter.str().c_str() );

    m_Renderer->AddActor2D( input_Actor );
//      m_Ren->AddActor2D( scalarbar );

    m_RenWin->Render();

    if( m_ScreenCapture )
      {
      std::string filename;
      std::stringstream yo;
      yo << "snapshot_" << this->GetLevelSetRepresentationName()
         <<"_" << std::setfill( '0' ) << std::setw( 5 ) << m_Count;
      filename = yo.str();
      filename.append ( ".png" );

      vtkCaptureScreen< vtkPNGWriter > capture ( m_RenWin );
      // begin mouse interaction
  //    m_Iren->Start();
      capture( filename );
      }
    else
      {
      m_Iren->Start();
      }
    }
  ++m_Count;
  }

#endif
