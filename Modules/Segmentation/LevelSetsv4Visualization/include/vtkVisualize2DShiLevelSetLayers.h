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

#ifndef __vtkVisualize2DShiLevelSetLayers_h
#define __vtkVisualize2DShiLevelSetLayers_h

#include "itkLightObject.h"

#include "itkShiSparseLevelSetImage.h"

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

template< class TInputImage, unsigned int VDimension >
class vtkVisualize2DShiLevelSetLayers : public itk::LightObject
{
public:
  typedef vtkVisualize2DShiLevelSetLayers   Self;
  typedef LightObject                       Superclass;
  typedef itk::SmartPointer< Self >         Pointer;
  typedef itk::SmartPointer< const Self >   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(vtkVisualize2DShiLevelSetLayers, LightObject);

  typedef TInputImage                         InputImageType;
  typedef typename InputImageType::PixelType  InputPixelType;

  typedef itk::ShiSparseLevelSetImage< VDimension > LevelSetType;
  typedef typename LevelSetType::Pointer            LevelSetPointer;

  typedef itk::ImageToRGBVTKImageFilter< InputImageType >  ConverterType;
  typedef typename ConverterType::Pointer                  ConverterPointer;

  void SetInputImage( const InputImageType * iImage )
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

    //m_Count = 0;
    }

  void SetLevelSet( LevelSetType *f )
    {
    m_LevelSet = f;
    // m_Count = 0;
    }

  void SetScreenCapture( const bool& iCapture )
    {
    m_ScreenCapture = iCapture;
    }

  void SetPeriod( const itk::IdentifierType& iPeriod )
    {
    m_Period = iPeriod;
    }

  void Update()
    {
    if( m_Count % m_Period == 0 )
      {
      vtkSmartPointer< vtkImageData > VTKImage = m_ImageConverter->GetOutput();

      typedef typename LevelSetType::LayerType          LayerType;
      typedef typename LevelSetType::LayerConstIterator LayerConstIterator;

      LayerType layer = m_LevelSet->GetLayer( LevelSetType::MinusOneLayer() );

      LayerConstIterator it = layer.begin();

      while( it != layer.end() )
        {
        typename InputImageType::IndexType idx = it->first;
        InputPixelType* vtkpixel =
            static_cast< InputPixelType* >( VTKImage->GetScalarPointer( idx[0], idx[1], 0 ) );
        vtkpixel[0] = 0;
        vtkpixel[1] = 255;
        vtkpixel[2] = 0;
        ++it;
        }

      layer = m_LevelSet->GetLayer( LevelSetType::PlusOneLayer() );

      it = layer.begin();

      while( it != layer.end() )
        {
        typename InputImageType::IndexType idx = it->first;
        InputPixelType* vtkpixel =
            static_cast< InputPixelType* >( VTKImage->GetScalarPointer( idx[0], idx[1], 0 ) );
        vtkpixel[0] = 255;
        vtkpixel[1] = 0;
        vtkpixel[2] = 0;
        ++it;
        }

  //    vtkSmartPointer< vtkLookupTable > lut =
  //        vtkSmartPointer< vtkLookupTable >::New();
  //    lut->SetNumberOfTableValues( 2 );
  //    lut->SetRange( -1., 1. );
  //    lut->SetTableValue( 0, 1., 0., 0. );
  //    lut->SetTableValue( 1, 0., 0., 1. );
  //    lut->Build();


  //    vtkSmartPointer< vtkScalarBarActor > scalarbar =
  //        vtkSmartPointer< vtkScalarBarActor >::New();
  //    scalarbar->SetTitle( "Layers" );
  //    scalarbar->SetNumberOfLabels( 2 );
  //    scalarbar->SetLookupTable( lut );

      vtkSmartPointer< vtkImageActor > input_Actor =
          vtkSmartPointer< vtkImageActor >::New();
      input_Actor->SetInput( VTKImage );
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
        yo << "snapshot_shi_" << std::setfill( '0' ) << std::setw( 5 ) << m_Count;
        filename = yo.str();
        filename.append ( ".png" );

        vtkCaptureScreen< vtkPNGWriter > capture ( m_RenWin );
        // begin mouse interaction
  //      m_Iren->Start();
        capture( filename );
        }
      else
        {
        m_Iren->Start();
        }
      }
    ++m_Count;
    }

protected:
  vtkVisualize2DShiLevelSetLayers() : Superclass(),
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

  ~vtkVisualize2DShiLevelSetLayers()
    {}

private:
  vtkVisualize2DShiLevelSetLayers ( const Self& );
  void operator = ( const Self& );

  ConverterPointer  m_ImageConverter;
  LevelSetPointer   m_LevelSet;

  vtkSmartPointer< vtkCornerAnnotation >        m_Annotation;
  vtkSmartPointer< vtkRenderer >                m_Renderer;
  vtkSmartPointer< vtkRenderWindow >            m_RenWin;
  vtkSmartPointer< vtkRenderWindowInteractor >  m_Iren;

  itk::IdentifierType m_Count;
  itk::IdentifierType m_Period;
  bool                m_ScreenCapture;

};
#endif
