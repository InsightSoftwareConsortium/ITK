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

#ifndef __vtkVisualize2DLevelSetImageBase_h
#define __vtkVisualize2DLevelSetImageBase_h

#include "itkLightObject.h"

#include "itkLevelSetImageBase.h"
#include "itkImageToVTKImageFilter.h"
#include "itkLevelSetImageBaseTovtkImageData.h"

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

/** \class vtkVisualize2DLevelSetImageBase
 *
 *  \tparam TInputImage Input Image Type
 *  \tparam TLevelSetImage  Level Set type
 *
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< class TInputImage, class TLevelSetImage >
class vtkVisualize2DLevelSetImageBase : public itk::LightObject
{
public:
  typedef vtkVisualize2DLevelSetImageBase Self;
  typedef LightObject                     Superclass;
  typedef itk::SmartPointer< Self >       Pointer;
  typedef itk::SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(vtkVisualize2DLevelSetImageBase, LightObject);

  typedef TInputImage     InputImageType;
  typedef TLevelSetImage  LevelSetImageType;

  typedef itk::LevelSetDenseImageBase< LevelSetImageType > LevelSetType;

  typedef itk::ImageToVTKImageFilter< InputImageType >  ImageConverterType;
  typedef typename ImageConverterType::Pointer          ImageConverterPointer;

  typedef itk::LevelSetImageBaseTovtkImageData< LevelSetImageType > LevelSetConverterType;
  typedef typename LevelSetConverterType::Pointer                   LevelSetConverterPointer;


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
    }

  void SetLevelSet( LevelSetType *f )
    {
    m_LevelSetConverter->SetInput( f );
    }

  void SetScreenCapture( const bool& iCapture )
    {
    m_ScreenCapture = iCapture;
    }

  void SetNumberOfLevels( const unsigned int& iLevel )
    {
    if( iLevel > 0 )
      {
      m_NumberOfLevels = iLevel;
      }
    }

  void SetLevelLimit( double iLimit )
    {
    if( iLimit > 0. )
      {
      m_LevelLimit = iLimit;
      }
    }

  void SetPeriod( const itk::IdentifierType& iPeriod )
    {
    m_Period = iPeriod;
    }

  void Update()
    {
    try
      {
      m_LevelSetConverter->Update();
      }
    catch( itk::ExceptionObject& e )
      {
      std::cout << e << std::endl;
      return;
      }
    if( m_Count % m_Period == 0 )
      {

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
    shift->SetInput( m_ImageConverter->GetOutput() );
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

    m_Renderer->AddActor ( input_Actor );
    m_Renderer->AddActor ( ContourActor );
    m_Renderer->AddActor2D( scalarbar );

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
      yo << "snapshot_dense_" << std::setfill( '0' ) << std::setw( 5 ) << m_Count;
      filename = yo.str();
      filename.append ( ".png" );

      vtkCaptureScreen< vtkPNGWriter > capture ( m_RenWin );
      // begin mouse interaction
      capture( filename );
      }
    }
  ++m_Count;
  }

protected:
  vtkVisualize2DLevelSetImageBase() : Superclass(),
    m_Count( 0 ),
    m_Period( 20 ),
    m_NumberOfLevels( 1 ),
    m_LevelLimit( 0 ),
    m_ScreenCapture( false )
    {
    m_ImageConverter = ImageConverterType::New();
    m_LevelSetConverter = LevelSetConverterType::New();

    m_Renderer = vtkSmartPointer< vtkRenderer >::New();
    m_Renderer->SetBackground( 0.5, 0.5, 0.5 );

    m_Annotation = vtkSmartPointer< vtkCornerAnnotation >::New();
    m_Renderer->AddActor2D( m_Annotation );

    m_Iren = vtkSmartPointer< vtkRenderWindowInteractor >::New();

    m_RenWin = vtkSmartPointer< vtkRenderWindow >::New();

    m_RenWin->AddRenderer( m_Renderer );
    m_Iren->SetRenderWindow( m_RenWin );
    }

  ~vtkVisualize2DLevelSetImageBase()
    {}

private:
  vtkVisualize2DLevelSetImageBase ( const Self& );
  void operator = ( const Self& );

  ImageConverterPointer     m_ImageConverter;
  LevelSetConverterPointer  m_LevelSetConverter;

  vtkSmartPointer< vtkCornerAnnotation >        m_Annotation;
  vtkSmartPointer< vtkRenderer >                m_Renderer;
  vtkSmartPointer< vtkRenderWindow >            m_RenWin;
  vtkSmartPointer< vtkRenderWindowInteractor >  m_Iren;

  itk::IdentifierType m_Count;
  itk::IdentifierType m_Period;
  unsigned int        m_NumberOfLevels;
  double              m_LevelLimit;
  bool                m_ScreenCapture;

};
#endif
