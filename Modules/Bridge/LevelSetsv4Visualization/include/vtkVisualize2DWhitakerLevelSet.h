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

#ifndef __vtkVisualize2DWhitakerLevelSet_h
#define __vtkVisualize2DWhitakerLevelSet_h

#include "itkLightObject.h"

#include "itkWhitakerSparseLevelSetImage.h"

#include "itkImageToVTKImageFilter.h"
#include "itkWhitakerLevelSetTovtkImageData.h"

#include "vtkCornerAnnotation.h"
#include "vtkImageData.h"
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

template< class TInputImage, typename TOutput, unsigned int VDimension >
class vtkVisualize2DWhitakerLevelSet : public itk::LightObject
{
public:
  typedef vtkVisualize2DWhitakerLevelSet  Self;
  typedef LightObject                     Superclass;
  typedef itk::SmartPointer< Self >       Pointer;
  typedef itk::SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(vtkVisualize2DWhitakerLevelSet, LightObject);

  typedef TInputImage     InputImageType;

  typedef itk::WhitakerSparseLevelSetImage< TOutput, VDimension > LevelSetType;
  typedef typename LevelSetType::Pointer                          LevelSetPointer;

  typedef itk::ImageToVTKImageFilter< InputImageType >  ImageConverterType;
  typedef typename ImageConverterType::Pointer          ImageConverterPointer;

  typedef itk::WhitakerLevelSetTovtkImageData< TOutput, VDimension >
    LevelSetConverterType;
  typedef typename LevelSetConverterType::Pointer
    LevelSetConverterPointer;


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

//    m_Count = 0;
    }

  void SetLevelSet( LevelSetType *f )
    {
    m_LevelSet = f;
//    m_Count = 0;
    }

  void SetScreenCapture( const bool& iCapture )
    {
    m_ScreenCapture = iCapture;
    }

  void SetNumberOfLevels( const unsigned int& iLevel )
    {
    if( iLevel != 0 )
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

  void Update()
    {
    if( m_LevelSet.IsNull() )
      {
      itkGenericExceptionMacro( << "m_LevelSet is NULL" );
      }

    LevelSetConverterPointer levelSetConverter = LevelSetConverterType::New();
    levelSetConverter->SetInput( m_LevelSet );
    try
      {
      levelSetConverter->Update();
      }
    catch( itk::ExceptionObject& e )
      {
      std::cout << e << std::endl;
      return;
      }

    vtkSmartPointer< vtkMarchingSquares > contours =
      vtkSmartPointer< vtkMarchingSquares >::New();
    contours->SetInput( levelSetConverter->GetOutput() );
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

    m_RenWin->Render();

    if( m_ScreenCapture )
      {
      std::string filename;
      std::stringstream yo;
      yo << "snapshot_" << m_Count;
      filename = yo.str();
      filename.append ( ".png" );

      vtkCaptureScreen< vtkPNGWriter > capture ( m_RenWin );
      // begin mouse interaction
//      m_Iren->Start();
      capture( filename );
      ++m_Count;
      }
    else
      {
      m_Iren->Start();
      }
    }

protected:
  vtkVisualize2DWhitakerLevelSet() : Superclass(),
    m_Count( 0 ),
    m_Period( 20 ),
    m_LevelLimit( 3. ),
    m_NumberOfLevels( 7 ),
    m_ScreenCapture( false )
    {
    m_ImageConverter = ImageConverterType::New();
    m_Renderer = vtkSmartPointer< vtkRenderer >::New();
    m_Renderer->SetBackground( 0.5, 0.5, 0.5 );

    m_Annotation = vtkSmartPointer< vtkCornerAnnotation >::New();
    m_Renderer->AddActor2D( m_Annotation );

    m_Iren = vtkSmartPointer< vtkRenderWindowInteractor >::New();

    m_RenWin = vtkSmartPointer< vtkRenderWindow >::New();

    m_RenWin->AddRenderer( m_Renderer );
    m_Iren->SetRenderWindow( m_RenWin );
    }

  ~vtkVisualize2DWhitakerLevelSet()
    {}

private:
  vtkVisualize2DWhitakerLevelSet ( const Self& );
  void operator = ( const Self& );

  ImageConverterPointer     m_ImageConverter;
  LevelSetPointer           m_LevelSet;

  vtkSmartPointer< vtkCornerAnnotation >        m_Annotation;
  vtkSmartPointer< vtkRenderer >                m_Renderer;
  vtkSmartPointer< vtkRenderWindow >            m_RenWin;
  vtkSmartPointer< vtkRenderWindowInteractor >  m_Iren;

  itk::IdentifierType m_Count;
  itk::IdentifierType m_Period;
  double              m_LevelLimit;
  unsigned int        m_NumberOfLevels;
  bool                m_ScreenCapture;

};
#endif
