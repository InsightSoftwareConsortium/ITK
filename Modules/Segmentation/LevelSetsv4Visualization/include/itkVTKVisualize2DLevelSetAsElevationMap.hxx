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
#ifndef itkVTKVisualize2DLevelSetAsElevationMap_hxx
#define itkVTKVisualize2DLevelSetAsElevationMap_hxx

#include "itkVTKVisualize2DLevelSetAsElevationMap.h"

#include "itkMath.h"

#include "vtkVersion.h"

#include "vtkScalarsToColors.h"
#include "vtkDoubleArray.h"
#include "vtkPointData.h"
#include "vtkCellArray.h"
#include "vtkProperty.h"
#include "vtkTextProperty.h"

namespace itk
{

template< typename TInputImage, typename TLevelSet >
VTKVisualize2DLevelSetAsElevationMap< TInputImage, TLevelSet >
::VTKVisualize2DLevelSetAsElevationMap()
{
  this->m_ColorValue = true;
  this->m_MinValue = itk::NumericTraits< double >::max( );
  this->m_MaxValue = itk::NumericTraits< double >::min( );
  this->m_HeightScaling = 0.1;

  this->m_Mesh = vtkSmartPointer< vtkPolyData >::New();

  this->m_ScalarBarActor = vtkSmartPointer< vtkScalarBarActor >::New();
  this->m_ScalarBarActor->SetTitle( "Level Set" );
  this->m_ScalarBarActor->GetPositionCoordinate( )->SetCoordinateSystemToNormalizedViewport( );
  this->m_ScalarBarActor->GetPositionCoordinate( )->SetValue( 0.1, 0.01 );
  this->m_ScalarBarActor->GetTitleTextProperty( )->SetColor( 0., 0., 0. );
  this->m_ScalarBarActor->GetLabelTextProperty( )->SetColor( 0., 0., 0. );
  this->m_ScalarBarActor->SetOrientationToHorizontal( );
  this->m_ScalarBarActor->SetWidth( 0.8 );
  this->m_ScalarBarActor->SetHeight( 0.17 );
  this->m_Renderer->AddActor2D( this->m_ScalarBarActor );

  this->m_MeshMapper = vtkSmartPointer< vtkPolyDataMapper >::New();
#if VTK_MAJOR_VERSION <= 5
  this->m_MeshMapper->SetInput( this->m_Mesh );
#else
  this->m_MeshMapper->SetInputData( this->m_Mesh );
#endif
  this->m_SurfaceActor = vtkSmartPointer< vtkActor >::New();
  this->m_SurfaceActor->SetMapper( this->m_MeshMapper );
  this->m_SurfaceActor->GetProperty( )->SetColor( 0.7, 0.7, 0.7 );
  this->m_Renderer->AddActor( this->m_SurfaceActor );
}

template< typename TInputImage, typename TLevelSet >
VTKVisualize2DLevelSetAsElevationMap< TInputImage, TLevelSet >
::~VTKVisualize2DLevelSetAsElevationMap()
{
}

template< typename TInputImage, typename TLevelSet >
void
VTKVisualize2DLevelSetAsElevationMap< TInputImage, TLevelSet >
::SetLevelSet( LevelSetType * levelSet )
{
  this->m_LevelSet = levelSet;
}

template< typename TInputImage, typename TLevelSet >
void
VTKVisualize2DLevelSetAsElevationMap< TInputImage, TLevelSet >
::PrepareVTKPipeline()
{
  this->GenerateElevationMap();

  if( !this->m_ColorValue )
    {
    this->m_MeshMapper->ScalarVisibilityOff( );
    }
  else
    {
    this->m_MeshMapper->ScalarVisibilityOn( );
    this->m_MeshMapper->SetScalarRange( this->m_MinValue, this->m_MaxValue );
    vtkScalarsToColors * lookupTable = this->m_MeshMapper->GetLookupTable();
    lookupTable->SetRange( this->m_MinValue, this->m_MaxValue );
    lookupTable->Build();

    this->m_ScalarBarActor->SetLookupTable( lookupTable );
    }
}

template< typename TInputImage, typename TLevelSet >
void
VTKVisualize2DLevelSetAsElevationMap< TInputImage, TLevelSet >
::GenerateElevationMap()
{
  typename InputImageType::ConstPointer inputImage = this->m_InputImageConverter->GetInput();
  typename InputImageType::RegionType       region = inputImage->GetLargestPossibleRegion();

  typedef typename InputImageType::IndexType      IndexType;
  typedef typename InputImageType::PointType      PointType;

  IndexType start = region.GetIndex();
  PointType itkPoint;
  PointType itkPoint2;

  InputImageSizeType   size =  region.GetSize();

  this->m_NumberOfSamples[0] = size[0] / 2;
  this->m_NumberOfSamples[1] = size[1] / 2;

  IndexType dx;
  dx[0] = static_cast< IndexValueType >( size[0] / this->m_NumberOfSamples[0] );
  dx[1] = static_cast< IndexValueType >( size[1] / this->m_NumberOfSamples[1] );

  vtkSmartPointer< vtkPoints >         vtkpoints = vtkSmartPointer< vtkPoints >::New( );
  vtkSmartPointer< vtkDoubleArray > vtkpointdata = vtkSmartPointer< vtkDoubleArray >::New( );
  vtkSmartPointer< vtkCellArray >          cells = vtkSmartPointer< vtkCellArray >::New( );

  this->m_Mesh->SetPoints( vtkpoints );
  this->m_Mesh->GetPointData( )->SetScalars( vtkpointdata );
  this->m_Mesh->SetPolys( cells );

  this->m_MinValue = itk::NumericTraits< double >::max( );
  this->m_MaxValue = itk::NumericTraits< double >::min( );

  InputImageSizeValueType k = 0;

  IndexType index;
  double p[3];

  for( InputImageSizeValueType i = 0; i < this->m_NumberOfSamples[0]; i++ )
    {
    index[0] = start[0] + i * dx[0];

    for( InputImageSizeValueType j = 0; j < this->m_NumberOfSamples[1]; j++ )
      {
      index[1] = start[1] + j * dx[1];

      inputImage->TransformIndexToPhysicalPoint( index, itkPoint );

      p[0] = itkPoint[0];
      p[1] = itkPoint[1];
      p[2] = static_cast< double >( this->m_LevelSet->Evaluate( index ) );

      vtkpointdata->InsertNextTuple1( p[2] );

      if( p[2] < m_MinValue )
        {
        m_MinValue = p[2];
        }
      if( p[2] > m_MaxValue )
        {
        m_MaxValue = p[2];
        }

      vtkpoints->InsertPoint( k++, p );
      }
    }

  double den = std::max( itk::Math::abs( m_MinValue ),
                             itk::Math::abs( m_MaxValue ) );

  inputImage->TransformIndexToPhysicalPoint( start, itkPoint );

  index = start;

  index[0] += size[0] - 1;
  index[1] += size[1] - 1;

  inputImage->TransformIndexToPhysicalPoint( index, itkPoint2 );

  double ratio = m_HeightScaling *
      static_cast< double >( itkPoint.EuclideanDistanceTo( itkPoint2 ) );

  if( den != 0. )
    {
    ratio /= den;
    }

  for( vtkIdType i = 0; i < vtkpoints->GetNumberOfPoints(); i++ )
    {
    vtkpoints->GetPoint( i, p );
    p[2] *= ratio;
    vtkpoints->SetPoint( i, p );
    }

  vtkIdType vtkId[3];


  for( InputImageSizeValueType i = 0; i < ( this->m_NumberOfSamples[0] -1 ); i++ )
    {
    for( InputImageSizeValueType j = 0; j < ( this->m_NumberOfSamples[1] - 1 ); j++ )
      {
      vtkId[0] = i * m_NumberOfSamples[1] + j;
      vtkId[1] = vtkId[0] + 1;
      vtkId[2] = vtkId[1] + m_NumberOfSamples[1];
      this->m_Mesh->InsertNextCell( VTK_TRIANGLE, 3, static_cast< vtkIdType* >( vtkId ) );

      vtkId[1] = vtkId[2];
      vtkId[2] = vtkId[1] - 1;
      this->m_Mesh->InsertNextCell( VTK_TRIANGLE, 3, static_cast< vtkIdType* >( vtkId ) );
      }
    }
}


} // end namespace itk

#endif
