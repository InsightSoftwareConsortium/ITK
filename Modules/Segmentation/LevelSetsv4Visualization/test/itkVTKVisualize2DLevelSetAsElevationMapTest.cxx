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

#include "itkLevelSetImage.h"
#include "itkLevelSetDenseImage.h"

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkVTKVisualize2DLevelSetAsElevationMap.h"

#include "vtkPolyData.h"
#include "vtkMassProperties.h"
#include "vtkSmartPointer.h"

template< typename TImage >
void GenerateImage( typename TImage::Pointer ioImage )
{
  typename TImage::IndexType  index;
  index.Fill( 0 );

  typename TImage::SizeType   size;
  size[0]=50;
  size[1]=49;

  typename TImage::RegionType region;
  region.SetIndex( index );
  region.SetSize( size );

  using PixelType = typename TImage::PixelType;

  ioImage->SetRegions( region );
  ioImage->Allocate();
  ioImage->FillBuffer( itk::NumericTraits< PixelType >::ZeroValue() );

  index.Fill( 10 );
  region.SetIndex( index );

  size.Fill( 30 );
  region.SetSize( size );

  typename itk::ImageRegionIterator<  TImage > it( ioImage, region );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    it.Set( itk::NumericTraits< PixelType >::max() );
    ++it;
    }

}

int itkVTKVisualize2DLevelSetAsElevationMapTest( int , char* [] )
{
  using PixelType = unsigned char;
  constexpr unsigned int Dimension = 2;
  using ImageType = itk::Image< PixelType, Dimension >;

  ImageType::Pointer image = ImageType::New();
  GenerateImage< ImageType >( image );

  using LevelSetOutputType = double;

  using LevelSetImageType = itk::Image< LevelSetOutputType, Dimension >;

  using LevelSetType = itk::LevelSetDenseImage< LevelSetImageType >;

  LevelSetImageType::Pointer LevelSetImage = LevelSetImageType::New();
  GenerateImage< LevelSetImageType >( LevelSetImage );

  using IteratorType = itk::ImageRegionIteratorWithIndex< LevelSetImageType >;
  IteratorType it( LevelSetImage, LevelSetImage->GetLargestPossibleRegion() );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    LevelSetImageType::IndexType idx = it.GetIndex();
    auto value = static_cast< LevelSetOutputType >( ( idx[0] - 25 ) * ( idx[0] - 25 ) +
                                           ( idx[1] - 25 ) * ( idx[1] - 25 ) );
    value = std::sqrt( value ) - 20;
    it.Set( value );
    ++it;
    }

  LevelSetType::Pointer levelset = LevelSetType::New();
  levelset->SetImage( LevelSetImage );

  using VisualizationType = itk::VTKVisualize2DLevelSetAsElevationMap< ImageType, LevelSetType >;
  VisualizationType::Pointer viewer = VisualizationType::New();
  viewer->SetInputImage( image );
  viewer->SetLevelSet( levelset );
  viewer->SetScreenCapture( true );
  viewer->SetHeightScaling( 0.1 );
  viewer->Update();

  vtkPolyData* levelsetSurface = viewer->GetElevationMapMesh();
  vtkCellArray*          cells = levelsetSurface->GetPolys();

  vtkSmartPointer<vtkMassProperties> massProperty = vtkSmartPointer<vtkMassProperties>::New();
#if VTK_MAJOR_VERSION <= 5
  massProperty->SetInput( levelsetSurface );
#else
  massProperty->SetInputData( levelsetSurface );
#endif
  double averageSurfaceAreaPerCell = massProperty->GetSurfaceArea()/cells->GetNumberOfCells();

  if ( averageSurfaceAreaPerCell > 2.5 )
    {
    std::cerr << "Average surface area per cell too high." << std::endl;
    return EXIT_FAILURE;
    }

  if ( viewer->GetHeightScaling() != 0.1)
    {
    std::cerr << "HeightScaling not set correctly." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
