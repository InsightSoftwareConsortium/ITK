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

#include "itkVTKVisualizeImageLevelSetIsoValues.h"

#include "itkWhitakerSparseLevelSetImage.h"
#include "itkBinaryImageToLevelSetImageAdaptor.h"

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"


template< typename TImage >
void GenerateImage( typename TImage::Pointer ioImage )
{
  typename TImage::IndexType  index;
  index.Fill( 0 );

  typename TImage::SizeType   size;
  size.Fill( 50 );

  typename TImage::RegionType region;
  region.SetIndex( index );
  region.SetSize( size );

  typedef typename TImage::PixelType PixelType;

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

int itkVTKVisualize2DWhitakerLevelSetTest( int , char* [] )
{
  typedef unsigned char PixelType;
  const unsigned int Dimension = 2;
  typedef itk::Image< PixelType, Dimension > ImageType;

  ImageType::Pointer image = ImageType::New();
  GenerateImage< ImageType >( image );

  typedef double LevelSetOutputType;

  typedef itk::WhitakerSparseLevelSetImage< LevelSetOutputType, Dimension >
      LevelSetType;

  typedef itk::BinaryImageToLevelSetImageAdaptor< ImageType, LevelSetType >
      BinaryToSparseAdaptorType;

  BinaryToSparseAdaptorType::Pointer adaptor = BinaryToSparseAdaptorType::New();
  adaptor->SetInputImage( image );
  adaptor->Initialize();

  LevelSetType::Pointer LevelSet = adaptor->GetLevelSet();

  typedef itk::VTKVisualizeImageLevelSetIsoValues< ImageType, LevelSetType >
      VisualizationType;
  VisualizationType::Pointer viewer = VisualizationType::New();
  viewer->SetInputImage( image );
  viewer->SetLevelSet( LevelSet );
  viewer->SetLevelLimit( 3. );
  viewer->SetNumberOfLevels( 7 );
  viewer->SetScreenCapture( true );
  viewer->Update();

  return EXIT_SUCCESS;
}
