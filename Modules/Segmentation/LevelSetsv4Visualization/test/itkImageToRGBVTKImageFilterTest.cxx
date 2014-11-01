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

#include "vtkVersion.h"

#include "vtkSmartPointer.h"
#include "vtkImageData.h"
#include "vtkImageActor.h"
#include "vtkImageMapper3D.h"
#include "vtkRenderer.h"
#include "vtkRenderWindow.h"
#include "vtkRenderWindowInteractor.h"

#include "itkImage.h"
#include "itkImageToRGBVTKImageFilter.h"
#include "itkImageRegionIterator.h"

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

int itkImageToRGBVTKImageFilterTest( int argc, char* argv[] )
{
  (void) argc;
  (void) argv;

  typedef unsigned char PixelType;
  const unsigned int Dimension = 2;
  typedef itk::Image< PixelType, Dimension > ImageType;

  ImageType::Pointer image = ImageType::New();
  GenerateImage< ImageType >( image );

  typedef itk::ImageToRGBVTKImageFilter< ImageType > RGBConvertType;
  RGBConvertType::Pointer converter = RGBConvertType::New();
  converter->SetInput( image );
  converter->Update();

  vtkSmartPointer< vtkImageData > VTKImage = converter->GetOutput();

  for( int i = 0; i < 50; i++ )
    {
    PixelType* vtkpixel =
        static_cast< PixelType* >( VTKImage->GetScalarPointer( i, i, 0 ) );
    vtkpixel[0] = 255;
    vtkpixel[1] = 0;
    vtkpixel[2] = 0;

    vtkpixel =
        static_cast< PixelType* >( VTKImage->GetScalarPointer( i, 49-i, 0 ) );
    vtkpixel[0] = 0;
    vtkpixel[1] = 0;
    vtkpixel[2] = 255;
    }

  vtkSmartPointer< vtkImageActor > input_Actor =
      vtkSmartPointer< vtkImageActor >::New();
#if VTK_MAJOR_VERSION <= 5
  input_Actor->SetInput( VTKImage );
#else
  input_Actor->GetMapper()->SetInputData( VTKImage );
#endif
  vtkSmartPointer< vtkRenderer > ren =
      vtkSmartPointer< vtkRenderer >::New();
  ren->SetBackground( 0.5, 0.5, 0.5 );

  vtkSmartPointer< vtkRenderWindowInteractor > iren =
      vtkSmartPointer< vtkRenderWindowInteractor >::New();

  vtkSmartPointer< vtkRenderWindow > renWin =
      vtkSmartPointer< vtkRenderWindow >::New();

  ren->AddActor ( input_Actor );

  iren->SetRenderWindow( renWin );

  renWin->AddRenderer( ren );
  renWin->Render();

  return EXIT_SUCCESS;
}
