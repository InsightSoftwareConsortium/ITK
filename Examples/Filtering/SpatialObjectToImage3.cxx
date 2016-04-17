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

//  Software Guide : BeginLatex
//
//  This example illustrates the use of the
//  \doxygen{PolygonSpatialObject} for generating a binary image through the
//  \doxygen{SpatialObjectToImageFilter}.
//
//  \index{itk::PolygonSpatialObject|textbf}
//  \index{itk::SpatialObjectToImageFilter|textbf}
//
//  We start by including the header of the PolygonSpatialObject that we will
//  use as elementary shape, and the header for the SpatialObjectToImageFilter
//  that we will use to rasterize the SpatialObject.
//
//  \index{itk::PolygonSpatialObject!header}
//  \index{itk::SpatialObjectToImageFilter!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkPolygonSpatialObject.h"
#include "itkSpatialObjectToImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileWriter.h"


int main( int argc, char *argv[] )
{
  if( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0] << " outputimagefile " << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  We declare the pixel type and dimension of the image to be produced as
  //  output.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef unsigned char PixelType;
  const unsigned int    Dimension = 3;

  typedef itk::Image< PixelType, Dimension >       ImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Using the same dimension, we instantiate the types of the elementary
  //  SpatialObjects that we plan to rasterize.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::PolygonSpatialObject< Dimension >  PolygonType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We instantiate the SpatialObjectToImageFilter type by using as template
  //  arguments the input SpatialObject and the output image types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::SpatialObjectToImageFilter<
    PolygonType, ImageType >   SpatialObjectToImageFilterType;

  SpatialObjectToImageFilterType::Pointer imageFilter =
    SpatialObjectToImageFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The SpatialObjectToImageFilter requires that the user defines the grid
  //  parameters of the output image. This includes the number of pixels along
  //  each dimension, the pixel spacing, image direction and
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::SizeType size;
  size[ 0 ] =  100;
  size[ 1 ] =  100;
  size[ 2 ] =    1;

  imageFilter->SetSize( size );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  ImageType::SpacingType spacing;
  spacing[0] =  100.0 / size[0];
  spacing[1] =  100.0 / size[1];
  spacing[2] =    1.0;

  imageFilter->SetSpacing( spacing );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We create the polygon object.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PolygonType::Pointer polygon = PolygonType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We populate the points of the polygon by computing the edges of a
  //  hexagon centered in the image.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfPoints = 6;
  PolygonType::PointType point;
  PolygonType::PointType::VectorType radial;
  radial[0] = 0.0;
  radial[1] = 0.0;
  radial[2] = 0.0;

  PolygonType::PointType center;
  center[0] = 50.0;
  center[1] = 50.0;
  center[2] =  0.0;

  const double radius = 40.0;

  for( unsigned int i=0; i < numberOfPoints; i++ )
    {
    const double angle = 2.0 * itk::Math::pi * i / numberOfPoints;
    radial[0] = radius * std::cos( angle );
    radial[1] = radius * std::sin( angle );
    point = center + radial;
    polygon->AddPoint( point );
    }
  // Software Guide : EndCodeSnippet

  std::cout << "Polygon Perimeter = " << polygon->MeasurePerimeter() << std::endl;
  std::cout << "Polygon Area      = " << polygon->MeasureArea() << std::endl;

  //  Software Guide : BeginLatex
  //
  //  We connect the polygon as the input to the SpatialObjectToImageFilter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  imageFilter->SetInput(  polygon  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally we are ready to run the filter. We use the typical invocation of
  //  the \code{Update} method, and we instantiate an \code{ImageFileWriter} in
  //  order to save the generated image into a file.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< ImageType >     WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[1] );
  writer->SetInput( imageFilter->GetOutput() );

  try
    {
    imageFilter->Update();
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet


  return EXIT_SUCCESS;
}
