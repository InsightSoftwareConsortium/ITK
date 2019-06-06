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

#include "itkPolylineMaskImageFilter.h"
#include "itkPolyLineParametricPath.h"
#include "itkEllipseSpatialObject.h"
#include "itkImageFileWriter.h"
#include "itkImageToImageFilter.h"
#include "itkSpatialObjectToImageFilter.h"
#include "itkTestingMacros.h"

#include <iostream>

int itkPolylineMaskImageFilterTest( int argc, char * argv[] )
{

  if ( argc < 2 )
    {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " outputFilename" << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  constexpr unsigned int iDimension = 3;

  //Define the dimension of the polyline
  constexpr unsigned int pDimension = 2;

  // Declare the types of the images
  using inputImageType = itk::Image<unsigned short, iDimension>;
  using outputImageType = itk::Image<unsigned short, iDimension>;
  using inputVectorType = itk::Vector<double, iDimension>;
  using inputPolylineType = itk::PolyLineParametricPath<pDimension>;

  // Create up and viewing direction vectors
  inputVectorType inputUpVector, inputViewVector;

  // Create polyline
  inputPolylineType::Pointer inputPolyline = inputPolylineType::New();
  std::cout << "Generating the synthetic object..." << std::endl;

  // Generate a synthetic ellipse image
  using EllipseType = itk::EllipseSpatialObject<2>;
  EllipseType::Pointer ellipse = EllipseType::New();
  EllipseType::PointType center;
  center.Fill(20);
  ellipse->SetCenterInObjectSpace(center);
  ellipse->SetRadiusInObjectSpace(10);
  ellipse->Update();

  std::cout << "Generating the image of the object..." << std::endl;

  using SpatialObjectToImageFilterType = itk::SpatialObjectToImageFilter<EllipseType,inputImageType>;
  SpatialObjectToImageFilterType::Pointer imageGenerationFilter = SpatialObjectToImageFilterType::New();

  inputImageType::SizeType size;
  inputImageType::PointType origin;

  origin[0] = 0.0;
  origin[1] = 0.0;
  origin[2] = 20.0;

  size[0] = 40;
  size[1] = 40;
  size[2] = 35;

  imageGenerationFilter->SetOrigin(origin);
  imageGenerationFilter->SetSize(size);
  imageGenerationFilter->SetInput(ellipse);
  imageGenerationFilter->SetInsideValue(2);
  imageGenerationFilter->SetOutsideValue(0);
  imageGenerationFilter->Update();

  // Write out the input image
  /*using SpatialObjectImageWriterType = itk::ImageFileWriter< inputImageType >;
  SpatialObjectImageWriterType::Pointer spatialObjectImageWriter =
    SpatialObjectImageWriterType::New();
  spatialObjectImageWriter->SetFileName( argv[1] );
  spatialObjectImageWriter->SetInput( imageGenerationFilter->GetOutput() );
  spatialObjectImageWriter->Update(); */

  std::cout << "Generating the polyline contour..." << std::endl;
  //Initialize the polyline
  using VertexType = inputPolylineType::VertexType;

  // Add vertices to the polyline
  VertexType v;
  v[0] = 19;
  v[1] = 0;
  inputPolyline->AddVertex(v);

  v[0] = 19;
  v[1] = 39;
  inputPolyline->AddVertex(v);

  v[0] = 25;
  v[1] = 39;
  inputPolyline->AddVertex(v);

  v[0] = 25;
  v[1] = 1;
  inputPolyline->AddVertex(v);

  std::cout << "Generating the view vector... " << std::endl;

  // View vector
  inputViewVector[0] = 0;
  inputViewVector[1] = 0;
  inputViewVector[2] = -1;

  // Up vector
  inputUpVector[0] = 1;
  inputUpVector[1] = 0;
  inputUpVector[2] = 0;

  // Declare the type for the polyline mask image filter
  using PolylineMaskImageFilterType = itk::PolylineMaskImageFilter<
                           inputImageType, inputPolylineType,
                           inputVectorType,
                           outputImageType >;

  using PointType = PolylineMaskImageFilterType::PointType;
  using ProjPlanePointType = PolylineMaskImageFilterType::ProjPlanePointType;

  std::cout << "Generating the filter..." << std::endl;

  // Create the mask filter
  PolylineMaskImageFilterType::Pointer polylineMaskFilter =
    PolylineMaskImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS( polylineMaskFilter, PolylineMaskImageFilter, ImageToImageFilter );

  // Set the input image
  polylineMaskFilter->SetInput1( imageGenerationFilter->GetOutput() );

  // Set the input the polyline
  polylineMaskFilter->SetInput2( inputPolyline );

  // Set the viewing direction vector
  polylineMaskFilter->SetViewVector( inputViewVector );

  ITK_TEST_SET_GET_VALUE( inputViewVector, polylineMaskFilter->GetViewVector() );

  // Set the up direction vector
  polylineMaskFilter->SetUpVector( inputUpVector );

  ITK_TEST_SET_GET_VALUE( inputUpVector, polylineMaskFilter->GetUpVector() );

  // Camera center point
  PointType cameraCenterPoint;

  cameraCenterPoint[0] = 20;
  cameraCenterPoint[1] = 20;
  cameraCenterPoint[2] = 60;

  polylineMaskFilter->SetCameraCenterPoint( cameraCenterPoint );

  ITK_TEST_SET_GET_VALUE( cameraCenterPoint, polylineMaskFilter->GetCameraCenterPoint() );

  // Set the camera focal distance
  double focalDistance = 30.0;
  polylineMaskFilter->SetFocalDistance( focalDistance );

  ITK_TEST_SET_GET_VALUE( focalDistance, polylineMaskFilter->GetFocalDistance() );

  // Set the camera focal point in the projection plane
  ProjPlanePointType focalpoint;
  focalpoint[0] = 20.0;
  focalpoint[1] = 20.0;
  polylineMaskFilter->SetFocalPoint( focalpoint );

  ITK_TEST_SET_GET_VALUE( focalpoint, polylineMaskFilter->GetFocalPoint() );

  polylineMaskFilter->Update();

  // Write out the output image
  using OutputWriterType = itk::ImageFileWriter< outputImageType >;
  OutputWriterType::Pointer outputWriter = OutputWriterType::New();
  outputWriter->SetFileName( argv[1] );
  outputWriter->SetInput( polylineMaskFilter->GetOutput() );
  outputWriter->Update();

  return EXIT_SUCCESS;
}
