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
#include "itkSpatialObjectToImageFilter.h"
#include <iostream>

int itkPolylineMaskImageFilterTest(int , char * [] )
{
  /*
  if(argc < 3)
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Filter usage:" << std :: endl;
    std::cerr << "   SpatialObjectOutputImage" << "   MaskedOutputImage"<<std::endl;
    exit(1);
    }
  */
  // Define the dimension of the images
  const unsigned int iDimension = 3;

  //Define the dimension of the polyline
  const unsigned int pDimension = 2;


  // Declare the types of the images
  typedef itk::Image<unsigned short, iDimension>     inputImageType;
  typedef itk::Image<unsigned short, iDimension>     outputImageType;
  typedef itk::Vector<double, iDimension>            inputVectorType;
  typedef itk::PolyLineParametricPath<pDimension>    inputPolylineType;

  // Create vector
  inputVectorType   inputUpVector,inputViewVector;

  // Create polyline
  inputPolylineType::Pointer inputPolyline   = inputPolylineType::New();
  std::cout<<"Generating the synthetic object...."<<std::endl;
  //Generate ellipse image

  typedef itk::EllipseSpatialObject<2>   EllipseType;
  EllipseType::Pointer ellipse = EllipseType::New();
  EllipseType::TransformType::OffsetType offset;
  offset.Fill(20);
  ellipse->GetObjectToParentTransform()->SetOffset(offset);
  ellipse->ComputeObjectToWorldTransform();
  ellipse->SetRadius(10);

  std::cout<<"Generating the image of the object...."<<std::endl;

  typedef itk::SpatialObjectToImageFilter<EllipseType,inputImageType> SpatialObjectToImageFilterType;
  SpatialObjectToImageFilterType::Pointer imageGenerationFilter = SpatialObjectToImageFilterType::New();

  inputImageType::SizeType size;
  inputImageType::PointType origin;

  origin[0] = 0.0;
  origin[1] = 0.0;
  origin[2] = 20.0;

  size[0]=40;
  size[1]=40;
  size[2]=35;

  imageGenerationFilter->SetOrigin(origin);
  imageGenerationFilter->SetSize(size);
  imageGenerationFilter->SetInput(ellipse);
  imageGenerationFilter->SetInsideValue(2);
  imageGenerationFilter->SetOutsideValue(0);
  imageGenerationFilter->Update();


  //Write out the input image
/*
  typedef  itk::ImageFileWriter<  inputImageType  > SpatialObjectImageWriterType;
  SpatialObjectImageWriterType::Pointer spatialObjectImageWriter = SpatialObjectImageWriterType::New();
  spatialObjectImageWriter->SetFileName( argv[1] );
  spatialObjectImageWriter->SetInput( imageGenerationFilter->GetOutput() );
  spatialObjectImageWriter->Update();
*/

  std::cout << "Generating the polyline contour..." << std::endl;
  //Initialize the polyline
  typedef inputPolylineType::VertexType VertexType;

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


  std::cout << "Generating the view vector..... " << std::endl;

  // View vector
  inputViewVector[0] = 0;
  inputViewVector[1] = 0;
  inputViewVector[2] = -1;

  // Up vector
  inputUpVector[0] = 1;
  inputUpVector[1] = 0;
  inputUpVector[2] = 0;

  // Declare the type for the Mask image filter
  typedef itk::PolylineMaskImageFilter<
                           inputImageType, inputPolylineType,
                           inputVectorType,
                           outputImageType  >     inputFilterType;

  typedef inputFilterType::PointType          PointType;
  typedef inputFilterType::ProjPlanePointType ProjPlanePointType;

  std::cout<< "Generating the filter....................." << std::endl;

  // Create a mask  Filter
  inputFilterType::Pointer filter = inputFilterType::New();

  //filter->DebugOn();
  //Connect the input image
  filter->SetInput1    ( imageGenerationFilter->GetOutput());

  inputImageType::PointType originA;

  /*
  originA = imageGenerationFilter->GetOutput()->GetOrigin();
  std::cout<<"Input image origin="<<originA<<std::endl;
*/

  // Connect the Polyline
  filter->SetInput2    ( inputPolyline );

  // Connect the Viewing direction vector
  filter->SetViewVector   ( inputViewVector );

  // Connect the Viewing direction vector
  filter->SetUpVector   ( inputUpVector );

  // camera center point
  PointType cameraCenterPoint;

  cameraCenterPoint[0] = 20;
  cameraCenterPoint[1] = 20;
  cameraCenterPoint[2] = 60;

  filter->SetCameraCenterPoint   ( cameraCenterPoint );

  // camera focal distance
  filter->SetFocalDistance(30.0);

  // camera focal point in the projection plane
  ProjPlanePointType focalpoint;
  focalpoint[0] = 20.0;
  focalpoint[1] = 20.0;
  filter->SetFocalPoint(focalpoint);
  filter->Update();

  //Write out the output image

  /*
  typedef  itk::ImageFileWriter<  outputImageType  > OutputWriterType;
  OutputWriterType::Pointer outputWriter = OutputWriterType::New();
  outputWriter->SetFileName( argv[2] );
  outputWriter->SetInput( filter->GetOutput() );
  outputWriter->Update();
*/


  return EXIT_SUCCESS;

}
