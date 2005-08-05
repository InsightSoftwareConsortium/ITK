/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolylineMaskImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <itkImage.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkPolylineMaskImageFilter.h>
#include <itkPolyLineParametricPath.h>
#include <itkEllipseSpatialObject.h>
#include <itkSpatialObjectToImageFilter.h>
#include <itkImageFileWriter.h>

int itkPolylineMaskImageFilterTest(int argc , char * argv [] ) 
{

  // Define the dimension of the images
  const unsigned int iDimension = 3;

  //Define the dimension of the polyline
  const unsigned int pDimension = 2;


  // Declare the types of the images
  typedef itk::Image<unsigned short, iDimension>     inputImageType;
  typedef itk::Image<unsigned short, iDimension>     outputImageType;
  typedef itk::Vector<double, iDimension>            inputVectorType;
  typedef itk::PolyLineParametricPath<pDimension>    inputPolylineType;

  // Declare the type of the index to access images
  typedef itk::Index<iDimension>         inputIndexType;

  // Declare the type of the size 
  typedef itk::Size<iDimension>          inputSizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<iDimension>   inputRegionType;

  // Create vector
  inputVectorType   inputUpVector,inputViewVector;

  // Create polyline
  inputPolylineType::Pointer inputPolyline   = inputPolylineType::New();
  std::cout<<"Generating the synthetic object...."<<std::endl;
  //Generate ellipse image

  typedef itk::EllipseSpatialObject<2>   EllipseType;
  EllipseType::Pointer ellipse = EllipseType::New();
  EllipseType::TransformType::OffsetType offset;
  offset.Fill(15);
  ellipse->GetObjectToParentTransform()->SetOffset(offset);
  ellipse->ComputeObjectToWorldTransform();
  ellipse->SetRadius(15);

  std::cout<<"Generating the image of the object...."<<std::endl;

  typedef itk::SpatialObjectToImageFilter<EllipseType,inputImageType> SpatialObjectToImageFilterType;
  SpatialObjectToImageFilterType::Pointer imageFilter = SpatialObjectToImageFilterType::New();
   
  inputImageType::SizeType size;
  size[0]=40;
  size[1]=40;
  size[2]=30;

  imageFilter->SetSize(size);
  imageFilter->SetInput(ellipse);
  imageFilter->SetInsideValue(2);
  imageFilter->SetOutsideValue(0);
  imageFilter->Update();

  
  //Create images
  inputImageType::Pointer inputImage    = inputImageType::New();
  outputImageType::Pointer outputImage  = outputImageType::New();

  std::cout << "Generating the polyline..." << std::endl;
  //Initialize the polyline 
  typedef inputPolylineType::VertexType VertexType;
  
  // Add vertices to the polyline
  VertexType v;
  v[0] = 8;
  v[1] = 8;
  inputPolyline->AddVertex(v);
  
  v[0] = 23;
  v[1] = 8;
  inputPolyline->AddVertex(v);
  
  v[0] = 23;
  v[1] = 23;
  inputPolyline->AddVertex(v);

  v[0] = 8;
  v[1] = 23;
  inputPolyline->AddVertex(v);
  
  
  std::cout << "Generating the view vector " << std::endl; 
 
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

  typedef inputFilterType::PointType PointType;
  typedef inputFilterType::ProjPlanePointType ProjPlanePointType;
            
  std::cout<< "Generating the filter....................." << std::endl;

  // Create a mask  Filter                                
  inputFilterType::Pointer filter = inputFilterType::New();

  //Connect the input image
  filter->SetInput1    ( imageFilter->GetOutput()); 
 
  // Connect the Polyline 
  filter->SetInput2    ( inputPolyline ); 

  // Connect the Viewing direction vector
  filter->SetViewVector   ( inputViewVector );

  // Connect the Viewing direction vector
  filter->SetUpVector   ( inputUpVector );

  // camera center point
  PointType cameraCenterPoint;
  cameraCenterPoint[0] = 15;
  cameraCenterPoint[1] = 15;
  cameraCenterPoint[2] = 60;

  filter->SetCameraCenterPoint   ( cameraCenterPoint );

  // camera focal distance 
  filter->SetFocalDistance(15);
  
  // camera focal point in the projection plane
  ProjPlanePointType focalpoint;
  focalpoint[0] = 15;
  focalpoint[1] = 15;
  filter->SetFocalPoint(focalpoint);
  filter->Update();

  return 0;

}




