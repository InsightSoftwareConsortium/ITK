/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTriangleMeshToBinaryImageFilterTest.cxx
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

/**
 *  
 *  This program illustrates the use of rasterization algorithm
 *  using a sphere of center (50,50,50) and radius 10 and 
 *  creates a binary image of size (100,100,100) 
 *
 */

#include "itkRegularSphereMeshSource.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkMesh.h"
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include <itkTriangleMeshToBinaryImageFilter.h>

int itkTriangleMeshToBinaryImageFilterTest(int, char* [] )
{
  
  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3> TriangleMeshTraits;
  typedef itk::Mesh<double,3, TriangleMeshTraits> TriangleMeshType;
  
  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType>  SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType PointType;
  typedef SphereMeshSourceType::VectorType VectorType;

  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center; 
  center[0] = 50;
  center[1] = 50;
  center[2] = 50;
  PointType::ValueType scaleInit[3] = {10,10,10};
  VectorType scale = scaleInit;
  
  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(3); 
  mySphereMeshSource->SetScale(scale);
  mySphereMeshSource->Update();

  std::cout << "Triangle mesh created. " << std::endl;
  std::cout << "with " << mySphereMeshSource->GetOutput()->GetNumberOfPoints() << " points";
  std::cout << "and " << mySphereMeshSource->GetOutput()->GetNumberOfCells() << " cells." << std::endl;
  
  std::cout << "Sending triangle mesh to rasterization algorithm. " << std::endl;
  typedef itk::Image<unsigned char,3> ImageType;
  
  typedef itk::TriangleMeshToBinaryImageFilter<TriangleMeshType,ImageType> TriangleMeshToBinaryImageFilterType;
  TriangleMeshToBinaryImageFilterType::Pointer imageFilter = TriangleMeshToBinaryImageFilterType::New();
  imageFilter->SetInput(mySphereMeshSource->GetOutput());
  ImageType::SizeType size;
  
  size[0]=100;
  size[1]=100;
  size[2]=100;
  imageFilter->SetSize(size);

  imageFilter->Update();

  std::cout << "[PASSED]" << std::endl;

  //uncomment this if you want to save the image 
  /*
  typedef itk::ImageFileWriter<ImageType > WriterType;

  WriterType::Pointer ImageWriter = WriterType::New();
  ImageWriter->SetInput(imageFilter->GetOutput() );
  ImageWriter->SetFileName("mybinaryimage.mhd");
  ImageWriter->Update();*/
  
  std::cout << "Test [DONE]" << std::endl;
  
  return EXIT_SUCCESS;
}
