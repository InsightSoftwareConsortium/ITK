/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTriangleMeshToBinaryImageFilterTest2.cxx
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

#include "itkImage.h"
#include "itkMesh.h"
#include "itkSimplexMesh.h"
#include "itkRegularSphereMeshSource.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkSimplexMeshToTriangleMeshFilter.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkDefaultStaticMeshTraits.h"
#include "itkTriangleMeshToBinaryImageFilter.h"
#include "itkImageFileWriter.h"

int itkTriangleMeshToBinaryImageFilterTest2( int , char * [] )
{ 
  
  // Declare the type of the input and output mesh
  typedef itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double> TriangleMeshTraits;
  typedef itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double> SimplexMeshTraits;
  typedef itk::Mesh<double,3,TriangleMeshTraits> TriangleMeshType;
  typedef itk::SimplexMesh<double,3, SimplexMeshTraits> SimplexMeshType;

  typedef itk::Image<unsigned char, 3> ImageType;
  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType>  SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType PointType;
  typedef SphereMeshSourceType::VectorType VectorType;

  // Declare the type of the gradient image
  typedef itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType>  SimplexFilterType;

  typedef itk::SimplexMeshToTriangleMeshFilter<SimplexMeshType,TriangleMeshType>  TriangleFilterType;
  typedef TriangleMeshType::Pointer                                               TriangleMeshPointer;
  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center; center.Fill(50);
  PointType::ValueType scaleInit[3] = {10,10,10};
  VectorType scale = scaleInit;
  
  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(3);
  mySphereMeshSource->SetScale(scale);

  SimplexFilterType::Pointer simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput( mySphereMeshSource->GetOutput() );
  
  TriangleFilterType::Pointer backFilter = TriangleFilterType::New();
  backFilter->SetInput( simplexFilter->GetOutput() );
  backFilter->Update();

  SimplexMeshType::Pointer simplexMesh = simplexFilter->GetOutput();
  TriangleMeshPointer originalTriangleMesh = mySphereMeshSource->GetOutput();
 
  std::cout << " Number of Points and Cells in Original Triangle Mesh" << std::endl;
  std::cout << originalTriangleMesh->GetNumberOfPoints() << std::endl;
  std::cout << originalTriangleMesh->GetNumberOfCells() << std::endl;
  std::cout << "Original triangle mesh: " << std::endl;
  std::cout << originalTriangleMesh << std::endl;
  
  
  std::cout << "Simplex Mesh: " << simplexMesh << std::endl;
  TriangleMeshType::Pointer triangleMesh = backFilter->GetOutput();

   std::cout << " Number of Points and Cells in Back Filtered Triangle Mesh" << std::endl;
   std::cout << triangleMesh->GetNumberOfPoints() << std::endl;
   std::cout << triangleMesh->GetNumberOfCells() << std::endl;

   std::cout << "Back filtered Triangle Mesh: " << triangleMesh << std::endl;

  
  triangleMesh->DisconnectPipeline();

  typedef  itk::TriangleMeshToBinaryImageFilter<TriangleMeshType, ImageType>  TriangleImageType;

  TriangleImageType::Pointer imageFilter = TriangleImageType::New();
  
  imageFilter->SetInput(triangleMesh);
  
  ImageType::SizeType size;
  
  size[0]=100;
  size[1]=100;
  size[2]=100;
  imageFilter->SetSize(size);
  
  std::cout << "[PASSED]" << std::endl; 

  // Testing PrintSelf
  std::cout << imageFilter <<std::endl;

  //Update the filter
  imageFilter->Update();
  // uncomment to save the binary image
  /*
  typedef itk::ImageFileWriter<ImageType > WriterType;

  WriterType::Pointer ImageWriter = WriterType::New();
  ImageWriter->SetInput(imageFilter->GetOutput() );
  ImageWriter->SetFileName("mybinaryimage.mhd");
  ImageWriter->Update();
  */
  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}




