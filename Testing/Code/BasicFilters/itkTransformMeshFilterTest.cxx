/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformMeshFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <itkTransformMeshFilter.h>
#include <itkMesh.h>
#include <itkAffineTransform.h>

int itkTransformMeshFilterTest(int, char* [] ) 
{

  // Declare the mesh pixel type.
  // Those are the values associated 
  // with each mesh point. (not used on this filter test)
  typedef int PixelType;
  
  // Declare the types of the Mesh
  // By default it is a 3D mesh using itk::Point<float,3>
  // on the vertices, and an itk::VectorContainter
  // as containter for points
  typedef itk::Mesh<PixelType>  MeshType;

  // Declare the type for PointsContainer
  typedef MeshType::PointsContainer     PointsContainerType;

  // Declare the type for PointsContainerPointer
  typedef MeshType::PointsContainerPointer     
                                        PointsContainerPointer;
  // Declare the type for Points
  typedef MeshType::PointType           PointType;

  // Create an input Mesh
  MeshType::Pointer inputMesh  = MeshType::New();

  // Insert data on the Mesh
  PointsContainerPointer  points = inputMesh->GetPoints();

  // Fill a cube with points , just to get some data
  int n = 1;  // let's start with a few of them
  PointsContainerType::ElementIdentifier  count = 0; // count them

  for(int x= -n; x <= n; x++)
    {
    for(int y= -n; y <= n; y++)
      {
      for(int z= -n; z <= n; z++)
        {
        PointType p;
        p[0] = x;
        p[1] = y;
        p[2] = z;
        std::cout << "Inserting point # ";
        std::cout.width( 3); std::cout << count << "  = ";
        std::cout.width( 4); std::cout << p[0] << ", ";
        std::cout.width( 4); std::cout << p[1] << ", ";
        std::cout.width( 4); std::cout << p[2] << std::endl;
        points->InsertElement( count, p );
        count++;
        }
      }
    }
  
  std::cout << "Input Mesh has " << inputMesh->GetNumberOfPoints();
  std::cout << "   points " << std::endl;

  
  // Declare the transform type
  typedef itk::AffineTransform<float,3> TransformType;
  

  // Declare the type for the filter
  typedef itk::TransformMeshFilter<
                                MeshType,
                                MeshType,
                                TransformType  >       FilterType;
            

  // Create a Filter                                
  FilterType::Pointer filter = FilterType::New();
  
  // Create an  Transform 
  // (it doesn't use smart pointers)
  TransformType::Pointer   affineTransform = TransformType::New();
  affineTransform->Scale( 3.5 );
  TransformType::OffsetType::ValueType tInit[3] = {100,200,300};
  TransformType::OffsetType   translation = tInit;
  affineTransform->Translate( translation );

  // Connect the inputs
  filter->SetInput( inputMesh ); 
  filter->SetTransform( affineTransform ); 

  // Execute the filter
  filter->Update();
  std::cout << "Filter: " << filter;

  // Get the Smart Pointer to the Filter Output 
  MeshType::Pointer outputMesh = filter->GetOutput();

  std::cout << "Output Mesh has " << outputMesh->GetNumberOfPoints();
  std::cout << "   points " << std::endl;

  // Get the the point container
  MeshType::PointsContainerPointer  
                  transformedPoints = outputMesh->GetPoints();


  PointsContainerType::ConstIterator it = transformedPoints->Begin();
  while( it != transformedPoints->End() )
    {
    PointType p = it.Value();
    std::cout.width( 5 ); std::cout << p[0] << ", ";
    std::cout.width( 5 ); std::cout << p[1] << ", ";
    std::cout.width( 5 ); std::cout << p[2] << std::endl;
    ++it;
    }
  
  // All objects should be automatically destroyed at this point

  return 0;

}




