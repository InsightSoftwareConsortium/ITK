/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransformMeshFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/



#include <itkAffineTransformMeshFilter.h>
#include <itkMesh.h>


int main() 
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
  unsigned int n = 1;  // let's start with a few of them
  PointsContainerType::ElementIdentifier  count = 0; // count them

  for(unsigned int x= -n; x<n; x++)
  {
    for(unsigned int y= -n; y<n; y++)
    {
      for(unsigned int z= -n; z<n; z++)
      {
        PointType p;
        p = (float)x,(float)y,(float)z;
        points->InsertElement( count, p );
        count++;
      }
    }
  }
      
  inputMesh->SetPoints( points );
  
  std::cout << "Input Mesh has " << inputMesh->GetNumberOfPoints();
  std::cout << "   points " << std::endl;


  // Declare the type for the filter
  typedef itk::AffineTransformMeshFilter<
                                MeshType,
                                MeshType  >       FilterType;
            

  // Create a Filter                                
  FilterType::Pointer filter = FilterType::New();


  // Create an Affine Transform 
  // (it doesn't use smart pointers)
  FilterType::AffineTransformType   affineTransform;


  // Connect the inputs
  filter->SetInput( inputMesh ); 
  filter->SetAffineTransform( affineTransform ); 

  // Execute the filter
  filter->Update();


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
    std::cout << p[0] << ", ";
    std::cout << p[1] << ", ";
    std::cout << p[2] << std::endl;
    ++it;
  }
  
  // All objects should be automatically destroyed at this point

  return 0;

}




