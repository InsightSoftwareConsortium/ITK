/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransformMeshFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
  int n = 1;  // let's start with a few of them
  PointsContainerType::ElementIdentifier  count = 0; // count them

  for(int x= -n; x <= n; x++)
  {
    for(int y= -n; y <= n; y++)
    {
      for(int z= -n; z <= n; z++)
      {
        PointType p;
        p = (float)x,(float)y,(float)z;
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


  // Declare the type for the filter
  typedef itk::AffineTransformMeshFilter<
                                MeshType,
                                MeshType  >       FilterType;
            

  // Create a Filter                                
  FilterType::Pointer filter = FilterType::New();


  // Create an Affine Transform 
  // (it doesn't use smart pointers)
  FilterType::AffineTransformType   affineTransform;

  affineTransform.Scale( 3.5 );

  FilterType::AffineTransformType::VectorType   translation;

  translation = 100.0, 200.0, 300.0;

  affineTransform.Translate( translation );

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
    std::cout.width( 5 ); std::cout << p[0] << ", ";
    std::cout.width( 5 ); std::cout << p[1] << ", ";
    std::cout.width( 5 ); std::cout << p[2] << std::endl;
    ++it;
  }
  
  // All objects should be automatically destroyed at this point

  return 0;

}




