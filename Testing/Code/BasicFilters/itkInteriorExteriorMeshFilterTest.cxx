/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInteriorExteriorMeshFilterTest.cxx
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
#include <itkInteriorExteriorMeshFilter.h>
#include <itkMesh.h>
#include <itkSphereSpatialFunction.h>

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
  int n = 3;  // let's start with a few of them
  PointsContainerType::ElementIdentifier  count = 0; // count them

  for(int x= -n; x <= n; x++)
    {
    for(int y= -n; y <= n; y++)
      {
      for(int z= -n; z <= n; z++)
        {
        PointType p;
        p = (float)x,(float)y,(float)z;
        points->InsertElement( count, p );
        count++;
        }
      }
    }
  
  
  // Declare the function type
  typedef itk::SphereSpatialFunction< 
                                MeshType::PointDimension,
                                MeshType::PointType >  
                                            SpatialFunctionType;
  

  // Declare the type for the filter
  typedef itk::InteriorExteriorMeshFilter<
                                MeshType,
                                MeshType,
                                SpatialFunctionType  > FilterType;
            

  // Create a Filter                                
  FilterType::Pointer filter = FilterType::New();
  
  // Create the Spatial Function
  SpatialFunctionType::Pointer   spatialFunction = 
                                      SpatialFunctionType::New();

  SpatialFunctionType::InputType center;
  center[0] = 0;
  center[1] = 0;
  center[2] = 2;   // Here we are assuming 3D !!!

  const double radius = 1.1f;

  spatialFunction->SetCenter( center );
  spatialFunction->SetRadius( radius );

  // Connect the inputs
  filter->SetInput( inputMesh ); 
  filter->SetSpatialFunction( spatialFunction ); 

  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output 
  MeshType::Pointer outputMesh = filter->GetOutput();


  // Get the the point container
  MeshType::PointsContainerPointer  
                  transformedPoints = outputMesh->GetPoints();


  PointsContainerType::ConstIterator it = transformedPoints->Begin();
  while( it != transformedPoints->End() )
    {
    PointType p = it.Value();

    const double distance = p.EuclideanDistanceTo( center );
    if( distance > radius ) 
      {
      std::cerr << "Point " << p << std::endl;
      std::cerr << " is at distance  " << distance << std::endl;
      std::cerr << " from the center " << center << std::endl;
      std::cerr << " so it is outside the sphere of radius ";
      std::cerr << radius << std::endl;
      return EXIT_FAILURE;
      }
    ++it;
    }
  
  // All objects shall be automatically destroyed at this point

  std::cout << "Test passed ! " << std::endl;
  return EXIT_SUCCESS;

}




