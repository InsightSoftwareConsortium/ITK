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

#include "itkInteriorExteriorMeshFilter.h"
#include "itkMesh.h"
#include "itkSphereSpatialFunction.h"

int itkInteriorExteriorMeshFilterTest(int, char* [] )
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
        p[0] = x;
        p[1] = y;
        p[2] = z;
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
