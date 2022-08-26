/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkTestingMacros.h"

int
itkInteriorExteriorMeshFilterTest(int, char *[])
{

  // Declare the mesh pixel type.
  // Those are the values associated
  // with each mesh point. (not used on this filter test)
  using PixelType = int;

  // Declare the types of the Mesh
  // By default it is a 3D mesh using itk::Point<float,3>
  // on the vertices, and an itk::VectorContainer
  // as container for points
  using MeshType = itk::Mesh<PixelType>;

  // Declare the type for PointsContainer
  using PointsContainerType = MeshType::PointsContainer;

  // Declare the type for PointsContainerPointer
  using PointsContainerPointer = MeshType::PointsContainerPointer;
  // Declare the type for Points
  using PointType = MeshType::PointType;

  // Create an input Mesh
  auto inputMesh = MeshType::New();

  // Insert data on the Mesh
  PointsContainerPointer points = inputMesh->GetPoints();

  // Fill a cube with points , just to get some data
  int                                    n = 3;     // let's start with a few of them
  PointsContainerType::ElementIdentifier count = 0; // count them

  for (int x = -n; x <= n; ++x)
  {
    for (int y = -n; y <= n; ++y)
    {
      for (int z = -n; z <= n; ++z)
      {
        PointType p;
        p[0] = x;
        p[1] = y;
        p[2] = z;
        points->InsertElement(count, p);
        count++;
      }
    }
  }


  // Declare the function type
  using SpatialFunctionType = itk::SphereSpatialFunction<MeshType::PointDimension, MeshType::PointType>;


  // Declare the type for the filter
  using FilterType = itk::InteriorExteriorMeshFilter<MeshType, MeshType, SpatialFunctionType>;


  // Create a Filter
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, InteriorExteriorMeshFilter, MeshToMeshFilter);


  // Test exceptions
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());

  // Create the Spatial Function
  auto spatialFunction = SpatialFunctionType::New();

  SpatialFunctionType::InputType center;
  center[0] = 0;
  center[1] = 0;
  center[2] = 2; // Here we are assuming 3D !!!

  const double radius = 1.1f;

  spatialFunction->SetCenter(center);
  spatialFunction->SetRadius(radius);

  filter->SetSpatialFunction(spatialFunction);
  ITK_TEST_SET_GET_VALUE(spatialFunction, filter->GetSpatialFunction());

  // Connect the inputs
  filter->SetInput(inputMesh);

  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output
  MeshType::Pointer outputMesh = filter->GetOutput();


  // Get the the point container
  MeshType::PointsContainerPointer transformedPoints = outputMesh->GetPoints();


  PointsContainerType::ConstIterator it = transformedPoints->Begin();
  while (it != transformedPoints->End())
  {
    PointType p = it.Value();

    const double distance = p.EuclideanDistanceTo(center);
    if (distance > radius)
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
