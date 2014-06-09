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

#include "itkTransformMeshFilter.h"
#include "itkMesh.h"
#include "itkAffineTransform.h"
#include "itkStdStreamStateSave.h"

int itkTransformMeshFilterTest(int, char* [] )
{
// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

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
  typedef itk::Transform<float,3,3>     BaseTransformType;


  // Declare the type for the filter
  typedef itk::TransformMeshFilter<
                                MeshType,
                                MeshType,
                                TransformType  >       FilterType;
  typedef itk::TransformMeshFilter<
                                MeshType,
                                MeshType,
                                BaseTransformType  >   FilterWithBaseTransformType;


  // Create a Filter
  FilterType::Pointer filter = FilterType::New();
  FilterWithBaseTransformType::Pointer filterwithbasetrfs
    = FilterWithBaseTransformType::New();

  // Create a Transform
  TransformType::Pointer   affineTransform = TransformType::New();
  affineTransform->Scale( 3.5 );
  TransformType::OffsetType::ValueType tInit[3] = {100,200,300};
  TransformType::OffsetType   translation = tInit;
  affineTransform->Translate( translation );

  // Connect the inputs
  filter->SetInput( inputMesh );
  filter->SetTransform( affineTransform );

  filterwithbasetrfs->SetInput( inputMesh );
  filterwithbasetrfs->SetTransform( affineTransform );

  // Execute the filter
  filter->Update();
  std::cout << "Filter: " << filter;

  filterwithbasetrfs->Update();
  std::cout << "Filter with base transform: " << filterwithbasetrfs;

  // Get the Smart Pointer to the Filter Output
  MeshType::Pointer outputMesh = filter->GetOutput();
  MeshType::Pointer outputMeshFromWithBase = filterwithbasetrfs->GetOutput();

  std::cout << "Output Mesh has " << outputMesh->GetNumberOfPoints()
            << "   points " << std::endl;

  std::cout << "Output Mesh from WithBaseTransfrom has "
            << outputMeshFromWithBase->GetNumberOfPoints()
            << "   points " << std::endl;

  // Get the the point container
  MeshType::PointsContainerPointer
    transformedPoints = outputMesh->GetPoints();

  MeshType::PointsContainerPointer
    transformedPointsFromWithBase = outputMeshFromWithBase->GetPoints();


  PointsContainerType::ConstIterator it = transformedPoints->Begin();
  PointsContainerType::ConstIterator itfwb = transformedPointsFromWithBase->Begin();
  while( it != transformedPoints->End() )
    {
    PointType p = it.Value();
    std::cout.width( 5 ); std::cout << p[0] << ", ";
    std::cout.width( 5 ); std::cout << p[1] << ", ";
    std::cout.width( 5 ); std::cout << p[2] << std::endl;
    ++it;

    PointType pfwb = itfwb.Value();
    std::cout.width( 5 ); std::cout << pfwb[0] << ", ";
    std::cout.width( 5 ); std::cout << pfwb[1] << ", ";
    std::cout.width( 5 ); std::cout << pfwb[2] << std::endl;
    ++itfwb;
    }

  // All objects should be automatically destroyed at this point

  return EXIT_SUCCESS;

}
