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

#include <iostream>

#include "itkSimplexMesh.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkTimeProbe.h"

int itkSimplexMeshTest(int , char *[] )
{

  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3, double, double, double> MeshTraits;

  typedef itk::SimplexMesh<double,3,MeshTraits>           SimplexMeshType;
  typedef itk::SimplexMeshGeometry                        SimplexMeshGeometryType;

  SimplexMeshType::Pointer simplexMesh = SimplexMeshType::New();

  typedef  SimplexMeshType::NeighborListType              NeighborsListType;

  NeighborsListType* neighbors = ITK_NULLPTR;

  /**
   * Define the 3d geometric positions for 8 points in a cube.
   */
  SimplexMeshType::CoordRepType testPointCoords[8][3]
    = { {0,0,0}, {9,0,0}, {9,0,9}, {0,0,9},
        {0,9,0}, {9,9,0}, {9,9,9}, {0,9,9} };


  /**
   * Typedef the generic cell type for the mesh.  It is an abstract class,
   * so we can only use information from it, like get its pointer type.
   */
  typedef SimplexMeshType::PointType      PointType;

  /**
   * Add our test points to the mesh.
   * simplexMesh->SetPoint(pointId, point)
   * Note that the constructor for Point is public, and takes an array
   * of coordinates for the point.
   */
  for(int i = 0; i < 8; ++i)
    {
    simplexMesh->SetPoint(i, PointType(testPointCoords[i]));
    simplexMesh->SetGeometryData(i, new SimplexMeshGeometryType );
    }

  /**
   * Specify the method used for allocating cells
   */
   simplexMesh->SetCellsAllocationMethod( SimplexMeshType::CellsAllocatedDynamicallyCellByCell );


   /**
    * Exercise the AddEdge method
    */
  simplexMesh->AddEdge( 0, 1 );
  simplexMesh->AddEdge( 0, 3 );
  simplexMesh->AddEdge( 0, 4 );
  simplexMesh->AddEdge( 1, 2 );
  simplexMesh->AddEdge( 1, 5 );
  simplexMesh->AddEdge( 2, 3 );
  simplexMesh->AddEdge( 2, 6 );
  simplexMesh->AddEdge( 3, 7 );
  simplexMesh->AddEdge( 4, 5 );
  simplexMesh->AddEdge( 4, 7 );
  simplexMesh->AddEdge( 5, 6 );
  simplexMesh->AddEdge( 6, 7 );

   /**
    * Exercise the AddNeighbor method
    */
  simplexMesh->AddNeighbor( 0, 1 );
  simplexMesh->AddNeighbor( 0, 3 );
  simplexMesh->AddNeighbor( 0, 4 );
  simplexMesh->AddNeighbor( 1, 2 );
  simplexMesh->AddNeighbor( 1, 5 );
  simplexMesh->AddNeighbor( 2, 3 );
  simplexMesh->AddNeighbor( 2, 6 );
  simplexMesh->AddNeighbor( 3, 7 );
  simplexMesh->AddNeighbor( 4, 5 );
  simplexMesh->AddNeighbor( 4, 7 );
  simplexMesh->AddNeighbor( 5, 6 );
  simplexMesh->AddNeighbor( 6, 7 );


  // Now add the symmetric relationships
  simplexMesh->AddNeighbor( 1, 0 );
  simplexMesh->AddNeighbor( 3, 0 );
  simplexMesh->AddNeighbor( 4, 0 );
  simplexMesh->AddNeighbor( 2, 1 );
  simplexMesh->AddNeighbor( 5, 1 );
  simplexMesh->AddNeighbor( 3, 2 );
  simplexMesh->AddNeighbor( 6, 2 );
  simplexMesh->AddNeighbor( 7, 3 );
  simplexMesh->AddNeighbor( 5, 4 );
  simplexMesh->AddNeighbor( 7, 4 );
  simplexMesh->AddNeighbor( 6, 5 );
  simplexMesh->AddNeighbor( 7, 6 );

  itk::TimeProbe timeProbe;

  for (unsigned int i=0; i < 2; i++)
    {
    timeProbe.Start();
    for (unsigned int pointIndex = 0; pointIndex < simplexMesh->GetPoints()->Size(); pointIndex++)
      {
      neighbors = simplexMesh->GetNeighbors( pointIndex, i, neighbors );
      }
    timeProbe.Stop();
    if (neighbors)
      {
      std::cout << "Rigidity: " << i << ", neighbor list size: " << neighbors->size() << std::endl;
      delete neighbors;
      neighbors = ITK_NULLPTR;
      }

    std::cout << ", Elapsed time (for getting neighbors): " << timeProbe.GetMean() << std::endl;
    }

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}
