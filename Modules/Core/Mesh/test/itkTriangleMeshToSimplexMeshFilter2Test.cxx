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

#include "itkRegularSphereMeshSource.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkTimeProbe.h"

int itkTriangleMeshToSimplexMeshFilter2Test(int , char *[] )
{

  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3, double, double, double> MeshTraits;

  typedef itk::Mesh<double,3,MeshTraits>        TriangleMeshType;
  typedef itk::SimplexMesh<double,3,MeshTraits> SimplexMeshType;


  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType>  SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType                 PointType;
  typedef SphereMeshSourceType::VectorType                VectorType;

  // Declare the type of the gradient image
  typedef itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType>  SimplexFilterType;

  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center; center.Fill(0);
  PointType::ValueType scaleInit[3] = {10,10,10};
  VectorType scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(2);
  mySphereMeshSource->SetScale(scale);

  SimplexFilterType::Pointer simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput( mySphereMeshSource->GetOutput() );
  simplexFilter->Update();

  SimplexMeshType::Pointer simplexMesh = simplexFilter->GetOutput();
  simplexMesh->DisconnectPipeline();

  typedef  SimplexMeshType::NeighborListType              NeighborsListType;

  for (int i=0; i < 7; i++)
    {
    itk::TimeProbe timeProbe;
    NeighborsListType* neighbors = ITK_NULLPTR;

    timeProbe.Start();
    const unsigned int lastIndex = simplexMesh->GetPoints()->Size();
    for (unsigned int pointIndex = 0; pointIndex < lastIndex; pointIndex++)
      {
      neighbors = simplexMesh->GetNeighbors( pointIndex, i );
      if (pointIndex != (lastIndex - 1))
        {
        delete neighbors;
        }
      }
    timeProbe.Stop();
    if(neighbors)
      {
      std::cout << "Rigidity: " << i << ", neighbor list size: " << neighbors->size() << std::endl;
      delete neighbors;
      }
    else
      {
      std::cout << "Rigidity: " << i << ", no neighbor" << std::endl;
      }
    std::cout << ", Elapsed time (for getting neighbors): " << timeProbe.GetMean() << std::endl;
    }

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;
}
