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

#include "itkRegularSphereMeshSource.h"

int itkRegularSphereMeshSourceTest2(int, char* [] )
{
  const unsigned int Dimension = 3;
  typedef float PixelType;

  typedef itk::Mesh< PixelType, Dimension > MeshType;

  typedef itk::RegularSphereMeshSource< MeshType >  SphereMeshSourceType;
  SphereMeshSourceType::VectorType scale;
  scale[0] = 1.;
  scale[1] = 2.;
  scale[2] = 3.;

  SphereMeshSourceType::Pointer  source1 = SphereMeshSourceType::New();
  source1->SetResolution( 1 );
  source1->SetScale( scale );
  source1->Update();

  MeshType::Pointer mesh1 = source1->GetOutput();

  if( mesh1->GetCellsAllocationMethod() != MeshType::CellsAllocatedDynamicallyCellByCell )
    {
    std::cerr << "mesh1->GetCellsAllocationMethod() != MeshType::CellsAllocatedDynamicallyCellByCell"
              << std::endl;
    return EXIT_FAILURE;
    }


  SphereMeshSourceType::Pointer  source2 = SphereMeshSourceType::New();
  source2->SetResolution( 2 );
  source2->SetScale( scale );
  source2->Update();

  MeshType::Pointer mesh2 = source2->GetOutput();

  mesh2->Graft( mesh1 );

  MeshType::PointsContainerConstPointer points = mesh2->GetPoints();
  MeshType::PointsContainerConstIterator it = points->Begin();
  MeshType::PointsContainerConstIterator end = points->End();

  MeshType::PointType center = source2->GetCenter();
  SphereMeshSourceType::VectorType scale2 = source2->GetScale();

  if( ( scale2 - scale ).GetNorm() != 0. )
    {
    std::cerr << "scale2 != scale" << std::endl;
    return EXIT_FAILURE;
    }

  while( it != end )
    {
    const MeshType::PointType p = it->Value();
    double d = itk::NumericTraits< double >::ZeroValue();
    for( unsigned int dim = 0; dim < Dimension; dim++ )
      {
      d += ( center[dim] - p[dim] ) * ( center[dim] - p[dim] ) / ( scale[dim] * scale[dim] );
      }

    if( itk::Math::abs( d - 1. ) > 1e-6 )
      {
      std::cerr << "too much deviation for vertex " << it->Index() << std::endl;
      std::cerr << "distance: " << d << std::endl;
      return EXIT_FAILURE;
      }
    ++it;
    }

  return EXIT_SUCCESS;

}
