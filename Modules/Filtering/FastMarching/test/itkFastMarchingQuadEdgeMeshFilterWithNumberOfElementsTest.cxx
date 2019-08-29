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

#include "itkFastMarchingQuadEdgeMeshFilterBase.h"
#include "itkQuadEdgeMeshExtendedTraits.h"
#include "itkRegularSphereMeshSource.h"
#include "itkFastMarchingNumberOfElementsStoppingCriterion.h"

int
itkFastMarchingQuadEdgeMeshFilterWithNumberOfElementsTest(int, char *[])
{
  using PixelType = float;
  using CoordType = double;

  constexpr unsigned int Dimension = 3;

  using Traits = itk::QuadEdgeMeshExtendedTraits<PixelType, // type of data for vertices
                                                 Dimension, // geometrical dimension of space
                                                 2,         // Mac topological dimension of a cell
                                                 CoordType, // type for point coordinate
                                                 CoordType, // type for interpolation weight
                                                 PixelType, // type of data for cell
                                                 bool,      // type of data for primal edges
                                                 bool       // type of data for dual edges
                                                 >;

  using MeshType = itk::QuadEdgeMesh<PixelType, Dimension, Traits>;

  using FastMarchingType = itk::FastMarchingQuadEdgeMeshFilterBase<MeshType, MeshType>;

  MeshType::PointType center;
  center.Fill(0.);

  using SphereSourceType = itk::RegularSphereMeshSource<MeshType>;
  SphereSourceType::Pointer sphere_filter = SphereSourceType::New();
  sphere_filter->SetCenter(center);
  sphere_filter->SetResolution(5);
  sphere_filter->Update();

  MeshType::Pointer sphere_output = sphere_filter->GetOutput();

  MeshType::PointsContainerConstPointer points = sphere_output->GetPoints();

  MeshType::PointsContainerConstIterator p_it = points->Begin();
  MeshType::PointsContainerConstIterator p_end = points->End();

  while (p_it != p_end)
  {
    sphere_output->SetPointData(p_it->Index(), 1.);
    ++p_it;
  }

  using NodePairType = FastMarchingType::NodePairType;
  //  using NodeContainerType = FastMarchingType::NodeContainerType;
  using NodePairContainerType = FastMarchingType::NodePairContainerType;

  NodePairContainerType::Pointer trial = NodePairContainerType::New();

  NodePairType node_pair(0, 1.);
  trial->push_back(node_pair);

  using CriterionType = itk::FastMarchingNumberOfElementsStoppingCriterion<MeshType, MeshType>;
  CriterionType::Pointer criterion = CriterionType::New();
  criterion->SetTargetNumberOfElements(100);

  FastMarchingType::Pointer fmm_filter = FastMarchingType::New();
  fmm_filter->SetInput(sphere_output);
  fmm_filter->SetTrialPoints(trial);
  fmm_filter->SetStoppingCriterion(criterion);

  try
  {
    fmm_filter->Update();
  }
  catch (itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
  }

  MeshType::Pointer output = fmm_filter->GetOutput();

  MeshType::PointDataContainerPointer pointdata = output->GetPointData();

  MeshType::PointDataContainerIterator it = pointdata->Begin();

  unsigned int counter = 0;

  while (it != pointdata->End())
  {
    if (it->Value() > 0.)
    {
      ++counter;
    }
    ++it;
  }

  if (counter >= 100)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}
