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

#include "itkFastMarchingQuadEdgeMeshFilterBase.h"
#include "itkQuadEdgeMeshExtendedTraits.h"
#include "itkFastMarchingThresholdStoppingCriterion.h"
#include "itkMeshFileWriter.h"

int
itkFastMarchingQuadEdgeMeshFilterBaseTest4(int, char *[])
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

  using MeshType = FastMarchingType::InputMeshType;

  using PointsContainer = MeshType::PointsContainer;
  using PointsContainerPointer = PointsContainer::Pointer;

  using PointDataContainer = MeshType::PointDataContainer;
  using PointDataContainerPointer = PointDataContainer::Pointer;

  // Let's create here a plane!
  auto plane = MeshType::New();

  PointsContainerPointer    points = PointsContainer::New();
  PointDataContainerPointer pointdata = PointDataContainer::New();

  points->Reserve(100);
  pointdata->Reserve(100);

  MeshType::PointType p;
  p[2] = 0.;

  int    k = 0;
  double alpha = (30.0 / 180.0) * itk::Math::pi;
  double delta = 2.0 / std::tan(alpha);

  for (int i = 0; i < 10; ++i)
  {

    for (int j = 0; j < 10; ++j)
    {
      p[0] = static_cast<CoordType>(0.5 * j * delta + i * delta);
      p[1] = static_cast<CoordType>(j);
      points->SetElement(k, p);
      pointdata->SetElement(k, 1.);
      k++;
    }
  }

  plane->SetPoints(points);
  plane->SetPointData(pointdata);

  k = 0;

  for (int i = 0; i < 9; ++i)
  {
    for (int j = 0; j < 9; ++j)
    {
      plane->AddFaceTriangle(k, k + 1, k + 11);
      plane->AddFaceTriangle(k, k + 11, k + 10);
      k++;
    }
    k++;
  }

  using NodePairType = FastMarchingType::NodePairType;
  //  using NodeContainerType = FastMarchingType::NodeContainerType;
  using NodePairContainerType = FastMarchingType::NodePairContainerType;

  auto trial = NodePairContainerType::New();

  NodePairType node_pair(0, 0.);
  trial->push_back(node_pair);

  using CriterionType = itk::FastMarchingThresholdStoppingCriterion<MeshType, MeshType>;
  auto criterion = CriterionType::New();
  criterion->SetThreshold(std::sqrt(100.0 + 182.25 * delta * delta) + 100.);

  auto fmm_filter = FastMarchingType::New();
  fmm_filter->SetInput(plane);
  fmm_filter->SetTrialPoints(trial);
  fmm_filter->SetStoppingCriterion(criterion);

  try
  {
    fmm_filter->Update();
  }
  catch (const itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
  }

  MeshType::Pointer output = fmm_filter->GetOutput();

  PointDataContainerPointer output_data = output->GetPointData();

  PointDataContainer::ConstIterator o_data_it = output_data->Begin();
  PointDataContainer::ConstIterator o_data_end = output_data->End();

  PointsContainer::ConstIterator p_it = points->Begin();

  p.Fill(0.);

  bool error = false;

  while (o_data_it != o_data_end)
  {
    PixelType expected_value = p.EuclideanDistanceTo(p_it->Value());

    if ((o_data_it->Value() - expected_value) > 5. * expected_value / 100.)
    {
      std::cout << "** k = " << k << std::endl;
      std::cout << o_data_it->Value() << " != " << expected_value << std::endl;
      error = true;
    }
    ++p_it;
    ++o_data_it;
  }

  using WriterType = itk::MeshFileWriter<MeshType>;
  auto writer = WriterType::New();
  writer->SetInput(output);
  writer->SetFileName("itkFastMarchingQuadEdgeMeshFilterBaseTest4.vtk");
  writer->Update();

  if (error)
  {
    return EXIT_FAILURE;
  }
  else
  {
    return EXIT_SUCCESS;
  }
}
