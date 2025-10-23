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

#include "itkPointFeature.h"
#include "itkPointSet.h"
#include "itkTestingMacros.h"
#include "itkMesh.h"
#include "itkVector.h"
#include "itkMeshFileReader.h"


namespace
{
constexpr unsigned int Dimension = 3;
using CoordType = double;
using VectorType = itk::Vector<CoordType, Dimension>;
using MeshType = itk::Mesh<CoordType, Dimension, itk::DefaultStaticMeshTraits<VectorType, Dimension>>;
constexpr unsigned numDebugPoints = 10;

MeshType::Pointer
ReadMesh(std::string filename)
{
  using ReaderType = itk::MeshFileReader<MeshType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(filename);
  reader->Update();
  MeshType::Pointer output = reader->GetOutput();
  output->DisconnectPipeline();
  return output;
}

int
BasicTests()
{
  using PixelType = float;
  using PointSetType = itk::PointSet<PixelType, Dimension>;

  using FilterType = itk::PointFeature<PointSetType, PointSetType>;
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, PointFeature, MeshToMeshFilter);

  std::cout << "Basic tests finished." << std::endl;

  return EXIT_SUCCESS;
}
} // namespace

int
itkPointFeatureTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputMesh";
    std::cerr << std::endl;
    std::cout << "Running basic tests only." << std::endl;
    return BasicTests();
  }

  MeshType::Pointer points = ReadMesh(argv[1]);
  MeshType::Pointer normals = ReadMesh(argv[1]);

  MeshType::PointsContainerPointer  normal_points = normals->GetPoints();
  MeshType::PointsContainerIterator p_it = normal_points->Begin();

  MeshType::PointDataContainerPointer  normal_normals = normals->GetPointData();
  MeshType::PointDataContainerIterator d_it = normal_normals->Begin();

  // Print first 10 points and normals for sanity check and debugging
  std::cout << "Index * Point * Normal (up to first 10)" << std::endl;
  unsigned i = 0;
  while (p_it != normal_points->End())
  {
    if (i < numDebugPoints) // debug
    {
      std::cout << p_it.Index() << " * ";
      std::cout << p_it.Value() << " * ";
      std::cout << d_it.Value() << std::endl;
    }

    for (unsigned d = 0; d < Dimension; ++d)
    {
      p_it.Value()[d] = d_it.Value()[d];
    }

    ++p_it;
    ++d_it;
    ++i;
  }

  // Code specific for this test
  using FilterType = itk::PointFeature<MeshType, MeshType>;
  FilterType::Pointer fpfh = FilterType::New();
  fpfh->ComputeFPFHFeature(points, normals, 2.4219912533797725, 100);
  auto result = fpfh->GetFpfhFeature();
  auto resultSTL = result->CastToSTLConstContainer();
  for (i = 0; i < numDebugPoints; ++i)
  {
    std::cout << "FPFH Feature for point " << i << ":";
    for (unsigned k = 0; k < 33; ++k)
      std::cout << ' ' << resultSTL[i * 33 + k];
    std::cout << std::endl;
  }

  return EXIT_SUCCESS;
}
