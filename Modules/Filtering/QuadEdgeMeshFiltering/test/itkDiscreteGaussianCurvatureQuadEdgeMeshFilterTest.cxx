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
#include "itkQuadEdgeMesh.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"

#include "itkQuadEdgeMeshExtendedTraits.h"
#include "itkDiscreteGaussianCurvatureQuadEdgeMeshFilter.h"
#include "itkTestingMacros.h"

int
itkDiscreteGaussianCurvatureQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;
  using CoordType = double;

  using Traits = itk::QuadEdgeMeshExtendedTraits<CoordType, Dimension, 2, CoordType, CoordType, CoordType, bool, bool>;

  using MeshType = itk::QuadEdgeMesh<CoordType, Dimension, Traits>;
  using CurvatureFilterType = itk::DiscreteGaussianCurvatureQuadEdgeMeshFilter<MeshType, MeshType>;

  using ReaderType = itk::MeshFileReader<MeshType>;

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  MeshType::Pointer mesh = reader->GetOutput();

  auto gaussianCurvatureFilter = CurvatureFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    gaussianCurvatureFilter, DiscreteGaussianCurvatureQuadEdgeMeshFilter, DiscreteCurvatureQuadEdgeMeshFilter);


  gaussianCurvatureFilter->SetInput(mesh);
  gaussianCurvatureFilter->Update();

  MeshType::Pointer output = gaussianCurvatureFilter->GetOutput();

  using WriterType = itk::MeshFileWriter<MeshType>;
  auto writer = WriterType::New();
  writer->SetInput(output);
  writer->SetFileName("gaussian_curvature.vtk");
  writer->Update();


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
