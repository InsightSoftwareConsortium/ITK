/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"

#include "itkQuadEdgeMeshExtendedTraits.h"
#include "itkDiscreteMinimumCurvatureQuadEdgeMeshFilter.h"
#include "itkTestingMacros.h"

int
itkDiscreteMinimumCurvatureQuadEdgeMeshFilterTest(int argc, char * argv[])
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
  using CurvatureFilterType = itk::DiscreteMinimumCurvatureQuadEdgeMeshFilter<MeshType, MeshType>;

  using ReaderType = itk::MeshFileReader<MeshType>;

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown while reading the input file " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  MeshType::Pointer mesh = reader->GetOutput();

  auto min_curvature = CurvatureFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    min_curvature, DiscreteMinimumCurvatureQuadEdgeMeshFilter, DiscretePrincipalCurvaturesQuadEdgeMeshFilter);


  min_curvature->SetInput(mesh);
  min_curvature->Update();

  MeshType::Pointer output = min_curvature->GetOutput();

  using WriterType = itk::MeshFileWriter<MeshType>;
  auto writer = WriterType::New();
  writer->SetInput(output);
  writer->SetFileName("min_curvature.vtk");
  writer->Update();

  return EXIT_SUCCESS;
}
