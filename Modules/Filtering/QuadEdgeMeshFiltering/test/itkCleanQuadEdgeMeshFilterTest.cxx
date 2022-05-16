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
#include "itkMath.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"


#include "itkCleanQuadEdgeMeshFilter.h"
#include "itkTestingMacros.h"


int
itkCleanQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  // ** ERROR MESSAGE AND HELP ** //
  if (argc != 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputFilename relativeTolerance outputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  // ** TYPEDEF **
  using Coord = double;
  constexpr unsigned int Dimension = 3;

  using MeshType = itk::QuadEdgeMesh<Coord, Dimension>;
  using ReaderType = itk::MeshFileReader<MeshType>;
  using WriterType = itk::MeshFileWriter<MeshType>;

  // ** READ THE FILE IN **
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  MeshType::Pointer mesh = reader->GetOutput();

  Coord             tol;
  std::stringstream ssout(argv[2]);
  ssout >> tol;

  using CleanFilterType = itk::CleanQuadEdgeMeshFilter<MeshType, MeshType>;
  auto filter = CleanFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, CleanQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);


  filter->SetInput(mesh);

  auto absTol = itk::NumericTraits<typename CleanFilterType::InputCoordRepType>::ZeroValue();
  filter->SetAbsoluteTolerance(absTol);
  ITK_TEST_SET_GET_VALUE(absTol, filter->GetAbsoluteTolerance());

  filter->SetRelativeTolerance(tol);
  const Coord epsilon = 1e-6;
  Coord       obtainedValue = filter->GetRelativeTolerance();
  if (!itk::Math::FloatAlmostEqual(tol, obtainedValue, 10, epsilon))
  {
    std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in pixel GetRelativeTolerance" << std::endl;
    std::cerr << "Expected value " << tol << std::endl;
    std::cerr << " differs from " << obtainedValue;
    std::cerr << " by more than " << epsilon << std::endl;
    return EXIT_FAILURE;
  }

  filter->Update();

  // ** WRITE OUTPUT **
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[3]);
  writer->Update();


  return EXIT_SUCCESS;
}
