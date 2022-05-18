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
#include "itkTestingMacros.h"
#include "itkQuadEdgeMesh.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"


#include "itkQuadEdgeMeshDecimationCriteria.h"
#include "itkSquaredEdgeLengthDecimationQuadEdgeMeshFilter.h"

int
itkSquaredEdgeLengthDecimationQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  // ** ERROR MESSAGE AND HELP ** //
  if (argc != 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputFilename numberOfFaces outputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  // ** TYPEDEF **
  using CoordType = double;
  constexpr unsigned int Dimension = 3;

  using MeshType = itk::QuadEdgeMesh<CoordType, Dimension>;
  using ReaderType = itk::MeshFileReader<MeshType>;
  using WriterType = itk::MeshFileWriter<MeshType>;

  // ** READ THE FILE IN **
  const auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  const auto mesh = reader->GetOutput();

  for (auto it = mesh->GetCells()->Begin(); it != mesh->GetCells()->End(); ++it)
  {
    mesh->SetCellData(it.Index(), 25);
  }
  ITK_TEST_EXPECT_EQUAL(mesh->GetNumberOfCells(), mesh->GetCellData()->Size());

  using CriterionType = itk::NumberOfFacesCriterion<MeshType>;
  using DecimationType = itk::SquaredEdgeLengthDecimationQuadEdgeMeshFilter<MeshType, MeshType, CriterionType>;

  long              N;
  std::stringstream ssout(argv[2]);
  ssout >> N;

  std::array<bool, 2> topological_change;
  topological_change[0] = true;
  topological_change[1] = false;

  for (const auto & tc : topological_change)
  {
    const auto criterion = CriterionType::New();
    ITK_TEST_SET_GET_BOOLEAN(criterion, TopologicalChange, tc);
    criterion->SetNumberOfElements(N);

    ITK_EXERCISE_BASIC_OBJECT_METHODS(criterion, NumberOfFacesCriterion, QuadEdgeMeshDecimationCriterion);

    const auto decimate = DecimationType::New();
    decimate->SetInput(mesh);
    decimate->SetCriterion(criterion);
    ITK_TRY_EXPECT_NO_EXCEPTION(decimate->Update());

    ITK_EXERCISE_BASIC_OBJECT_METHODS(
      decimate, SquaredEdgeLengthDecimationQuadEdgeMeshFilter, EdgeDecimationQuadEdgeMeshFilter);

    ITK_TEST_EXPECT_EQUAL(decimate->GetOutput()->GetNumberOfCells(), decimate->GetOutput()->GetCellData()->Size());

    // ** WRITE OUTPUT **
    const auto writer = WriterType::New();
    writer->SetInput(decimate->GetOutput());
    writer->SetFileName(argv[3]);
    ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());
  }

  return EXIT_SUCCESS;
}
