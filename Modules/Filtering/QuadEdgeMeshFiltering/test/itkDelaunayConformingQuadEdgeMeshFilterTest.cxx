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
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkQuadEdgeMesh.h"
#include "itkNormalVariateGenerator.h"

// NEW
#include "itkDelaunayConformingQuadEdgeMeshFilter.h"

int
itkDelaunayConformingQuadEdgeMeshFilterTestHelper(const std::string & input,
                                                  const std::string & output,
                                                  const bool          cell_data)
{

  // ** TYPEDEF **
  using Coord = double;

  using MeshType = itk::QuadEdgeMesh<Coord, 3>;
  using ReaderType = itk::MeshFileReader<MeshType>;
  using DelaunayConformFilterType = itk::DelaunayConformingQuadEdgeMeshFilter<MeshType, MeshType>;
  using WriterType = itk::MeshFileWriter<MeshType>;

  // ** READ THE FILE IN **
  const auto reader = ReaderType::New();
  reader->SetFileName(input);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  MeshType::Pointer mesh = reader->GetOutput();

  using GeneratorType = itk::Statistics::NormalVariateGenerator;
  const auto generator = GeneratorType::New();
  generator->Initialize(0);

  const double sigma = 0.01;

  for (auto it = mesh->GetPoints()->Begin(); it != mesh->GetPoints()->End(); ++it)
  {
    for (size_t d = 0; d < MeshType::MeshTraits::PointDimension; ++d)
    {
      it.Value()[d] += (generator->GetVariate() * sigma);
    }
  }

  if (cell_data)
  {
    for (auto it = mesh->GetCells()->Begin(); it != mesh->GetCells()->End(); ++it)
    {
      mesh->SetCellData(it.Index(), it.Index());
    }
  }

  const auto filter = DelaunayConformFilterType::New();
  filter->SetInput(mesh);
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  if (cell_data)
  {
    for (auto it = mesh->GetCells()->Begin(); it != mesh->GetCells()->End(); ++it)
    {
      ITK_TEST_EXPECT_TRUE(mesh->GetCellData()->IndexExists(it.Index()));
    }
    ITK_TEST_EXPECT_EQUAL(mesh->GetNumberOfCells(), mesh->GetCellData()->Size());
  }

  // ** WRITE OUTPUT **
  const auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(output);
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Input: " << input << std::endl;
  std::cout << "Output: " << output << std::endl;
  std::cout << "Number of Edge flipped performed: " << filter->GetNumberOfEdgeFlips() << std::endl;

  // ** PRINT **
  std::cout << filter;

  return EXIT_SUCCESS;
}

int
itkDelaunayConformingQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  // ** ERROR MESSAGE AND HELP ** //
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputFilename outputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string input(argv[1]);
  const std::string output(argv[2]);

  if (EXIT_FAILURE == itkDelaunayConformingQuadEdgeMeshFilterTestHelper(input, output, true))
  {
    return EXIT_FAILURE;
  }
  if (EXIT_FAILURE == itkDelaunayConformingQuadEdgeMeshFilterTestHelper(input, output, false))
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
