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


#include "itkSmoothingQuadEdgeMeshFilter.h"

int
itkSmoothingQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  // ** ERROR MESSAGE AND HELP ** //
  if (argc != 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputFilename numberOfIterations relaxationFactor useDelaunayConformingFilter outputFilename"
              << std::endl;
    return EXIT_FAILURE;
  }

  // ** TYPEDEF **
  using Coord = float;
  constexpr unsigned int Dimension = 3;

  using MeshType = itk::QuadEdgeMesh<Coord, Dimension>;
  using ReaderType = itk::MeshFileReader<MeshType>;
  using WriterType = itk::MeshFileWriter<MeshType>;

  // ** READ THE FILE IN **
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  unsigned int      nb_iter;
  std::stringstream ssout(argv[2]);
  ssout >> nb_iter;

  double            relaxation_factor;
  std::stringstream ssout2(argv[3]);
  ssout2 >> relaxation_factor;

  bool              del_conf;
  std::stringstream ssout3(argv[4]);
  ssout3 >> del_conf;

  const auto mesh = reader->GetOutput();

  itk::OnesMatrixCoefficients<MeshType> coeff0;

  using SmoothingType = itk::SmoothingQuadEdgeMeshFilter<MeshType, MeshType>;
  const auto filter = SmoothingType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, SmoothingQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

  filter->SetInput(mesh);
  filter->SetNumberOfIterations(nb_iter);
  ITK_TEST_SET_GET_VALUE(nb_iter, filter->GetNumberOfIterations());
  filter->SetRelaxationFactor(relaxation_factor);
  ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(relaxation_factor, filter->GetRelaxationFactor()));
  filter->SetDelaunayConforming(del_conf);
  ITK_TEST_SET_GET_BOOLEAN(filter, DelaunayConforming, del_conf);
  filter->SetCoefficientsMethod(&coeff0);
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // ** WRITE OUTPUT **
  const auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[5]);
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
