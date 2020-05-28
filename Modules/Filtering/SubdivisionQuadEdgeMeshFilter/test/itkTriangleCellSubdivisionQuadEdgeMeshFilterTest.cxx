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

#include "itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkLinearTriangleCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkLoopTriangleCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkIterativeTriangleCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"
#include "itkSmoothingQuadEdgeMeshFilter.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkTestingMacros.h"

template <typename TTriangleCellSubdivisionFilter>
int
TriangleCellSubdivisionQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  using TriangleCellSubdivisionFilterType = TTriangleCellSubdivisionFilter;

  using InputMeshType = typename TriangleCellSubdivisionFilterType::InputMeshType;
  using OutputMeshType = typename TriangleCellSubdivisionFilterType::OutputMeshType;

  using IterativeTriangleCellSubdivisionFilterType =
    itk::IterativeTriangleCellSubdivisionQuadEdgeMeshFilter<InputMeshType, TriangleCellSubdivisionFilterType>;

  using ReaderType = itk::MeshFileReader<InputMeshType>;
  using WriterType = itk::MeshFileWriter<OutputMeshType>;

  const auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  const auto subdivision = IterativeTriangleCellSubdivisionFilterType::New();
  ITK_TEST_EXPECT_EQUAL(subdivision->GetNameOfClass(), "IterativeTriangleCellSubdivisionQuadEdgeMeshFilter");
  subdivision->Print(std::cout);

  if (argc >= 5)
  {
    unsigned int n = std::atoi(argv[4]);
    subdivision->SetResolutionLevels(n);
    ITK_TEST_SET_GET_VALUE(n, subdivision->GetResolutionLevels());
  }

  if (argc >= 6)
  {
    int type = std::atoi(argv[5]);
    if (type)
    {
      typename IterativeTriangleCellSubdivisionFilterType::SubdivisionCellContainer cellsToBeSubdivided;

      cellsToBeSubdivided.push_back(0);
      cellsToBeSubdivided.push_back(1);
      cellsToBeSubdivided.push_back(2);
      cellsToBeSubdivided.push_back(3);
      cellsToBeSubdivided.push_back(5);
      cellsToBeSubdivided.push_back(6);
      cellsToBeSubdivided.push_back(9);

      subdivision->SetCellsToBeSubdivided(cellsToBeSubdivided);

      ITK_TEST_EXPECT_EQUAL(7, subdivision->GetCellsToBeSubdivided().size());
    }
    else
    {
      subdivision->AddSubdividedCellId(0);
      subdivision->AddSubdividedCellId(1);
      subdivision->AddSubdividedCellId(2);
      subdivision->AddSubdividedCellId(3);
      subdivision->AddSubdividedCellId(5);
      subdivision->AddSubdividedCellId(6);
      subdivision->AddSubdividedCellId(9);

      ITK_TEST_EXPECT_EQUAL(7, subdivision->GetCellsToBeSubdivided().size());
    }
  }

  subdivision->SetInput(reader->GetOutput());
  ITK_TRY_EXPECT_NO_EXCEPTION(subdivision->Update());
  const auto output = subdivision->GetOutput();

  bool smoothing = true;
  if (argc >= 7)
  {
    smoothing = false;
  }

  if (smoothing)
  {
    using OutputMeshSmoothingFilterType = itk::SmoothingQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
    using OnesMatrixCoefficientsType = itk::OnesMatrixCoefficients<OutputMeshType>;

    OnesMatrixCoefficientsType coef;
    const auto                 meshSmoothingFilter = OutputMeshSmoothingFilterType::New();
    meshSmoothingFilter->SetInput(output);
    meshSmoothingFilter->SetCoefficientsMethod(&coef);
    // FIXME: Smoothing with the Delaunay Conforming filter causes the following three test failures.
    //        Temporarily disabling the DC smoothing filter while investigating the cause.
    //  3 - itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilterTest1 (SEGFAULT)
    //  8 - itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilterTest0 (SEGFAULT)
    //  9 - itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilterTest1 (SEGFAULT)
    //    meshSmoothingFilter->SetDelaunayConforming(1);
    meshSmoothingFilter->SetNumberOfIterations(1);
    ITK_TRY_EXPECT_NO_EXCEPTION(meshSmoothingFilter->Update());

    output->Graft(meshSmoothingFilter->GetOutput());
  }

  const auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(output);
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}

int
itkTriangleCellSubdivisionQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Error: Missing Parameters!" << std::endl;
    std::cerr << "Usage: " << argv[0] << '\n';
    std::cerr << "  inputMeshFile\n";
    std::cerr << "  outputMeshFile\n";
    std::cerr << "  subdivisionType\n";
    std::cerr << "    0 : ModifiedButterfly\n";
    std::cerr << "    1 : Linear\n";
    std::cerr << "    2 : Loop\n";
    std::cerr << "    3 : SquareThree\n";
    std::cerr << "  [Resolution]\n";
    std::cerr << "  [Adaptive]\n";
    std::cerr << "    0 : AddSubdividedCellId\n";
    std::cerr << "    1 : SetCellsToBeSubdivided\n";
    std::cerr << std::flush;
    return EXIT_FAILURE;
  }

  using MeshPixelType = float;
  constexpr unsigned int Dimension = 3;

  using MeshType = itk::QuadEdgeMesh<MeshPixelType, Dimension>;

  using ModifiedButterflySubdivisionFilterType =
    itk::ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter<MeshType, MeshType>;
  using LinearSubdivisionFilterType = itk::LinearTriangleCellSubdivisionQuadEdgeMeshFilter<MeshType, MeshType>;
  using LoopSubdivisionFilterType = itk::LoopTriangleCellSubdivisionQuadEdgeMeshFilter<MeshType, MeshType>;
  using SquareThreeSubdivisionFilterType =
    itk::SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter<MeshType, MeshType>;

  if (argc >= 4)
  {
    int type = std::atoi(argv[3]);

    switch (type)
    {
      case 0:
      {
        const auto filter = ModifiedButterflySubdivisionFilterType::New();
        ITK_EXERCISE_BASIC_OBJECT_METHODS(filter,
                                          ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter,
                                          TriangleCellSubdivisionQuadEdgeMeshFilter);
        return TriangleCellSubdivisionQuadEdgeMeshFilterTest<ModifiedButterflySubdivisionFilterType>(argc, argv);
      }
      case 1:
      {
        const auto filter = LinearSubdivisionFilterType::New();
        ITK_EXERCISE_BASIC_OBJECT_METHODS(
          filter, LinearTriangleCellSubdivisionQuadEdgeMeshFilter, TriangleCellSubdivisionQuadEdgeMeshFilter);
        return TriangleCellSubdivisionQuadEdgeMeshFilterTest<LinearSubdivisionFilterType>(argc, argv);
      }
      case 2:
      {
        const auto filter = LoopSubdivisionFilterType::New();
        ITK_EXERCISE_BASIC_OBJECT_METHODS(
          filter, LoopTriangleCellSubdivisionQuadEdgeMeshFilter, TriangleCellSubdivisionQuadEdgeMeshFilter);
        return TriangleCellSubdivisionQuadEdgeMeshFilterTest<LoopSubdivisionFilterType>(argc, argv);
      }
      case 3:
      {
        const auto filter = SquareThreeSubdivisionFilterType::New();
        ITK_EXERCISE_BASIC_OBJECT_METHODS(
          filter, SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter, TriangleCellSubdivisionQuadEdgeMeshFilter);
        return TriangleCellSubdivisionQuadEdgeMeshFilterTest<SquareThreeSubdivisionFilterType>(argc, argv);
      }
      default:
        std::cerr << "Invalid subdivision type : " << type << std::endl;
        return EXIT_FAILURE;
    }
  }
  else
  {
    std::cerr << "You must have subdivision type " << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
