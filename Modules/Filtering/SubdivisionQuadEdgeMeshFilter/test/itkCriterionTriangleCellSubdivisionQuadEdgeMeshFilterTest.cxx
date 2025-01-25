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

#include "itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkLinearTriangleCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkLoopTriangleCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkConditionalSubdivisionQuadEdgeMeshFilter.h"
#include "itkCellAreaTriangleCellSubdivisionCriterion.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"
#include "itkSmoothingQuadEdgeMeshFilter.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkTestingMacros.h"

template <typename TTriangleCellSubdivisionFilter>
int
CriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest(int argc, char * argv[])
{

  using TriangleCellSubdivisionFilterType = TTriangleCellSubdivisionFilter;
  using InputMeshType = typename TriangleCellSubdivisionFilterType::InputMeshType;
  using OutputMeshType = typename TriangleCellSubdivisionFilterType::OutputMeshType;

  using CriterionType =
    itk::CellAreaTriangleCellSubdivisionCriterion<typename TriangleCellSubdivisionFilterType::SubdivisionFilterType>;
  using ReaderType = itk::MeshFileReader<InputMeshType>;
  using WriterType = itk::MeshFileWriter<OutputMeshType>;

  const auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  const auto subdivision = TriangleCellSubdivisionFilterType::New();
  const auto criterion = CriterionType::New();
  ITK_TEST_EXPECT_EQUAL(criterion->GetNameOfClass(), std::string("CellAreaTriangleCellSubdivisionCriterion"));
  criterion->SetMaximumArea(1.0);
  ITK_TEST_SET_GET_VALUE(1.0, criterion->GetMaximumArea());
  if (argc >= 5)
  {
    float area = std::atof(argv[4]);
    criterion->SetMaximumArea(area);
    ITK_TEST_SET_GET_VALUE(area, criterion->GetMaximumArea());
  }

  subdivision->SetSubdivisionCriterion(criterion.GetPointer());
  subdivision->SetInput(reader->GetOutput());
  subdivision->Update();
  const auto output = subdivision->GetOutput();

  bool smoothing = false;
  if (argc >= 6)
  {
    smoothing = true;
  }

  if (smoothing)
  {
    using OutputMeshSmoothingFilterType = itk::SmoothingQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
    using OnesMatrixCoefficientsType = itk::OnesMatrixCoefficients<OutputMeshType>;

    OnesMatrixCoefficientsType coef;
    const auto                 meshSmoothingFilter = OutputMeshSmoothingFilterType::New();
    meshSmoothingFilter->SetInput(output);
    meshSmoothingFilter->SetCoefficientsMethod(&coef);
    meshSmoothingFilter->SetDelaunayConforming(1);
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
itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputMeshFile  outputMeshFile subdivisionType area" << std::endl;
    std::cerr << " 0 : ModifiedButterfly " << std::endl;
    std::cerr << " 1 : Linear " << std::endl;
    std::cerr << " 2 : Loop " << std::endl;
    std::cerr << " 3 : Squarethree " << std::endl;
    return EXIT_FAILURE;
  }

  using MeshPixelType = float;
  constexpr unsigned int Dimension = 3;

  using InputMeshType = itk::QuadEdgeMesh<MeshPixelType, Dimension>;
  using OutputMeshType = itk::QuadEdgeMesh<MeshPixelType, Dimension>;

  using ButterflySubdivisionFilterType =
    itk::ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
  using LinearSubdivisionFilterType =
    itk::LinearTriangleCellSubdivisionQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
  using LoopSubdivisionFilterType = itk::LoopTriangleCellSubdivisionQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
  using SquareSubdivisionFilterType =
    itk::SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;

  using ConditionalButterflySubdivisionFilterType =
    itk::ConditionalSubdivisionQuadEdgeMeshFilter<InputMeshType, ButterflySubdivisionFilterType>;
  using ConditionalLinearSubdivisionFilterType =
    itk::ConditionalSubdivisionQuadEdgeMeshFilter<InputMeshType, LinearSubdivisionFilterType>;
  using ConditionalLoopSubdivisionFilterType =
    itk::ConditionalSubdivisionQuadEdgeMeshFilter<InputMeshType, LoopSubdivisionFilterType>;
  using ConditionalSquareSubdivisionFilterType =
    itk::ConditionalSubdivisionQuadEdgeMeshFilter<InputMeshType, SquareSubdivisionFilterType>;

  if (argc >= 4)
  {
    int type = std::atoi(argv[3]);

    switch (type)
    {
      case 0:
      {
        const auto filter = ConditionalButterflySubdivisionFilterType::New();
        ITK_EXERCISE_BASIC_OBJECT_METHODS(
          filter, ConditionalSubdivisionQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);
        return CriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest<ConditionalButterflySubdivisionFilterType>(argc,
                                                                                                                 argv);
      }
      case 1:
      {
        const auto filter = ConditionalLinearSubdivisionFilterType::New();
        ITK_EXERCISE_BASIC_OBJECT_METHODS(
          filter, ConditionalSubdivisionQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);
        return CriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest<ConditionalLinearSubdivisionFilterType>(argc,
                                                                                                              argv);
      }
      case 2:
      {
        const auto filter = ConditionalLoopSubdivisionFilterType::New();
        ITK_EXERCISE_BASIC_OBJECT_METHODS(
          filter, ConditionalSubdivisionQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);
        return CriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest<ConditionalLoopSubdivisionFilterType>(argc, argv);
      }
      case 3:
      {
        const auto filter = ConditionalSquareSubdivisionFilterType::New();
        ITK_EXERCISE_BASIC_OBJECT_METHODS(
          filter, ConditionalSubdivisionQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);
        return CriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest<ConditionalSquareSubdivisionFilterType>(argc,
                                                                                                              argv);
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
