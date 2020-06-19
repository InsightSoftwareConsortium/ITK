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

#include "itkModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkLinearTriangleEdgeCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkLoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkConditionalSubdivisionQuadEdgeMeshFilter.h"
#include "itkEdgeLengthTriangleEdgeCellSubdivisionCriterion.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"
#include "itkSmoothingQuadEdgeMeshFilter.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkTestingMacros.h"

template <typename TTriangleEdgeCellSubdivisionFilter>
int
CriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest(int argc, char * argv[])
{

  using TriangleEdgeCellSubdivisionFilterType = TTriangleEdgeCellSubdivisionFilter;
  using InputMeshType = typename TriangleEdgeCellSubdivisionFilterType::InputMeshType;
  using OutputMeshType = typename TriangleEdgeCellSubdivisionFilterType::OutputMeshType;

  using CriterionType = itk::EdgeLengthTriangleEdgeCellSubdivisionCriterion<
    typename TriangleEdgeCellSubdivisionFilterType::SubdivisionFilterType>;
  using ReaderType = itk::MeshFileReader<InputMeshType>;
  using WriterType = itk::MeshFileWriter<OutputMeshType>;

  const auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  const auto subdivision = TriangleEdgeCellSubdivisionFilterType::New();
  const auto criterion = CriterionType::New();
  ITK_TEST_EXPECT_EQUAL(criterion->GetNameOfClass(), std::string("EdgeLengthTriangleEdgeCellSubdivisionCriterion"));
  criterion->SetMaximumLength(1.0);
  ITK_TEST_SET_GET_VALUE(1.0, criterion->GetMaximumLength());
  if (argc >= 5)
  {
    float length = std::atof(argv[4]);
    criterion->SetMaximumLength(length);
    ITK_TEST_SET_GET_VALUE(length, criterion->GetMaximumLength());
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
itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputMeshFile  outputMeshFile subdivisionType area" << std::endl;
    std::cerr << " 0 : ModifiedButterfly " << std::endl;
    std::cerr << " 1 : Linear " << std::endl;
    std::cerr << " 2 : Loop " << std::endl;
    return EXIT_FAILURE;
  }

  using MeshPixelType = float;
  constexpr unsigned int Dimension = 3;

  using InputMeshType = itk::QuadEdgeMesh<MeshPixelType, Dimension>;
  using OutputMeshType = itk::QuadEdgeMesh<MeshPixelType, Dimension>;

  using ButterflySubdivisionFilterType =
    itk::ModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
  using LinearSubdivisionFilterType =
    itk::LinearTriangleEdgeCellSubdivisionQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
  using LoopSubdivisionFilterType =
    itk::LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;

  using ConditionalButterflySubdivisionFilterType =
    itk::ConditionalSubdivisionQuadEdgeMeshFilter<InputMeshType, ButterflySubdivisionFilterType>;
  using ConditionalLinearSubdivisionFilterType =
    itk::ConditionalSubdivisionQuadEdgeMeshFilter<InputMeshType, LinearSubdivisionFilterType>;
  using ConditionalLoopSubdivisionFilterType =
    itk::ConditionalSubdivisionQuadEdgeMeshFilter<InputMeshType, LoopSubdivisionFilterType>;

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
        return CriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest<ConditionalButterflySubdivisionFilterType>(
          argc, argv);
      }
      case 1:
      {
        const auto filter = ConditionalLinearSubdivisionFilterType::New();
        ITK_EXERCISE_BASIC_OBJECT_METHODS(
          filter, ConditionalSubdivisionQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);
        return CriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest<ConditionalLinearSubdivisionFilterType>(argc,
                                                                                                                  argv);
      }
      case 2:
      {
        const auto filter = ConditionalLoopSubdivisionFilterType::New();
        ITK_EXERCISE_BASIC_OBJECT_METHODS(
          filter, ConditionalSubdivisionQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);
        return CriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest<ConditionalLoopSubdivisionFilterType>(argc,
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
