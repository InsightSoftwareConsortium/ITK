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
#include "itkConditionalSubdivisionQuadEdgeMeshFilter.h"
#include "itkCellAreaTriangleCellSubdivisionCriterion.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"
#include "itkSmoothingQuadEdgeMeshFilter.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"

template <typename TTriangleCellSubdivisionFilter>
int
CriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest(int argc, char * argv[])
{

  using TriangleCellSubdivisionFilterType = TTriangleCellSubdivisionFilter;
  using TriangleCellSubdivisionFilterPointer = typename TTriangleCellSubdivisionFilter::Pointer;
  using InputMeshType = typename TriangleCellSubdivisionFilterType::InputMeshType;
  using OutputMeshType = typename TriangleCellSubdivisionFilterType::OutputMeshType;

  using CriterionType =
    itk::CellAreaTriangleCellSubdivisionCriterion<typename TriangleCellSubdivisionFilterType::SubdivisionFilterType>;
  using CriterionPointer = typename CriterionType::Pointer;
  using ReaderType = itk::MeshFileReader<InputMeshType>;
  using WriterType = itk::MeshFileWriter<OutputMeshType>;

  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  try
  {
    reader->Update();
  }
  catch (itk::ExceptionObject & exp)
  {
    std::cerr << "Exception thrown while reading the input file " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
  }

  TriangleCellSubdivisionFilterPointer subdivision = TriangleCellSubdivisionFilterType::New();

  CriterionPointer criterion = CriterionType::New();
  criterion->SetMaximumArea(1.0);
  if (argc >= 5)
  {
    float area = std::atof(argv[4]);
    criterion->SetMaximumArea(area);
  }

  subdivision->SetSubdivisionCriterion(criterion.GetPointer());
  subdivision->SetInput(reader->GetOutput());
  subdivision->Update();
  typename OutputMeshType::Pointer output = subdivision->GetOutput();

  bool smoothing = false;
  if (argc >= 6)
  {
    smoothing = true;
  }

  if (smoothing)
  {
    using OutputMeshSmoothingFilterType = itk::SmoothingQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
    using OnesMatrixCoefficientsType = itk::OnesMatrixCoefficients<OutputMeshType>;

    OnesMatrixCoefficientsType                      coef;
    typename OutputMeshSmoothingFilterType::Pointer meshSmoothingFilter = OutputMeshSmoothingFilterType::New();
    meshSmoothingFilter->SetInput(output);
    meshSmoothingFilter->SetCoefficientsMethod(&coef);
    meshSmoothingFilter->SetDelaunayConforming(1);
    meshSmoothingFilter->SetNumberOfIterations(1);
    meshSmoothingFilter->Update();

    output->Graft(meshSmoothingFilter->GetOutput());
  }

  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(output);

  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & exp)
  {
    std::cerr << "Exception thrown while writting the output file " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
  }

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

  using ModifiedButterflySubdivisionFilterType =
    itk::ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
  using LinearSubdivisionFilterType =
    itk::LinearTriangleCellSubdivisionQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
  using LoopSubdivisionFilterType = itk::LoopTriangleCellSubdivisionQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
  using SquareThreeSubdivisionFilterType =
    itk::SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;

  using ConditionalModifiedButterflySubdivisionFilterType =
    itk::ConditionalSubdivisionQuadEdgeMeshFilter<InputMeshType, ModifiedButterflySubdivisionFilterType>;
  using ConditionalLinearSubdivisionFilterType =
    itk::ConditionalSubdivisionQuadEdgeMeshFilter<InputMeshType, LinearSubdivisionFilterType>;
  using ConditionalLoopSubdivisionFilterType =
    itk::ConditionalSubdivisionQuadEdgeMeshFilter<InputMeshType, LoopSubdivisionFilterType>;
  using ConditionalSquareThreeSubdivisionFilterType =
    itk::ConditionalSubdivisionQuadEdgeMeshFilter<InputMeshType, SquareThreeSubdivisionFilterType>;

  if (argc >= 4)
  {
    int type = std::atoi(argv[3]);

    switch (type)
    {
      case 0:
        return CriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest<
          ConditionalModifiedButterflySubdivisionFilterType>(argc, argv);
      case 1:
        return CriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest<ConditionalLinearSubdivisionFilterType>(argc,
                                                                                                              argv);
      case 2:
        return CriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest<ConditionalLoopSubdivisionFilterType>(argc, argv);
      case 3:
        return CriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest<ConditionalSquareThreeSubdivisionFilterType>(
          argc, argv);
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
