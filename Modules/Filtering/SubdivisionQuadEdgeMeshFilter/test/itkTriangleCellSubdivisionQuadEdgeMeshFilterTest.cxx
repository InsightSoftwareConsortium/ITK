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

template <typename TTriangleCellSubdivisionFilter>
int
TriangleCellSubdivisionQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  using TriangleCellSubdivisionFilterType = TTriangleCellSubdivisionFilter;

  using InputMeshType = typename TriangleCellSubdivisionFilterType::InputMeshType;
  using OutputMeshType = typename TriangleCellSubdivisionFilterType::OutputMeshType;

  using IterativeTriangleCellSubdivisionFilterType =
    itk::IterativeTriangleCellSubdivisionQuadEdgeMeshFilter<InputMeshType, TriangleCellSubdivisionFilterType>;
  using IterativeTriangleCellSubdivisionFilterPointer = typename IterativeTriangleCellSubdivisionFilterType::Pointer;

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

  IterativeTriangleCellSubdivisionFilterPointer subdivision = IterativeTriangleCellSubdivisionFilterType::New();

  if (argc >= 5)
  {
    unsigned int n = std::atoi(argv[4]);
    subdivision->SetResolutionLevels(n);
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
    }
  }

  subdivision->SetInput(reader->GetOutput());
  subdivision->Update();
  typename OutputMeshType::Pointer output = subdivision->GetOutput();

  bool smoothing = true;
  if (argc >= 7)
  {
    smoothing = false;
  }

  if (smoothing)
  {
    using OutputMeshSmoothingFilterType = itk::SmoothingQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
    using OnesMatrixCoefficientsType = itk::OnesMatrixCoefficients<OutputMeshType>;

    OnesMatrixCoefficientsType                      coef;
    typename OutputMeshSmoothingFilterType::Pointer meshSmoothingFilter = OutputMeshSmoothingFilterType::New();
    meshSmoothingFilter->SetInput(output);
    meshSmoothingFilter->SetCoefficientsMethod(&coef);
    // FIXME: Smoothing with the Delaunay Conforming filter causes the following three test failures.
    //        Temporarily disabling the DC smoothing filter while investigating the cause.
    //  3 - itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilterTest1 (SEGFAULT)
    //  8 - itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilterTest0 (SEGFAULT)
    //  9 - itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilterTest1 (SEGFAULT)
    //    meshSmoothingFilter->SetDelaunayConforming(1);
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
itkTriangleCellSubdivisionQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputMeshFile  outputMeshFile subdivisionType Resolution non-uniform" << std::endl;
    std::cerr << " 0 : ModifiedButterfly " << std::endl;
    std::cerr << " 1 : Linear " << std::endl;
    std::cerr << " 2 : Loop " << std::endl;
    std::cerr << " 3 : Squarethree " << std::endl;
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
        return TriangleCellSubdivisionQuadEdgeMeshFilterTest<ModifiedButterflySubdivisionFilterType>(argc, argv);
      case 1:
        return TriangleCellSubdivisionQuadEdgeMeshFilterTest<LinearSubdivisionFilterType>(argc, argv);
      case 2:
        return TriangleCellSubdivisionQuadEdgeMeshFilterTest<LoopSubdivisionFilterType>(argc, argv);
      case 3:
        return TriangleCellSubdivisionQuadEdgeMeshFilterTest<SquareThreeSubdivisionFilterType>(argc, argv);
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
