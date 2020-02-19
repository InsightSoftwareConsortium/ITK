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
#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"
#include "itkSmoothingQuadEdgeMeshFilter.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"

template <typename TTriangleEdgeCellSubdivisionFilter>
int
TriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest(int argc, char * argv[])
{

  using TriangleEdgeCellSubdivisionFilterType = TTriangleEdgeCellSubdivisionFilter;
  using TriangleEdgeCellSubdivisionFilterPointer = typename TriangleEdgeCellSubdivisionFilterType::Pointer;
  using InputMeshType = typename TriangleEdgeCellSubdivisionFilterType::InputMeshType;
  using OutputMeshType = typename TriangleEdgeCellSubdivisionFilterType::OutputMeshType;
  using SubdivisionCellContainer = typename TriangleEdgeCellSubdivisionFilterType::SubdivisionCellContainer;

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

  TriangleEdgeCellSubdivisionFilterPointer subdivision = TriangleEdgeCellSubdivisionFilterType::New();

  double edgeLengthThreshold = 1.0;
  if (argc >= 5)
  {
    edgeLengthThreshold = std::atof(argv[4]);
  }

  SubdivisionCellContainer                             edgesToBeSubdivided;
  typename InputMeshType::PointType                    pointArray[2];
  typename InputMeshType::ConstPointer                 input = reader->GetOutput();
  typename InputMeshType::CellsContainer::ConstPointer edges = input->GetEdgeCells();
  for (typename InputMeshType::CellsContainer::ConstIterator eter = edges->Begin(); eter != edges->End(); ++eter)
  {
    auto * edge = dynamic_cast<typename InputMeshType::EdgeCellType *>(eter.Value());
    if (edge)
    {
      input->GetPoint(edge->PointIdsBegin()[0], &pointArray[0]);
      input->GetPoint(edge->PointIdsBegin()[1], &pointArray[1]);
      if (static_cast<double>(pointArray[1].SquaredEuclideanDistanceTo(pointArray[0])) > edgeLengthThreshold)
      {
        std::cout << "to be subdivided edge id = " << eter->Index() << std::endl;
        edgesToBeSubdivided.push_back(input->FindEdge(edge->PointIdsBegin()[0], edge->PointIdsBegin()[1]));
      }
    }
  }

  std::cout << "number of subdivided edges = " << edgesToBeSubdivided.size() << std::endl;
  subdivision->SetCellsToBeSubdivided(edgesToBeSubdivided);
  subdivision->SetInput(reader->GetOutput());
  subdivision->Update();

  bool smoothing = true;
  if (argc >= 6)
  {
    smoothing = false;
  }

  if (smoothing)
  {
    using OutputMeshSmoothingFilterType = itk::SmoothingQuadEdgeMeshFilter<OutputMeshType, OutputMeshType>;
    using OnesMatrixCoefficientsType = itk::OnesMatrixCoefficients<OutputMeshType>;

    OnesMatrixCoefficientsType                      coef;
    typename OutputMeshSmoothingFilterType::Pointer meshSmoothingFilter = OutputMeshSmoothingFilterType::New();
    meshSmoothingFilter->SetInput(subdivision->GetOutput());
    meshSmoothingFilter->SetCoefficientsMethod(&coef);
    meshSmoothingFilter->SetDelaunayConforming(1);
    meshSmoothingFilter->SetNumberOfIterations(1);
    meshSmoothingFilter->Update();

    subdivision->GetOutput()->Graft(meshSmoothingFilter->GetOutput());
  }

  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(subdivision->GetOutput());

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
itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputMeshFile  outputMeshFile subdivisionType edgeLengthThreshold" << std::endl;
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
    itk::ModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter<InputMeshType, OutputMeshType>;
  using LinearSubdivisionFilterType =
    itk::LinearTriangleEdgeCellSubdivisionQuadEdgeMeshFilter<InputMeshType, OutputMeshType>;
  using LoopSubdivisionFilterType =
    itk::LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter<InputMeshType, OutputMeshType>;

  if (argc >= 4)
  {
    int type = std::atoi(argv[3]);

    switch (type)
    {
      case 0:
        return TriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest<ButterflySubdivisionFilterType>(argc, argv);
      case 1:
        return TriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest<LinearSubdivisionFilterType>(argc, argv);
      case 2:
        return TriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest<LoopSubdivisionFilterType>(argc, argv);
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
