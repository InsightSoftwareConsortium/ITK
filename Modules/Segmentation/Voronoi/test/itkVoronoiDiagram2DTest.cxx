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

#include "itkVoronoiDiagram2DGenerator.h"
#include "itkMeshSource.h"
#include "itkMeshFileWriter.h"
#include "itkTestingMacros.h"

int
itkVoronoiDiagram2DTest(int argc, char * argv[])
{

  if (argc != 2)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " outputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr double       height = 400;
  constexpr double       width = 400;
  constexpr unsigned int numberOfSeeds = 20;

  using VoronoiDiagram = itk::VoronoiDiagram2D<double>;
  using VoronoiDiagramGenerator = itk::VoronoiDiagram2DGenerator<double>;

  using PointType = VoronoiDiagram::PointType;
  using CellType = VoronoiDiagram::CellType;
  using CellAutoPointer = VoronoiDiagram::CellAutoPointer;
  using PointIdIterator = CellType::PointIdIterator;
  using NeighborIdIterator = VoronoiDiagram::NeighborIdIterator;

  auto voronoiDiagram = VoronoiDiagram::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(voronoiDiagram, VoronoiDiagram2D, Mesh);

  auto voronoiDiagramGenerator = VoronoiDiagramGenerator::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(voronoiDiagramGenerator, VoronoiDiagram2DGenerator, MeshSource);

  PointType insize;
  insize[0] = width;
  insize[1] = height;
  voronoiDiagramGenerator->SetBoundary(insize);

  voronoiDiagramGenerator->SetRandomSeeds(numberOfSeeds);
  ITK_TEST_SET_GET_VALUE(numberOfSeeds, voronoiDiagramGenerator->GetNumberOfSeeds());

  voronoiDiagramGenerator->Update();
  voronoiDiagram = voronoiDiagramGenerator->GetOutput();

  ITK_TEST_SET_GET_VALUE(numberOfSeeds, voronoiDiagram->GetNumberOfSeeds());

  for (unsigned int i = 0; i < voronoiDiagramGenerator->GetNumberOfSeeds(); ++i)
  {
    PointType currP = voronoiDiagram->GetSeed(i);
    std::cout << "Seed No." << i << ": At (" << currP[0] << "," << currP[1] << ")" << std::endl;
    std::cout << "Boundary Vertices List (in order): ";
    CellAutoPointer currCell;
    voronoiDiagram->GetCellId(i, currCell);
    PointIdIterator currCellP;
    for (currCellP = currCell->PointIdsBegin(); currCellP != currCell->PointIdsEnd(); ++currCellP)
    {
      std::cout << *currCellP << ",";
    }
    std::cout << std::endl;
    std::cout << " Neighbors (Seed No.): ";
    NeighborIdIterator currNeibor;
    for (currNeibor = voronoiDiagram->NeighborIdsBegin(i); currNeibor != voronoiDiagram->NeighborIdsEnd(i);
         ++currNeibor)
    {
      std::cout << *currNeibor << ",";
    }
    std::cout << std::endl << std::endl;
  }

  std::cout << "Vertices Informations:" << std::endl;
  VoronoiDiagram::VertexIterator allVerts;
  int                            j = 0;
  for (allVerts = voronoiDiagram->VertexBegin(); allVerts != voronoiDiagram->VertexEnd(); ++allVerts)
  {
    std::cout << "Vertices No. " << j;
    j++;
    std::cout << ": At (" << allVerts.Value()[0] << "," << allVerts.Value()[1] << ")" << std::endl;
  }

  using WriterType = itk::MeshFileWriter<VoronoiDiagram>;
  auto writer = WriterType::New();
  writer->SetInput(voronoiDiagram);
  writer->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
