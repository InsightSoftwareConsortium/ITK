/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkVoronoiDiagram2DGenerator.h"
#include "itkMeshFileWriter.h"


int itkVoronoiDiagram2DTest(int argc, char* argv[] ){

  if( argc != 2 )
    {
    std::cerr << "Usage: itkVoronoiDiagram2DTest outputFileName" << std::endl;
    return EXIT_FAILURE;
    }

  const double HEI=400;
  const double WID=400;
  const int NUMSEEDS=20;

  typedef itk::VoronoiDiagram2D<double>          Vor;
  typedef itk::VoronoiDiagram2DGenerator<double> VorGenerator;

  typedef Vor::PointType            PointType;
  typedef Vor::CellType             CellType;
  typedef Vor::CellAutoPointer      CellAutoPointer;
  typedef CellType::PointIdIterator PointIdIterator;
  typedef Vor::NeighborIdIterator   NeighborIdIterator;

  Vor::Pointer testVor(Vor::New());
  VorGenerator::Pointer testVorGen(VorGenerator::New());

  PointType insize;
  insize[0]=WID;
  insize[1]=HEI;
  testVorGen->SetBoundary(insize);

  testVorGen->SetRandomSeeds(NUMSEEDS);
  testVorGen->Update();
  testVor=testVorGen->GetOutput();

  for( int i = 0; i < NUMSEEDS; i++ ){
    PointType currP=testVor->GetSeed(i);
    std::cout<<"Seed No."<<i<<": At ("<<currP[0]<<"," <<currP[1]<<")"<<std::endl;
    std::cout<<"  Boundary Vertices List (in order):";
    CellAutoPointer currCell;
    testVor->GetCellId(i, currCell);
    PointIdIterator currCellP;
    for( currCellP = currCell->PointIdsBegin(); currCellP != currCell->PointIdsEnd(); ++currCellP )
    {
      std::cout<<(*currCellP)<<",";
    }
    std::cout<<std::endl;
    std::cout<<"  Neighbors (Seed No.):";
    NeighborIdIterator currNeibor;
    for( currNeibor = testVor->NeighborIdsBegin(i); currNeibor != testVor->NeighborIdsEnd(i); ++currNeibor )
    {
      std::cout<<(*currNeibor)<<",";
    }
    std::cout<<std::endl<<std::endl;
  }

  std::cout<<"Vertices Informations:"<<std::endl;
  Vor::VertexIterator allVerts;
  int j = 0;
  for( allVerts = testVor->VertexBegin(); allVerts != testVor->VertexEnd(); ++allVerts )
  {
    std::cout<<"Vertices No."<<j;
    j++;
    std::cout<<": At ("<<(allVerts.Value())[0]<<","<<(allVerts.Value())[1]<<")"<<std::endl;
  }

  typedef itk::MeshFileWriter<Vor> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(testVor);
  writer->SetFileName(argv[1]);
  writer->Update();

  return EXIT_SUCCESS;
}
