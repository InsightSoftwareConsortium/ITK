/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiDiagram2DTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkVoronoiDiagram2D.h"
#include "itkVoronoiDiagram2DGenerator.h"
#include <stdio.h>


int itkVoronoiDiagram2DTest(int, char* [] ){
  const double HEI=400;
  const double WID=400;
  const int NUMSEEDS=20;

  typedef itk::VoronoiDiagram2D<double> Vor;
  typedef itk::VoronoiDiagram2DGenerator<double> VorGenerator;

  typedef Vor::PointType PointType;
  typedef Vor::CellType CellType;
  typedef Vor::CellAutoPointer CellAutoPointer;
  typedef CellType::PointIdIterator PointIdIterator;
  typedef Vor::NeighborIdIterator NeighborIdIterator;

  Vor::Pointer testVor(Vor::New());
  VorGenerator::Pointer testVorGen(VorGenerator::New());

  PointType insize;
  insize[0]=WID;
  insize[1]=HEI;
  testVorGen->SetBoundary(insize);

  testVorGen->SetRandomSeeds(NUMSEEDS);
  testVorGen->Update();
  testVor=testVorGen->GetOutput();

  for(int i=0;i<NUMSEEDS; i++){
    PointType currP=testVor->GetSeed(i);
    std::cout<<"Seed No."<<i<<": At ("<<currP[0]<<"," <<currP[1]<<")"<<std::endl;
    std::cout<<"  Boundary Vertices List (in order):";
    CellAutoPointer currCell;
    testVor->GetCellId(i, currCell);
    PointIdIterator currCellP;
    for(currCellP=currCell->PointIdsBegin();currCellP!=currCell->PointIdsEnd();++currCellP)
    {
      std::cout<<(*currCellP)<<",";
    }
    std::cout<<std::endl;
    std::cout<<"  Neighbors (Seed No.):";
    NeighborIdIterator currNeibor;
    for(currNeibor=testVor->NeighborIdsBegin(i);currNeibor!=testVor->NeighborIdsEnd(i);++currNeibor)
    {
      std::cout<<(*currNeibor)<<",";
    }
    std::cout<<std::endl<<std::endl;
  }

  std::cout<<"Vertices Informations:"<<std::endl;
  Vor::VertexIterator allVerts;
  int j = 0;
  for(allVerts=testVor->VertexBegin();allVerts!=testVor->VertexEnd();++allVerts)
  {
    std::cout<<"Vertices No."<<j;
    j++;
    std::cout<<": At ("<<(*allVerts)[0]<<","<<(*allVerts)[1]<<")"<<std::endl;
  }

  return 0;
}
