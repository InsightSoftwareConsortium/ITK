/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoi2DDiagramTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include "itkVoronoi2DDiagram.h"
#include <cstdio>

const double HEI=400;
const double WID=400;
const int NUMSEEDS=20;


int main(void){
	typedef itk::Voronoi2DDiagram<double> Vor;
	typedef Vor::PointType PointType;
	typedef Vor::Cell Cell;
	typedef Vor::CellPointer CellPointer;
	typedef Cell::PointIdIterator PointIdIterator;
	typedef Vor::NeighborIdIterator NeighborIdIterator;

	Vor::Pointer testVor(Vor::New());

	PointType insize;
	insize[0]=WID;
	insize[1]=HEI;
	testVor->SetBoundary(insize);

	testVor->SetRandomSeeds(NUMSEEDS);
	testVor->GenerateDiagram();

	for(int i=0;i<NUMSEEDS; i++){
		PointType currP=testVor->getSeed(i);
		std::cout<<"Seed No."<<i<<": At ("<<currP[0]<<"," <<currP[1]<<")"<<std::endl;
		std::cout<<"  Boundary Vertices List (in order):";
		CellPointer currCell;
		currCell=testVor->GetCellId(i);
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