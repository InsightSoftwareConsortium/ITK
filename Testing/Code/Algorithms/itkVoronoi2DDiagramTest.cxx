/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoi2DDiagramTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#include "itkVoronoi2DDiagram.h"
#include <stdio.h>

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
