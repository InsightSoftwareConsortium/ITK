#include <iostream>
#include <string>
#include <math.h>

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkDeformableMesh.h"
#include "itkBalloonForceFilter.h"
#include "itkTriangleCell.h"
#include "itkImage.h"
#include "itkSimpleImageRegionIterator.h"

const int WIDTH = 20;
const int HEIGHT = 20;

using namespace itk;

typedef itk::DeformableMesh<float>  DMesh;
typedef itk::Mesh<float>  MyMesh;
typedef itk::BalloonForceFilter<MyMesh, MyMesh> BFilter;
typedef itk::TriangleCell<DMesh::PixelType, DMesh::CellType>	   TriCell;
typedef itk::Image<unsigned short,3> IImage;

int main(void)
{
  DMesh::Pointer m_Mesh(DMesh::New());
  BFilter::Pointer m_Filter = BFilter::New();
  MyMesh::Pointer force(MyMesh::New());
  MyMesh::Pointer displace(MyMesh::New());
  MyMesh::Pointer derive(MyMesh::New());
	
  IImage::Pointer inputimg=IImage::New();
  IImage::SizeType size={{HEIGHT,WIDTH,1}};
  IImage::IndexType index=IImage::IndexType::ZeroIndex;
  IImage::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);

  inputimg->SetLargestPossibleRegion( region );
  inputimg->SetBufferedRegion( region );
  inputimg->SetRequestedRegion( region );
  inputimg->Allocate();
  	
  itk::SimpleImageRegionIterator <IImage> it(inputimg, region);
  it.Begin();	
  int k=0;
  while( !it.IsAtEnd()) {    
	if ( (k%WIDTH==5) || (k%WIDTH==15) || (k/HEIGHT==5) || (k/HEIGHT==15) ||
		(k%WIDTH==4) || (k%WIDTH==16) || (k/HEIGHT==4) || (k/HEIGHT==16) ) it.Set(1);
	else it.Set(0);
	k++;
	++it;
  }

  m_Mesh->SetDefault();
  m_Mesh->SetResolution(1, 20);
  m_Mesh->SetScale(1.0, 1.0, 1.0);
  m_Mesh->SetCenter(10, 10, 0);
  m_Mesh->Allocate();

//////////////////////////////////////////////////
// for test

  MyMesh::PointType ds;
  MyMesh::PixelType dc;
  const unsigned long *dl;
  m_Mesh->SetDefault();

  MyMesh::PointsContainerPointer      myPoints = m_Mesh->GetPoints();
  MyMesh::PointsContainer::Iterator   point    = myPoints->Begin();
  while(  point != myPoints->End() )
  {
	  ds = point.Value();
    ++point;
	std::cout << "Point: " << ds[0] << ds[1] <<ds[2] << std::endl;
  }


/*
  MyMesh::CellDataContainerPointer      myCellData = m_Mesh->GetCellData();
  MyMesh::CellDataContainer::Iterator   celldata   = myCellData->Begin();
  while(  celldata != myCellData->End() )
  {
	  dc = celldata.Value();
    ++celldata;
  }

  MyMesh::CellsContainerPointer      myCells = m_Mesh->GetCells();
  MyMesh::CellsContainer::Iterator   cells   = myCells->Begin();
  while(  cells != myCells->End() )
  {
	  dl = cells.Value()->GetPointIds();
    ++cells;
  }
//  SetPoint(1, (PointType)d);

//  m_Mesh->GetPoint(3, (ds));


//////////////////////////////////////////////////
*/
  m_Filter->SetInput(m_Mesh);
  m_Filter->SetPotential(inputimg);
  m_Filter->SetResolution(1, 20, 1);
  m_Filter->SetCenter(10, 10, 0);
  m_Filter->SetForces(force);
  m_Filter->SetDisplacements(displace);
  m_Filter->SetDerives(derive);
  m_Filter->Initialize();
  m_Filter->SetStiffnessMatrix();
  int i, j, sign;
  for (k=0; k<30; k++) {
	m_Filter->Reset();
	m_Filter->ComputeForce();
	m_Filter->ComputeDt();
	m_Filter->Advance();
    m_Filter->ComputeOutput();
	it.Begin();
	for(int i = 0;i < HEIGHT; i++){
	  for (int j = 0; j < WIDTH; j++){
		sign = 0;
		point = myPoints->Begin();
		while(  point != myPoints->End() ) {
		  ds = point.Value();
		  ++point;
		  if ((i == (int)ds[0]) && (j == (int)ds[1])) {
			std::cout << "*" ;
			sign = 1;
			break;
		  }
		}
		if (sign == 0) std::cout<<it.Get();
		++it;
	  }
	  std::cout<<std::endl;
	}
    getchar();
  }
//  m_Filter->Delete();
//  m_Mesh->Delete();
//  force->Delete();
//  displace->Delete();
//  derive->Delete();
/*
  point    = myPoints->Begin();
  while(  point != myPoints->End() )
  {
	  ds = point.Value();
    ++point;
	std::cout << "Point: " << ds[0] << ds[1] <<ds[2] << std::endl;
  }
*/
	it.Begin();
	for(int i = 0;i < HEIGHT; i++){
	  for (int j = 0; j < WIDTH; j++){
		sign = 0;
		point = myPoints->Begin();
		while(  point != myPoints->End() ) {
		  ds = point.Value();
		  ++point;
		  if ((i == (int)ds[0]) && (j == (int)ds[1])) {
			std::cout << "*" ;
			sign = 1;
			break;
		  }
		}
		if (sign == 0) std::cout<<it.Get();
		++it;
	  }
	  std::cout<<std::endl;
	}

  return 0;
}