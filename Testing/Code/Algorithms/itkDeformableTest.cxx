#include <iostream>
#include <string>
#include <math.h>

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkDeformableMesh.h"
#include "itkBalloonForceFilter.h"
#include "itkTriangleCell.h"

using namespace itk;

typedef itk::DeformableMesh<float>  DMesh;
typedef itk::Mesh<float>  MyMesh;
typedef itk::BalloonForceFilter<MyMesh, MyMesh> BFilter;
typedef itk::TriangleCell<DMesh::PixelType, DMesh::CellType>	   TriCell;

int main(void)
{
  DMesh::Pointer m_Mesh(DMesh::New());
  BFilter::Pointer m_Filter = BFilter::New();
  MyMesh::Pointer force(MyMesh::New());
  MyMesh::Pointer displace(MyMesh::New());
  MyMesh::Pointer derive(MyMesh::New());
  
  m_Mesh->SetDefault();
  m_Mesh->SetCenter(0, 0, 0);
  m_Mesh->SetResolution(4, 9);
  m_Mesh->SetScale(1.0, 1.0, 1.0);
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
  m_Filter->SetResolution(4, 9, 1);
  m_Filter->SetForces(force);
  m_Filter->SetDisplacements(displace);
  m_Filter->SetDerives(derive);
  m_Filter->Initialize();
  m_Filter->SetStiffnessMatrix();
  m_Filter->ComputeForce();
  m_Filter->ComputeDt();
  m_Filter->Advance();
  m_Filter->ComputeOutput();
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
  return 0;
}