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

const int WIDTH = 25;
const int HEIGHT = 25;

using namespace itk;

typedef itk::DeformableMesh<float>  DMesh;
typedef itk::Mesh<float>  MyMesh;
typedef itk::BalloonForceFilter<DMesh, DMesh> BFilter;
typedef itk::TriangleCell<DMesh::PixelType, DMesh::CellTraits>	   TriCell;
typedef itk::Image<unsigned short,3> IImage;

int main(void)
{


  DMesh::Pointer m_Mesh(DMesh::New());
  BFilter::Pointer m_Filter = BFilter::New();
  DMesh::Pointer force(DMesh::New());
  DMesh::Pointer displace(DMesh::New());
  DMesh::Pointer derive(DMesh::New());
  DMesh::Pointer normal(DMesh::New());
  DMesh::Pointer location(DMesh::New());
	
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
	if ((k%10 == 0) || 
		((k/10)%10 == 0)) {
	  it.Set(1);
	} else {
	  it.Set(0);
	}
	k++;
	++it;
  }

  m_Mesh->SetDefault();
  m_Mesh->SetResolution(1, 10);
  m_Mesh->SetScale(1.0, 1.0, 1.0);
  m_Mesh->SetCenter(13, 13, 0);
  m_Mesh->Allocate();

//////////////////////////////////////////////////
// for test

  MyMesh::PointType ds;
  m_Mesh->SetDefault();

  MyMesh::PointsContainerPointer      myPoints = m_Mesh->GetPoints();
  MyMesh::PointsContainer::Iterator   point    = myPoints->Begin();
  while(  point != myPoints->End() )
  {
	  ds = point.Value();
    ++point;
	std::cout << "Point: " << ds[0] << ds[1] <<ds[2] << std::endl;
  }

  m_Filter->SetInput(m_Mesh);
  m_Filter->SetPotential(inputimg);
  m_Filter->SetResolution(1, 10, 1);
  m_Filter->SetCenter(13, 13, 0);
  m_Filter->SetLocations(location);
  m_Filter->SetForces(force);
  m_Filter->SetNormals(normal);
  m_Filter->SetDisplacements(displace);
  m_Filter->SetDerives(derive);
  m_Filter->Initialize();
  m_Filter->SetStiffnessMatrix();
  int sign;


  for (k=0; k<10; k++) {

	m_Filter->ComputeForce();
	m_Filter->ComputeDt();
	m_Filter->Advance();

  }



  m_Filter->GapSearch();
  m_Filter->ComputeOutput();

  MyMesh::PointsContainerPointer      myoutput = m_Filter->GetOutput()->GetPoints();
  MyMesh::PointsContainer::Iterator   m_output = myoutput->Begin();
  while(  m_output != myoutput->End() )
  {
	  ds = point.Value();
    ++m_output;
	std::cout << "Point: " << ds[0] << ds[1] <<ds[2] << std::endl;
  }
  return 0;
}
