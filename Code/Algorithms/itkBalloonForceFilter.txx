/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBalloonForceFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkBalloonForceFilter.h"

namespace itk
{
/**
 * standard 
 */
template <typename TInputMesh, typename TOutputMesh>
BalloonForceFilter<TInputMesh, TOutputMesh>
::BalloonForceFilter()
{

}

/**
 * set the stiffness parameter which would help 
 * to build the stiffness mtrix
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetStiffness(double a, double b)
{
  Stiffness[0] = a; 
  Stiffness[1] = b; 
}

template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetCenter(int a, int b, int c)
{
  Center[0] = a; 
  Center[1] = b; 
  Center[2] = c;
}

/**
 * store the force vectors on each node into a mesh 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetForces(TInputMesh* force)
{
  Forces = force; 
}

/**
 * store the displacement vectors on each node into a mesh 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetDisplacements(TInputMesh* displace)
{
  Displacements = displace; 
}

/**
 * set the input binary image 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetPotential(ImagePointer potential)
{
  Potential = potential; 
}

template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetNormals(TInputMesh* normals)
{
  Normals = normals; 
}

template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetLocations(TInputMesh* location)
{
  Locations = location; 
}

/**
 * store the displacement derivative vectors on 
 * each node into a mesh 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetDerives(TInputMesh* derive)
{
  Derives = derive; 
}

/**
 * set the resolutions of the model
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetResolution(int a, int b, int c)
{
  Resolution[0] = a; 
  Resolution[1] = b;
  Resolution[2] = c;
}

/**
 * set default value of some parameters and reset forces,
 * displacements and displacement derivatives
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::Initialize()
{
// the Locations store the position of the nodes on the model
  m_Input = this->GetInput(0);
  m_NumNodes = m_Input->GetNumberOfPoints();
  m_NumCells = m_Input->GetNumberOfCells();
//  Locations = this->GetOutput();

  typename TInputMesh::PointsContainerPointer		myPoints = m_Input->GetPoints();
  typename TInputMesh::PointsContainer::Iterator	points = myPoints->Begin();
	
  typename TInputMesh::PointsContainerPointer		myForces = Forces->GetPoints();
  myForces->Reserve(m_NumNodes*10);
  typename TInputMesh::PointsContainer::Iterator	forces = myForces->Begin();

  typename TInputMesh::PointsContainerPointer		myDerives = Derives->GetPoints();
  myDerives->Reserve(m_NumNodes*10);
  typename TInputMesh::PointsContainer::Iterator	derives = myDerives->Begin();

  typename TInputMesh::PointsContainerPointer		myDisplacements = Displacements->GetPoints();
  myDisplacements->Reserve(m_NumNodes*10);
  typename TInputMesh::PointsContainer::Iterator	displacements = myDisplacements->Begin();

  typename TInputMesh::PointsContainerPointer		myNormals = Normals->GetPoints();
  myNormals->Reserve(m_NumNodes*10);
  typename TInputMesh::PointsContainer::Iterator	normals = myNormals->Begin();

  typename TInputMesh::PointsContainerPointer		myLocations = Locations->GetPoints();
  myLocations->Reserve(m_NumNodes*10);
  typename TInputMesh::PointsContainer::Iterator	locations = myLocations->Begin();

  typename TInputMesh::CellsContainerPointer		myCells;
  myCells = m_Input->GetCells();
  typename TInputMesh::CellsContainer::Iterator		cells = myCells->Begin(); 
  
  typename TInputMesh::CellDataContainerPointer		myCellData = m_Input->GetCellData();
  typename TInputMesh::CellDataContainer::Iterator	celldata = myCellData->Begin(); 
  
  Stiffness[0] = 0.000001; 
  Stiffness[1] = 0.04; 
  m_NumNewNodes = 0;

  float d[3] = {0,0,0}, fd=0; 

  TimeStep = 0.001; 
  m_Step = 0;

  while( points != myPoints->End() )
  {
	locations.Value() = points.Value();
    ++points;
	++locations;
  }

  while( forces != myForces->End() )
  {
	forces.Value() = d;
    ++forces;
  }

  while( normals != myNormals->End() )
  {
	normals.Value() = d;
    ++normals;
  }

  for (int i=0; i<m_NumNodes-2; i++  )
  {
	Forces->SetPointData(i, 1.0);
	Locations->SetPointData(i, 0.0);
  }

  while( derives != myDerives->End() )
  {
	derives.Value() = d;
    ++derives;
  }

  while( displacements != myDisplacements->End() )
  {
	displacements.Value() = d;
    ++displacements;
  }

  TriCell::Pointer insertCell(TriCell::New());
  unsigned long tripoints[3];
  const unsigned long *tp;
  float x;

  for (int i=0; i<m_NumCells; i++) {
	tp = cells.Value()->GetPointIds();
	tripoints[0] = tp[0];
	tripoints[1] = tp[1];
	tripoints[2] = tp[2];
	insertCell->SetPointIds(tripoints);
	Locations->SetCell(i, insertCell);
	x = celldata.Value();
	Locations->SetCellData(i, (PT)x);
	++cells;
  }
} 

/**
 * set the stiffness matrix 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetStiffnessMatrix () 
{ 
  typename TInputMesh::CellDataContainerPointer myCellData = Locations->GetCellData();
//  myCellData = Locations->GetCellData();
  typename TInputMesh::CellDataContainer::Iterator celldata = myCellData->Begin();

//  unsigned long CellNum;
//  CellNum = m_Input->GetNumberOfCells();

  K = (vnl_matrix_fixed<double,4,4>**) malloc(sizeof(vnl_matrix_fixed<double,4,4>*)*m_NumCells);
  float x;	

  float us = vnl_math::pi / 9; 
  float vs = 2.0*vnl_math::pi / 18; 
  float a = us*us, b = vs*vs; 
  float area = us*vs/2, k00, k01, k02, k11, k12, k22; 
 
  k00 = area * (Stiffness[1]/a + Stiffness[1]/b + Stiffness[0]); 
  k01 = area * (-Stiffness[1]/a + Stiffness[0]); 
  k02 = area * (-Stiffness[1]/b + Stiffness[0]); 
  k11 = area * (Stiffness[1]/a+Stiffness[0]); 
  k12 = area * Stiffness[0]; 
  k22 = area * (Stiffness[1]/b + Stiffness[0]); 
 
  NStiffness[0][0] = k00; 
  NStiffness[0][1] = k01; 
  NStiffness[0][2] = k02; 
  NStiffness[0][3] = 0.0; 
  NStiffness[1][0] = k01; 
  NStiffness[1][1] = k11; 
  NStiffness[1][2] = k12; 
  NStiffness[1][3] = 0.0; 
  NStiffness[2][0] = k02; 
  NStiffness[2][1] = k12; 
  NStiffness[2][2] = k22; 
  NStiffness[2][3] = 0.0; 
  NStiffness[3][0] = 0.0; 
  NStiffness[3][1] = 0.0; 
  NStiffness[3][2] = 0.0; 
  NStiffness[3][3] = 1.0; 
     
  k00 = area * (Stiffness[1]/a+Stiffness[0]); 
  k01 = area * (-Stiffness[1]/a+Stiffness[0]); 
  k02 = area * Stiffness[0]; 
  k11 = area * (Stiffness[1]/a+Stiffness[1]/b+Stiffness[0]); 
  k12 = area * (-Stiffness[1]/b+Stiffness[0]); 
  k22 = area * (Stiffness[1]/b+Stiffness[0]); 
     
  SStiffness[0][0] = k00; 
  SStiffness[0][1] = k01; 
  SStiffness[0][2] = k02; 
  SStiffness[0][3] = 0.0; 
  SStiffness[1][0] = k01; 
  SStiffness[1][1] = k11; 
  SStiffness[1][2] = k12; 
  SStiffness[1][3] = 0.0; 
  SStiffness[2][0] = k02; 
  SStiffness[2][1] = k12; 
  SStiffness[2][2] = k22; 
  SStiffness[2][3] = 0.0; 
  SStiffness[3][0] = 0.0; 
  SStiffness[3][1] = 0.0; 
  SStiffness[3][2] = 0.0; 
  SStiffness[3][3] = 1.0; 
 
  k00 = area * (Stiffness[1]/b + Stiffness[0]); 
  k01 = area * (-Stiffness[1]/b + Stiffness[0]); 
  k02 = area * Stiffness[0]; 
  k11 = area * (Stiffness[1]/a + Stiffness[1]/b + Stiffness[0]); 
  k12 = area * (-Stiffness[1]/a + Stiffness[0]); 
  k22 = area * (Stiffness[1]/a + Stiffness[0]); 
 
  CStiffness[0][0] = k00; 
  CStiffness[0][1] = k01; 
  CStiffness[0][2] = k02; 
  CStiffness[0][3] = 0.0; 
  CStiffness[1][0] = k01; 
  CStiffness[1][1] = k11; 
  CStiffness[1][2] = k12; 
  CStiffness[1][3] = 0.0; 
  CStiffness[2][0] = k02; 
  CStiffness[2][1] = k12; 
  CStiffness[2][2] = k22; 
  CStiffness[2][3] = 0.0; 
  CStiffness[3][0] = 0.0; 
  CStiffness[3][1] = 0.0; 
  CStiffness[3][2] = 0.0; 
  CStiffness[3][3] = 1.0; 

  int j=0;
  while (celldata != myCellData->End()){
	x = celldata.Value();
	++celldata;
	switch ((int)(x)) { 
    case 1: 
      K[j] = &SStiffness; 
      break; 
    case 2: 
      K[j] = &NStiffness; 
      break; 
    case 3: 
      K[j] = &CStiffness; 
      break; 
    } 
	++j;
  }

} 

/**
 * compute force using the information from image 
 * and the balloon force. The calculation of the 
 * image force not included by far, it will be added 
 * when more experiments are done 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::ComputeForce()
{
  int i, p, q, sign, label, l=0; 
  IndexType coord = {0, 0, 0};
  IndexType extend = {0, 0, 0};
  float extends[3], t, xs, ys, zs, r[2]; 
  FloatVector n1, n, vec1, vec2; 
  float f[3]={0, 0, 0};
  IPT x, y, z;
  IPT* y_pt;
  IPT* z_pt;
  y_pt = &y;
  z_pt = &z;

  typename TInputMesh::PointsContainerPointer Points;
  Points = Locations->GetPoints();
  typename TInputMesh::PointsContainer::Iterator   points = Points->Begin();

  typename TInputMesh::PointsContainerPointer		myForces = Forces->GetPoints();
  typename TInputMesh::PointsContainer::Iterator	forces = myForces->Begin();

  typename TInputMesh::PointsContainerPointer		myNormals = Normals->GetPoints();
  typename TInputMesh::PointsContainer::Iterator	normals = myNormals->Begin();

  typename TInputMesh::PointDataContainerPointer	myForceData = Forces->GetPointData();
  typename TInputMesh::PointDataContainer::Iterator	forcedata = myForceData->Begin();

  typename TInputMesh::PointDataContainerPointer	myPointData = Locations->GetPointData();
  typename TInputMesh::PointDataContainer::Iterator	pointstatus = myPointData->Begin();
/*
  m_size = Potential->GetLargestPossibleRegion().GetSize();
  IndexType index = IndexType::ZeroIndex;
  typename ImageType::RegionType region;
  region.SetSize(m_size);
  region.SetIndex(index);
  Potential->SetLargestPossibleRegion( region );
  Potential->SetBufferedRegion( region );
  Potential->SetRequestedRegion( region );
  Potential->Allocate();  

  SimpleImageRegionIterator <ImageType> it(this->Potential, region);

  it.Begin();
*/
//  const int npoints = Forces->GetNumberOfPoints(); 
  int slicediv = m_NumNodes / this->Resolution[0]; 
  float pxx, pyy, pxy, qxx, qyy, qxy;
  double d=0.0, f1=0.0;

  i = 0;
  int slice = i / slicediv; 
  
  while( i != m_NumNodes - 2 )
  {
    xs = ys = zs = 1.0; 
	x = points.Value();

	if ( forcedata.Value() == 1.0) {
	  q = (i < m_NumNodes-3)?i+1:0;
      Locations->GetPoint (q, y_pt);  
      p = (i == 0)?m_NumNodes-3:i-1;
      Locations->GetPoint (p, z_pt);
	}

	coord[0] = (int) x[0];
	coord[1] = (int) x[1];
	coord[2] = (int) x[2];
	if ( Potential->GetPixel(coord) != 64000 ) {
		xs = ys = zs = 0.0;
	}
	extends[0] = x[0];
	extends[1] = x[1];
	extends[2] = x[2];
	extend[0] = (int) x[0];
	extend[1] = (int) x[1];
	extend[2] = (int) x[2];

	if ( forcedata.Value() == 1.0) {
	  qxx = y[1] - x[1];
      qyy = x[0] - y[0];
      qxy = sqrt(qxx*qxx+qyy*qyy);
	  pxx = x[1] - z[1];
	  pyy = z[0] - x[0];
	  pxy = sqrt(pxx*pxx+pyy*pyy);	  
	  qxx = qxx/qxy;
	  qyy = qyy/qxy;
	  pxx = pxx/pxy;  
	  pyy = pyy/pxy;
	  n[0] = qxx + pxx;
	  n[1] = qyy + pyy;  
	  n[2] = 0;
	  f[0] = n[0];
	  f[1] = n[1];
	  f[2] = n[2];
	  normals.Value() = f;
	} else {
	  y = normals.Value();
	  n[0] = y[0];
	  n[1] = y[1];
	  n[2] = y[2];
	}
/*
	if ((pyy*qyy+pxx*qxx)<0.5) {
	  x[0] = (y[0] + z[0])/2;
	  x[1] = (y[1] + z[1])/2;
	  points.Value() = x;
	} 
*/
	if (n[0]*n[0] >= n[1]*n[1]) {
	  if (n[0] < 0) {
	    r[1] = -n[1]/n[0];
	    r[0] = -1;
	  }
	  if (n[0] > 0) {
	    r[1] = n[1]/n[0];
	    r[0] = 1;
	  }
	}
	else {
	  if (n[1] < 0) {
	    r[1] = -1;
	    r[0] = -n[0]/n[1];
	  }
	  if (n[1] > 0) {
	    r[1] = 1;
	    r[0] = n[0]/n[1];
	  }
	}

	t = 0.0;
	sign = 0.0;
	m_MiniT = 20;
	while ((t < 20.0) && (sign == 0)){
	  extends[0] += r[0];
	  extends[1] += r[1];
      extend[0] = (int) extends[0];
      extend[1] = (int) extends[1];
	  label = Potential->GetPixel(extend);
	  if ( label != 64000 ) sign = 1;

	  if (r[0] >= 0) extend[0] = (int)(extends[0]-1);
	  else  extend[0] = (int)(extends[0]+1);
	  extend[1] = (int) extends[1];
	  label = Potential->GetPixel(extend);
	  if ( label != 64000 ) sign = 1;

	  if (r[1] >= 0) extend[1] = (int)(extends[1]-1);
	  else  extend[1] = (int)(extends[1]+1);  
	  extend[0] = (int) extends[0];
	  label = Potential->GetPixel(extend);
	  if ( label != 64000 ) sign = 1;

	  t += 1.0;
	}

	if (t < 2) pointstatus.Value() = 1.0;
	else pointstatus.Value() = 0.0;
	
    f[0] = t*50*n[0]*xs; 
    f[1] = t*50*n[1]*ys;
    f[2] = 0;

    forces.Value() = f;
	forcedata.Value() = 0.0;
	++pointstatus;
    ++forces;
	++forcedata;
	++points;
	++normals;
	++i;
  }
/*
  forces = myForces->Begin();
  forcedata = myForceData->Begin();
  while( i != npoints - 2 ) {
	t = forcedata.Value();
	if (t > 5) {
	  t = m_MiniT/t;
	  x = forces.Value();
	  x[0] = x[0] * t;
	  x[1] = x[1] * t;
	  forces.Value() = x;
	}
	++forcedata;
	++forces;
  }
*/
}

/**
 * compute the derivatives using d'- Kd = f 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::ComputeDt()
{
  int i; 
  const unsigned long *tp;
  typename Cell::Pointer testCell(TriCell::New());
  int npts = 3;

  typename TInputMesh::CellsContainerPointer myCells;
  myCells = Locations->GetCells();
  typename TInputMesh::CellsContainer::Iterator cells = myCells->Begin();

  typename  TInputMesh::PointsContainerPointer      myForces = Forces->GetPoints();
  typename TInputMesh::PointsContainer::Iterator   forces = myForces->Begin();

  typename TInputMesh::PointsContainerPointer      myDerives = Derives->GetPoints();
  typename TInputMesh::PointsContainer::Iterator   derives = myDerives->Begin();
/*
  typename TInputMesh::CellDataContainerPointer myCellData = m_Mesh->GetCellData();
  myCellData = Locations->GetCellData();
  typename TInputMesh::CellDataContainer::Iterator celldata = myCellData->Begin();
*/
//  const int ncells = Locations->GetNumberOfCells();

//  const int npoints = Forces->GetNumberOfPoints();  
  float p = 1; 
  i = 0;
  IPT v1, v2, v3;
  IPT* v1_pt;
  IPT* v2_pt;
  IPT* v3_pt;
  v1_pt = &v1;
  v2_pt = &v2;
  v3_pt = &v3;

  while( cells != myCells->End() )
  {
	tp = cells.Value()->GetPointIds();
	++cells;
    Displacements->GetPoint (tp[0], v1_pt); 
    Displacements->GetPoint (tp[1], v2_pt); 
    Displacements->GetPoint (tp[2], v3_pt); 
    v1[0] *= K[i]->get(0, 0)*p; 
    v1[1] *= K[i]->get(0, 0)*p; 
    v1[2] *= K[i]->get(0, 0)*p; 
    v2[0] *= K[i]->get(0, 1)*p; 
    v2[1] *= K[i]->get(0, 1)*p; 
    v2[2] *= K[i]->get(0, 1)*p; 
    v3[0] *= K[i]->get(0, 2)*p; 
    v3[1] *= K[i]->get(0, 2)*p; 
    v3[2] *= K[i]->get(0, 2)*p; 
    v1[0] += v2[0]+v3[0]; 
    v1[1] += v2[1]+v3[1]; 
    v1[2] += v2[2]+v3[2]; 
    Forces->GetPoint (tp[0], v2_pt); 
//    if ((v2[0]==0) && (v2[1]==0)){}
//    else {
	v2[0] -= v1[0]; 
	v2[1] -= v1[1]; 
	v2[2] -= v1[2];
//    } 
    Forces->SetPoint (tp[0], v2); 
 
    Displacements->GetPoint (tp[0], v1_pt); 
    Displacements->GetPoint (tp[1], v2_pt); 
    Displacements->GetPoint (tp[2], v3_pt); 
    v1[0] *= K[i]->get(1, 0)*p; 
    v1[1] *= K[i]->get(1, 0)*p; 
    v1[2] *= K[i]->get(1, 0)*p; 
    v2[0] *= K[i]->get(1, 1)*p; 
    v2[1] *= K[i]->get(1, 1)*p; 
    v2[2] *= K[i]->get(1, 1)*p; 
    v3[0] *= K[i]->get(1, 2)*p; 
    v3[1] *= K[i]->get(1, 2)*p; 
    v3[2] *= K[i]->get(1, 2)*p; 
    v1[0] += v2[0]+v3[0]; 
    v1[1] += v2[1]+v3[1]; 
    v1[2] += v2[2]+v3[2]; 
    Forces->GetPoint (tp[1], v2_pt);  
//    if ((v2[0]==0) && (v2[1]==0)){}
//    else {
	v2[0] -= v1[0]; 
	v2[1] -= v1[1]; 
	v2[2] -= v1[2];
//    } 
    Forces->SetPoint (tp[1], v2); 
 
    Displacements->GetPoint (tp[0], v1_pt); 
    Displacements->GetPoint (tp[1], v2_pt); 
    Displacements->GetPoint (tp[2], v3_pt); 
    v1[0] *= K[i]->get(2, 0)*p; 
    v1[1] *= K[i]->get(2, 0)*p; 
    v1[2] *= K[i]->get(2, 0)*p; 
    v2[0] *= K[i]->get(2, 1)*p; 
    v2[1] *= K[i]->get(2, 1)*p; 
    v2[2] *= K[i]->get(2, 1)*p; 
    v3[0] *= K[i]->get(2, 2)*p; 
    v3[1] *= K[i]->get(2, 2)*p; 
    v3[2] *= K[i]->get(2, 2)*p; 
    v1[0] += v2[0]+v3[0]; 
    v1[1] += v2[1]+v3[1]; 
    v1[2] += v2[2]+v3[2]; 
    Forces->GetPoint (tp[2], v2_pt); 
//    if ((v2[0]==0) && (v2[1]==0)){}
//    else {
	v2[0] -= v1[0]; 
	v2[1] -= v1[1]; 
	v2[2] -= v1[2];
//    } 
    Forces->SetPoint (tp[2], v2);  
	++i;
  } 

  while ( derives != myDerives->End() ) {
	derives.Value() = forces.Value();
	++derives; 
	++forces;
  }
   
}

template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::Reset()
{
  typename TInputMesh::PointsContainerPointer		myForces = Forces->GetPoints();
  typename TInputMesh::PointsContainer::Iterator	forces = myForces->Begin();

  typename TInputMesh::PointDataContainerPointer	myForceData = Forces->GetPointData();
  typename TInputMesh::PointDataContainer::Iterator	forcedata = myForceData->Begin();

  typename TInputMesh::PointsContainerPointer		myNormals = Normals->GetPoints();
  typename TInputMesh::PointsContainer::Iterator	normals = myNormals->Begin();

  typename TInputMesh::PointsContainerPointer		myDerives = Derives->GetPoints();
  typename TInputMesh::PointsContainer::Iterator	derives = myDerives->Begin();

  typename TInputMesh::PointsContainerPointer		myDisplacements = Displacements->GetPoints();
  typename TInputMesh::PointsContainer::Iterator	displacements = myDisplacements->Begin();
  
  typename TriCell::Pointer				insertCell(TriCell::New()); 

//  const int npoints = Locations->GetNumberOfPoints();
//  m_NumNewNodes = m_NumNewNodes + m_NumNewNodes;
  int i, j, cell=0; 
  float status, d[3] = {0,0,0}, z;
  IPT x;
  IPT* x_pt; 
  x_pt = &x;
  unsigned long tripoints[3];
 
  if (m_NumNewNodes != 0) cell=1;

  j = m_NumNodes;;
  m_NumNodes = m_NumNodes + m_NumNewNodes;
  i = m_NumNodes;

  while ( m_NumNewNodes != 0) {
	if ( (j-1) > (int)(m_NewNodes[m_NumNewNodes-1][3]) ) {
	  Locations->GetPoint(--j, x_pt);
	  status = 0.0;
	}
	else {
	  m_NumNewNodes--;
	  x[0] = m_NewNodes[m_NumNewNodes][0];
	  x[1] = m_NewNodes[m_NumNewNodes][1];
	  x[2] = m_NewNodes[m_NumNewNodes][2];
	  status = 1.0;
    }
	
	Locations->SetPoint(--i, x);
	Forces->SetPointData(i, status);
	Locations->SetPointData(i, 0.0);
	if (status == 0.0) {
	  Normals->GetPoint(j, x_pt);
	  Normals->SetPoint(i, x);
	}
  }

  while( forces != myForces->End() )
  {
	forces.Value() = d;
    ++forces;
  }

  while( derives != myDerives->End() )
  {
	derives.Value() = d;
    ++derives;
  }

  while( displacements != myDisplacements->End() )
  {
	displacements.Value() = d;
    ++displacements;
  }

  if (cell == 1) {
  int p = 0, jn;

  for (int j=0; j<m_NumNodes-2; j++) { 
	jn = (j+1)%(m_NumNodes-2);
    tripoints[0] = m_NumNodes-2; 
    tripoints[1] = jn; 
    tripoints[2] = j; 
	insertCell->SetPointIds(tripoints);
	Locations->SetCell(p, insertCell);
	Locations->SetCellData(p, (PT)1.0);
	p++;
	insertCell = TriCell::New();
	tripoints[2] = (Resolution[0]-1)*Resolution[1]+j; 
	tripoints[1] = m_NumNodes-1; 
    tripoints[0] = tripoints[2]-j+jn; 
	insertCell->SetPointIds(tripoints);
	Locations->SetCell(p, insertCell);
	Locations->SetCellData(p, (PT)2.0);
	p++;
	insertCell = TriCell::New();
  }
	
  m_NumCells = p;

  K = (vnl_matrix_fixed<double,4,4>**) malloc(sizeof(vnl_matrix_fixed<double,4,4>*)*m_NumCells);
  
  typename TInputMesh::CellDataContainer::Iterator celldata = Locations->GetCellData()->Begin();
  int j=0;
  while (celldata != Locations->GetCellData()->End()){
	z = celldata.Value();
	++celldata;
	switch ((int)(z)) { 
    case 1: 
      K[j] = &SStiffness; 
      break; 
    case 2: 
      K[j] = &NStiffness; 
      break; 
    case 3: 
      K[j] = &CStiffness; 
      break; 
    } 
	++j;
  }

  }
}

/**
 * update the displacements using dnew = dold + timestep*d' 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::Advance(){
  typename TInputMesh::PointType s, d, ds; 
//  const int npoints = Forces->GetNumberOfPoints(); 
  int i;

  typename TInputMesh::PointsContainerPointer      myDerives = Derives->GetPoints();
  typename TInputMesh::PointsContainer::Iterator   derives = myDerives->Begin();

  typename TInputMesh::PointsContainerPointer      myDisplacements = Displacements->GetPoints();
  typename TInputMesh::PointsContainer::Iterator   displacements = myDisplacements->Begin();  

  typename TInputMesh::PointsContainerPointer      myPoints = Locations->GetPoints();
  typename TInputMesh::PointsContainer::Iterator   points = myPoints->Begin();
 
  i = 0;
  while( derives != myDerives->End() ) {
	d = displacements.Value();
    ds = derives.Value();
	s = points.Value();
	++derives; 
    s[0] += TimeStep*ds[0]; 
    s[1] += TimeStep*ds[1]; 
    s[2] += TimeStep*ds[2]; 
	d[0] += TimeStep*ds[0]; 
    d[1] += TimeStep*ds[1]; 
    d[2] += TimeStep*ds[2]; 
	if ( i < m_NumNodes - 2 ) {
	  displacements.Value() = d;
	  points.Value() = s;
	}
    ++displacements;
	++points;
	++i;
  } 

/*  

  displacements = myDisplacements->Begin();  
  
  while( displacements != myDisplacements->End() ) {
	d = displacements.Value();
    s = points.Value();
	++displacements;
    s[0] += d[0]; 
    s[1] += d[1]; 
    s[2] += d[2]; 
    points.Value() = s;
	++points;
  }
*/   
}

template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::ComputeOutput() {
  int i;
  m_Output = this->GetOutput();
  typename TInputMesh::PointsContainerPointer		myPoints = m_Output->GetPoints();
  myPoints->Reserve(m_NumNodes);
  typename TInputMesh::PointsContainer::Iterator	points = myPoints->Begin();

  typename TInputMesh::PointsContainerPointer		myLocations = Locations->GetPoints();
  typename TInputMesh::PointsContainer::Iterator	locations = myLocations->Begin();
  i = 0;
  for (; i<m_NumNodes; i++) {
	points.Value() = locations.Value();
	++locations;
	++points;
  } 

}

template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::GapSearch() {
  typename TInputMesh::PointsContainerPointer Points;
  Points = Locations->GetPoints();
  typename TInputMesh::PointsContainer::Iterator   points = Points->Begin();
//  const int npoints = Points->GetNumberOfPoints(); 

  typename TInputMesh::PointDataContainerPointer	myPointData = Locations->GetPointData();
  typename TInputMesh::PointDataContainer::Iterator	pointstatus = myPointData->Begin();

  int i, q;
  IPT x, y, z;
  IPT* y_pt;
  IPT* z_pt;
  y_pt = &y;
  z_pt = &z;
  float gap, dis[3]={0, 0, 0}, st, *st_pt;
  st_pt = &st;

  i = 0;
  while( i != m_NumNodes - 2 )
  { 
	x = points.Value();
	q = (i < m_NumNodes-3)?i+1:0;
    Locations->GetPoint (q, y_pt);
	
	dis[0] = x[0] - y[0];
	dis[1] = x[1] - y[1];
	gap = sqrt(dis[0]*dis[0]+dis[1]*dis[1]);
	
	Locations->GetPointData(q, st_pt);
	if ( (gap > 5) && ( pointstatus.Value() == 1.0) && (st==1.0)) NodeAddition(i);
	i++;
	++points;
	++pointstatus;
  }
  
  Reset();	 	
	  
}

template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::NodeAddition(int i) {
//  typename TInputMesh::PointsContainerPointer Points;
//  Points = Locations->GetPoints();
//  const int npoints = Points->GetNumberOfPoints();

  int q, label; 
  IndexType coord={0,0,0};
  float newnodes[5][3];
  IPT x, y;
  IPT* x_pt;
  IPT* y_pt;
  x_pt = &x;
  y_pt = &y;

  q = (i < m_NumNodes-3)?i+1:0;
  Locations->GetPoint (i, x_pt);
  Locations->GetPoint (q, y_pt);

  coord[2] = 0;
  for (int j=0; j<5; j++) {
	newnodes[j][0] = (y[0]*(j+1)+x[0]*(5-j))/6;
	newnodes[j][1] = (y[1]*(j+1)+x[1]*(5-j))/6;
	newnodes[j][2] = 0;
	coord[0] = (int) (newnodes[j][0]);
	coord[1] = (int) (newnodes[j][1]);
	label = Potential->GetPixel(coord);
	if ( label == 64000 ) {
	  m_NewNodes[m_NumNewNodes][0] = newnodes[j][0];
	  m_NewNodes[m_NumNewNodes][1] = newnodes[j][1];
	  m_NewNodes[m_NumNewNodes][2] = 0;
	  m_NewNodes[m_NumNewNodes++][3] = (float) i;
	}
  }

}


} // end namespace itk
