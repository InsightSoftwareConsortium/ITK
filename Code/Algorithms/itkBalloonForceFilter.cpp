/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBalloonForceFilter.cpp
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
  Locations = this->GetInput(0);
  Stiffness[0] = 0.000001; 
  Stiffness[1] = 0.04; 
  const int npoints = Locations->GetNumberOfPoints()-2;
  int i; 
  float x[3], d[3] = {0,0,0}; 

  TimeStep = 0.001; 

  for (i = 0; i < npoints; i++) Forces->SetPoint (i, d);

  for (i = 0; i < npoints; i++) Derives->SetPoint (i, d);
 
  for (i = 0; i < npoints; i++) Displacements->SetPoint (i, d); 
 
} 

/**
 * set the stiffness matrix 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetStiffnessMatrix () 
{ 

  TInputMesh::CellDataContainerPointer CellType;
  TInputMesh::CellDataContainerIterator CellTypeIterator;
  unsigned long CellNum;
  CellType = Locations->GetCellData();
  CellNum = Locations->GetNumberOfCells();
  K = (vnl_matrix_fixed<double,4,4>**) malloc(sizeof(vnl_matrix_fixed<double,4,4>*)*CellNum);
  float *x, x1;	
  x = &x1;

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
  for(j=1; j<CellNum; j++) {
	Locations->GetCellData(j, (PT*)x);
	switch ((int)(x1)) { 
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
  int i, potential_ijk[3], potential_dims[3], potential_dis[3], p, q, t, sign, l=0; 
  float potential_coords[3], xs, ys, zs, s, srange[2], x1, y1; 
  FloatVector n1, n; 
  float x[3], y[3], z[3], ntemp[3], f[3]={0, 0, 0};

  TInputMesh::PointsContainerPointer Points;
  Points = Locations->GetPoints();

  const int npoints = Forces->GetNumberOfPoints(); 
  int slicediv = npoints / this->Resolution[0]; 
  float pxx, pyy, pxy, qxx, qyy, qxy, r[2], dis[2];
  double d=0.0, e, f1=0.0;

 
  for (i = 0; i < npoints; i++) { 
    int slice = i / slicediv; 
    xs = ys = zs = 1.0; 
    Locations->GetPoint (i, (IPT*)(x)); 
    q = (i < npoints-1)?i+1:0;
    Locations->GetPoint (q, (IPT*)(y));  
    p = (i == 0)?npoints-1:i-1;
    Locations->GetPoint (p, (IPT*)(z));

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

    f[0] = 400*n[0]; 
    f[1] = 400*n[1];

    f[2] = 0;

    Forces->SetPoint (i, f);  

  }

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
  Cell::Pointer testCell(TriCell::New());
  int npts = 3, *pts;
  TInputMesh::CellsContainerPointer Cells;
  Cells = Locations->GetCells();

  const int ncells = Locations->GetNumberOfCells();

  const int npoints = Forces->GetNumberOfPoints();  
  float p = 1; 

  float v1[3], v2[3], v3[3];
  for (i = 1; i < ncells; i++) { 
    Locations->GetCell(i, &testCell);
	tp=testCell->GetPointIds();
    Displacements->GetPoint (tp[0], (IPT*)v1); 
    Displacements->GetPoint (tp[1], (IPT*)v2); 
    Displacements->GetPoint (tp[2], (IPT*)v3); 
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
    Forces->GetPoint (tp[0], (IPT*)v2); 
    if ((v2[0]==0) && (v2[1]==0)){}
    else {
	v2[0] -= v1[0]; 
	v2[1] -= v1[1]; 
	v2[2] -= v1[2];
    } 
    Derives->SetPoint (tp[0], v2); 
 
    Displacements->GetPoint (tp[0], (IPT*)v1); 
    Displacements->GetPoint (tp[1], (IPT*)v2); 
    Displacements->GetPoint (tp[2], (IPT*)v3); 
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
    Forces->GetPoint (tp[1], (IPT*)v2);  
    if ((v2[0]==0) && (v2[1]==0)){}
    else {
	v2[0] -= v1[0]; 
	v2[1] -= v1[1]; 
	v2[2] -= v1[2];
    } 
    v2[0] -= v1[0]; 
    v2[1] -= v1[1]; 
    v2[2] -= v1[2]; 
    Derives->SetPoint (tp[1], v2); 
 
    Displacements->GetPoint (tp[0], (IPT*)v1); 
    Displacements->GetPoint (tp[1], (IPT*)v2); 
    Displacements->GetPoint (tp[2], (IPT*)v3); 
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
    Forces->GetPoint (tp[2], (IPT*)v2); 
    if ((v2[0]==0) && (v2[1]==0)){}
    else {
	v2[0] -= v1[0]; 
	v2[1] -= v1[1]; 
	v2[2] -= v1[2];
    } 
    v2[0] -= v1[0]; 
    v2[1] -= v1[1]; 
    v2[2] -= v1[2]; 
    Derives->SetPoint (tp[2], v2); 
    
  } 
   

}

template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::Reset()
{
  const int npoints = Locations->GetNumberOfPoints()-2;
    int i; 
  float x[3], d[3] = {0,0,0}; 
 
  for (i = 0; i < npoints; i++) { 
    Displacements->SetPoint (i, d); 
    Derives->SetPoint (i, d); 
    Forces->SetPoint (i, d); 
//    Locations->SetPoint (i, x); 
  } 
}

/**
 * update the displacements using dnew = dold + timestep*d' 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::Advance(){
  float s[3], ds[3]; 
  const int npoints = Forces->GetNumberOfPoints(); 
  for (int i = 0; i < npoints; i++) { 
    Displacements->GetPoint (i, (IPT*)s); 
    Derives->GetPoint (i, (IPT*)ds); 
    s[0] += TimeStep*ds[0]; 
    s[1] += TimeStep*ds[1]; 
    s[2] += TimeStep*ds[2]; 
    Displacements->SetPoint (i, s); 
  } 
}

template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::ComputeOutput(){
  float s[3], d[3]; 
  const int npoints = Forces->GetNumberOfPoints(); 
  for (int i = 0; i < npoints; i++) { 
    Displacements->GetPoint (i, (IPT*)d); 
    Locations->GetPoint (i, (IPT*)s); 
    s[0] += d[0]; 
    s[1] += d[1]; 
    s[2] += d[2]; 
    Locations->SetPoint (i, s); 
  } 
}

} // end namespace itk
