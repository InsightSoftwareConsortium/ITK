/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBalloonForceFilter.txx
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
#ifndef _itkBalloonForceFilter_txx
#define _itkBalloonForceFilter_txx

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
 * to build the stiffness matrix
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetStiffness(double a, double b)
{
  m_Stiffness[0] = a; 
  m_Stiffness[1] = b; 
}

template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetCenter(int a, int b, int c)
{
  m_Center[0] = a; 
  m_Center[1] = b; 
  m_Center[2] = c;
}

/**
 * store the force vectors on each node into a mesh 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetForces(TInputMesh* force)
{
  m_Forces = force; 
}

/**
 * store the displacement vectors on each node into a mesh 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetDisplacements(TInputMesh* displace)
{
  m_Displacements = displace; 
}

/**
 * set the input binary image 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetPotential(ImagePointer potential)
{
  m_Potential = potential; 
}

/**
 * set the output model region image 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetImageOutput(ImagePointer outputimg)
{
  m_ImageOutput = outputimg; 
}

/**
 * store the normal vectors on each node into a mesh 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetNormals(TInputMesh* normals)
{
  m_Normals = normals; 
}

/**
 * location serve as a copy of the income mesh, and on each
 * node there is a sign to show if it is stopped, which is 
 * stored in the pointdatacontainer 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetLocations(TInputMesh* location)
{
  m_Locations = location; 
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
  m_Derives = derive; 
}

/**
 * set the resolutions of the model
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetResolution(int a, int b, int c)
{
  m_Resolution[0] = a; 
  m_Resolution[1] = b;
  m_Resolution[2] = c;
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

  InputPointsContainerPointer			myPoints = m_Input->GetPoints();
  InputPointsContainerIterator  		points = myPoints->Begin();
	
  InputPointsContainerPointer   		myForces = m_Forces->GetPoints();
  myForces->Reserve(m_NumNodes);
  InputPointsContainerIterator			forces = myForces->Begin();

  InputPointsContainerPointer   		myDerives = m_Derives->GetPoints();
  myDerives->Reserve(m_NumNodes);
  InputPointsContainerIterator			derives = myDerives->Begin();

  InputPointsContainerPointer   		myDisplacements = m_Displacements->GetPoints();
  myDisplacements->Reserve(m_NumNodes);
  InputPointsContainerIterator			displacements = myDisplacements->Begin();

  InputPointsContainerPointer   		myNormals = m_Normals->GetPoints();
  myNormals->Reserve(m_NumNodes);
  InputPointsContainerIterator			normals = myNormals->Begin();

  InputPointsContainerPointer   		myLocations = m_Locations->GetPoints();
  myLocations->Reserve(m_NumNodes);
  InputPointsContainerIterator			locations = myLocations->Begin();

  InputCellsContainerPointer			myCells = m_Input->GetCells();
  InputCellsContainerIterator			cells = myCells->Begin(); 
  
  InputCellDataContainerPointer			myCellData = m_Input->GetCellData();
  InputCellDataContainerIterator		celldata = myCellData->Begin(); 
  
  m_Stiffness[0] = 0.000001; 
  m_Stiffness[1] = 0.04; 
  m_NumNewNodes = 0;
  m_ObjectLabel = m_Potential->GetPixel(m_Center);

  PotentialSizeType PotentialSize = m_Potential->GetBufferedRegion().GetSize();
  //---------------------------------------------------------------------
  //Get the image width/height and depth
  //---------------------------------------------------------------------       
  m_imgWidth  = PotentialSize[0];
  m_imgHeight = PotentialSize[1];
  m_imgDepth  = PotentialSize[2];

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
	m_Forces->SetPointData(i, 1.0);
	m_Locations->SetPointData(i, 0.0);
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

  TriCell::Pointer insertCell = TriCell::New();
  unsigned long tripoints[3];
  const unsigned long *tp;
  float x;

  for (int i=0; i<m_NumCells; i++) {
	tp = cells.Value()->GetPointIds();
	tripoints[0] = tp[0];
	tripoints[1] = tp[1];
	tripoints[2] = tp[2];
	insertCell->SetPointIds(tripoints);
	m_Locations->SetCell(i, insertCell);
	x = celldata.Value();
	m_Locations->SetCellData(i, (PT)x);
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
  InputCellDataContainerPointer			myCellData = m_Locations->GetCellData();
  InputCellDataContainerIterator		celldata = myCellData->Begin();

  K = (vnl_matrix_fixed<double,4,4>**) malloc(sizeof(vnl_matrix_fixed<double,4,4>*)*m_NumCells);
  float x;	

  float us = vnl_math::pi / 9; 
  float vs = 2.0*vnl_math::pi / 18; 
  float a = us*us, b = vs*vs; 
  float area = us*vs/2, k00, k01, k02, k11, k12, k22; 
 
  k00 = area * (m_Stiffness[1]/a + m_Stiffness[1]/b + m_Stiffness[0]); 
  k01 = area * (-m_Stiffness[1]/a + m_Stiffness[0]); 
  k02 = area * (-m_Stiffness[1]/b + m_Stiffness[0]); 
  k11 = area * (m_Stiffness[1]/a + m_Stiffness[0]); 
  k12 = area * m_Stiffness[0]; 
  k22 = area * (m_Stiffness[1]/b + m_Stiffness[0]); 
 
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
     
  k00 = area * (m_Stiffness[1]/a + m_Stiffness[0]); 
  k01 = area * (-m_Stiffness[1]/a + m_Stiffness[0]); 
  k02 = area * m_Stiffness[0]; 
  k11 = area * (m_Stiffness[1]/a + m_Stiffness[1]/b+m_Stiffness[0]); 
  k12 = area * (-m_Stiffness[1]/b + m_Stiffness[0]); 
  k22 = area * (m_Stiffness[1]/b + m_Stiffness[0]); 
     
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
 
  k00 = area * (m_Stiffness[1]/b + m_Stiffness[0]); 
  k01 = area * (-m_Stiffness[1]/b + m_Stiffness[0]); 
  k02 = area * m_Stiffness[0]; 
  k11 = area * (m_Stiffness[1]/a + m_Stiffness[1]/b + m_Stiffness[0]); 
  k12 = area * (-m_Stiffness[1]/a + m_Stiffness[0]); 
  k22 = area * (m_Stiffness[1]/a + m_Stiffness[0]); 
 
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
  int i, p, q, label, l=0; 
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

  InputPointsContainerPointer			Points = m_Locations->GetPoints();
  InputPointsContainerIterator			points = Points->Begin();

  InputPointsContainerPointer			myForces = m_Forces->GetPoints();
  InputPointsContainerIterator			forces = myForces->Begin();

  InputPointsContainerPointer			myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator			normals = myNormals->Begin();

  InputPointDataContainerPointer		myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator		forcedata = myForceData->Begin();

  InputPointDataContainerPointer		myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator		pointstatus = myPointData->Begin();
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

  int slicediv = m_NumNodes / this->m_Resolution[0]; 
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
      m_Locations->GetPoint (q, y_pt);  
      p = (i == 0)?m_NumNodes-3:i-1;
      m_Locations->GetPoint (p, z_pt);
	}

	coord[0] = (int) x[0];
	coord[1] = (int) x[1];
	coord[2] = (int) x[2];
	if ( m_Potential->GetPixel(coord) != m_ObjectLabel ) {
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
	m_MiniT = 20;

	while (t < 20.0){
	  extends[0] += r[0];
	  extends[1] += r[1];
      extend[0] = (int) extends[0];
      extend[1] = (int) extends[1];
	  if ((extend[0] < 0) || (extend[1] < 0) || (extend[0] >= m_imgWidth)
		|| (extend[1] >= m_imgHeight)) break;

	  label = m_Potential->GetPixel(extend);
	  if ( label != m_ObjectLabel ) break;

	  if (r[0] >= 0) extend[0] = (int)(extends[0]-1);
	  else  extend[0] = (int)(extends[0]+1);
	  extend[1] = (int) extends[1];
	  label = m_Potential->GetPixel(extend);
	  if ( label != m_ObjectLabel ) break;

	  if (r[1] >= 0) extend[1] = (int)(extends[1]-1);
	  else  extend[1] = (int)(extends[1]+1);  
	  extend[0] = (int) extends[0];
	  label = m_Potential->GetPixel(extend);
	  if ( label != m_ObjectLabel ) break;

	  t += 1.0;
	}

	if (t < 2) pointstatus.Value() = 1.0;
	else {
	  pointstatus.Value() = 0.0;
	  m_ImageOutput->SetPixel(coord, 1);
	}
	
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
  typename TriCell::Pointer testCell = TriCell::New();
  int npts = 3;

  InputCellsContainerPointer		myCells = m_Locations->GetCells();
  InputCellsContainerIterator		cells = myCells->Begin();

  InputPointsContainerPointer	    myForces = m_Forces->GetPoints();
  InputPointsContainerIterator		forces = myForces->Begin();

  InputPointsContainerPointer		myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator		derives = myDerives->Begin();

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
    m_Displacements->GetPoint (tp[0], v1_pt); 
    m_Displacements->GetPoint (tp[1], v2_pt); 
    m_Displacements->GetPoint (tp[2], v3_pt); 
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
    m_Forces->GetPoint (tp[0], v2_pt); 

	v2[0] -= v1[0]; 
	v2[1] -= v1[1]; 
	v2[2] -= v1[2];

    m_Forces->SetPoint (tp[0], v2); 
 
    m_Displacements->GetPoint (tp[0], v1_pt); 
    m_Displacements->GetPoint (tp[1], v2_pt); 
    m_Displacements->GetPoint (tp[2], v3_pt); 
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
    m_Forces->GetPoint (tp[1], v2_pt);  

	v2[0] -= v1[0]; 
	v2[1] -= v1[1]; 
	v2[2] -= v1[2];

    m_Forces->SetPoint (tp[1], v2); 
 
    m_Displacements->GetPoint (tp[0], v1_pt); 
    m_Displacements->GetPoint (tp[1], v2_pt); 
    m_Displacements->GetPoint (tp[2], v3_pt); 
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
    m_Forces->GetPoint (tp[2], v2_pt); 

	v2[0] -= v1[0]; 
	v2[1] -= v1[1]; 
	v2[2] -= v1[2];

    m_Forces->SetPoint (tp[2], v2);  
	++i;
  } 

  while ( derives != myDerives->End() ) {
	derives.Value() = forces.Value();
	++derives; 
	++forces;
  }
   
}

/**
 * When there is new nodes added, must do a reset to reallocate
 * the memory and redistribute the nodes and reconstruct the cells,
 * now the mthod is only suitable for 2D models, it will be a much
 * different case for 3D model 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::Reset()
{
  int i, j, cell=0; 
  float status, d[3] = {0,0,0}, z;
  IPT x;
  IPT* x_pt; 
  x_pt = &x;
  unsigned long tripoints[3];
 
  if (m_NumNewNodes == 0) return;
  else cell = 1;

  j = m_NumNodes;;
  m_NumNodes = m_NumNodes + m_NumNewNodes;
  i = m_NumNodes;
  
  InputPointsContainerPointer			myForces = m_Forces->GetPoints();
  myForces->Reserve(m_NumNodes);
  InputPointsContainerIterator			forces = myForces->Begin();

  InputPointsContainerPointer			myPoints = m_Locations->GetPoints();
  myPoints->Reserve(m_NumNodes);
  InputPointsContainerIterator			points = myPoints->Begin();
 
  InputPointsContainerPointer			myNormals = m_Normals->GetPoints();
  myNormals->Reserve(m_NumNodes);
  InputPointsContainerIterator			normals = myNormals->Begin();

  InputPointsContainerPointer			myDerives = m_Derives->GetPoints();
  myDerives->Reserve(m_NumNodes);
  InputPointsContainerIterator			derives = myDerives->Begin();

  InputPointsContainerPointer			myDisplacements = m_Displacements->GetPoints();
  myDisplacements->Reserve(m_NumNodes);
  InputPointsContainerIterator			displacements = myDisplacements->Begin();

  InputPointDataContainerPointer		myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator		forcedata = myForceData->Begin();
   
  typename TriCell::Pointer				insertCell = TriCell::New(); 

  while ( m_NumNewNodes != 0) {
	if ( (j-1) > (int)(m_NewNodes[m_NumNewNodes-1][3]) ) {
	  m_Locations->GetPoint(--j, x_pt);
	  status = 0.0;
	}
	else {
	  m_NumNewNodes--;
	  x[0] = m_NewNodes[m_NumNewNodes][0];
	  x[1] = m_NewNodes[m_NumNewNodes][1];
	  x[2] = m_NewNodes[m_NumNewNodes][2];
	  status = 1.0;
    }
	
	m_Locations->SetPoint(--i, x);
	m_Forces->SetPointData(i, status);
	m_Locations->SetPointData(i, 0.0);
	if (status == 0.0) {
	  m_Normals->GetPoint(j, x_pt);
	  m_Normals->SetPoint(i, x);
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
	  m_Locations->SetCell(p, insertCell);
	  m_Locations->SetCellData(p, (PT)1.0);
	  p++;
	  insertCell = TriCell::New();
	  tripoints[2] = (m_Resolution[0]-1)*m_Resolution[1]+j; 
	  tripoints[1] = m_NumNodes-1; 
      tripoints[0] = tripoints[2]-j+jn; 
	  insertCell->SetPointIds(tripoints);
	  m_Locations->SetCell(p, insertCell);
	  m_Locations->SetCellData(p, (PT)2.0);
	  p++;
	  insertCell = TriCell::New();
	}
	
	m_NumCells = p;

	K = (vnl_matrix_fixed<double,4,4>**) 
		malloc(sizeof(vnl_matrix_fixed<double,4,4>*)*m_NumCells);
  
	InputCellDataContainerIterator		celldata = m_Locations->GetCellData()->Begin();

	int j=0;
	while (celldata != m_Locations->GetCellData()->End()){
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
 * update the displacements using d_{new} = d_{old} + timestep*d' 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::Advance()
{
  typename TInputMesh::PointType s, d, ds; 

  int i;
  float x;

  InputPointsContainerPointer		myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator		derives = myDerives->Begin();

  InputPointsContainerPointer		myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator		displacements = myDisplacements->Begin();  

  InputPointsContainerPointer		myPoints = m_Locations->GetPoints();
  InputPointsContainerIterator		points = myPoints->Begin();
 
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
	  if (s[0] < 0) {
		x = s[0];
		s[0] = 0;
		d[0] -= x;
	  }
	  if (s[1] < 0) {
		x = s[1];
		s[1] = 0;
		d[1] -= x;
	  }
	  if (s[0] > m_imgWidth) {
		x = s[0] - m_imgWidth;
		s[0] = m_imgWidth-0.001;
		d[0] -= x;
	  }
	  if (s[1] > m_imgHeight) {
		x = s[1] - m_imgHeight;
		s[1] = m_imgHeight-0.001;
		d[1] -= x;
	  }
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

/**
 * copy the content of m_Location into output 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::ComputeOutput() 
{
  int i;
  m_Output = this->GetOutput();

  InputPointsContainerPointer		myPoints = m_Output->GetPoints();
  myPoints->Reserve(m_NumNodes);
  InputPointsContainerIterator		points = myPoints->Begin();

  InputPointsContainerPointer		myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator		locations = myLocations->Begin();

  i = 0;
  for (; i<m_NumNodes; i++) {
	points.Value() = locations.Value();
	++locations;
	++points;
  } 

}

/**
 * when almost all the nodes is at the estimated boundary, use
 * gapsearch to fit the model to more complicated shapes
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::GapSearch() 
{
  InputPointsContainerPointer		Points = m_Locations->GetPoints();
  InputPointsContainerIterator		points = Points->Begin();

  InputPointDataContainerPointer	myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator	pointstatus = myPointData->Begin();

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
    m_Locations->GetPoint (q, y_pt);
	
	dis[0] = x[0] - y[0];
	dis[1] = x[1] - y[1];
	gap = sqrt(dis[0]*dis[0]+dis[1]*dis[1]);
	
	m_Locations->GetPointData(q, st_pt);
	if ( (gap > 5) && ( pointstatus.Value() == 1.0) && (st==1.0)) NodeAddition(i);
	i++;
	++points;
	++pointstatus;
  }
  
  Reset();	 	
	  
}

/**
 * add new nodes into the model 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::NodeAddition(int i) 
{

  int q, label; 
  IndexType coord={0,0,0};
  float newnodes[5][3];
  IPT x, y;
  IPT* x_pt;
  IPT* y_pt;
  x_pt = &x;
  y_pt = &y;

  q = (i < m_NumNodes-3)?i+1:0;
  m_Locations->GetPoint (i, x_pt);
  m_Locations->GetPoint (q, y_pt);

  coord[2] = 0;
  for (int j=0; j<5; j++) {
	newnodes[j][0] = (y[0]*(j+1)+x[0]*(5-j))/6;
	newnodes[j][1] = (y[1]*(j+1)+x[1]*(5-j))/6;
	newnodes[j][2] = 0;
	coord[0] = (int) (newnodes[j][0]);
	coord[1] = (int) (newnodes[j][1]);
	label = m_Potential->GetPixel(coord);
	if ( label == m_ObjectLabel ) {
	  m_NewNodes[m_NumNewNodes][0] = newnodes[j][0];
	  m_NewNodes[m_NumNewNodes][1] = newnodes[j][1];
	  m_NewNodes[m_NumNewNodes][2] = 0;
	  m_NewNodes[m_NumNewNodes++][3] = (float) i;
	}
  }

}


} // end namespace itk

#endif
