/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableMesh3DFilter.txx
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
#ifndef _itkDeformableMesh3DFilter_txx
#define _itkDeformableMesh3DFilter_txx

#include "itkDeformableMesh3DFilter.h"

namespace itk
{
/**
 * standard 
 */
template <typename TInputMesh, typename TOutputMesh>
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::DeformableMesh3DFilter()
{
  m_Step = 0;
  m_FirstSlice = 0;
  m_NewNode = 0;
  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
}


/**
 * PrintSelf
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Balloon Force Filter" << std::endl;

}// end PrintSelf

/**
 * set default value of parameters and initialize local data container such as forces,
 * displacements and displacement derivatives
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::Initialize()
{
  m_Input = this->GetInput(0);
  m_NumNodes = m_Input->GetNumberOfPoints();
  m_NumCells = m_Input->GetNumberOfCells();

  m_Forces = InputMeshType::New();
  m_Displacements = InputMeshType::New();
  m_Derives = InputMeshType::New();
  m_Normals = InputMeshType::New();
  m_Locations = InputMeshType::New();

  InputPointsContainerPointer      myPoints = m_Input->GetPoints();
  InputPointsContainerIterator     points = myPoints->Begin();

  InputPointsContainerPointer      myForces = m_Forces->GetPoints();
  myForces->Reserve(m_NumNodes);
  InputPointsContainerIterator     forces = myForces->Begin();

  InputPointsContainerPointer      myDerives = m_Derives->GetPoints();
  myDerives->Reserve(m_NumNodes);
  InputPointsContainerIterator     derives = myDerives->Begin();

  InputPointsContainerPointer      myDisplacements = m_Displacements->GetPoints();
  myDisplacements->Reserve(m_NumNodes);
  InputPointsContainerIterator     displacements = myDisplacements->Begin();

  InputPointsContainerPointer      myNormals = m_Normals->GetPoints();
  myNormals->Reserve(m_NumNodes);
  InputPointsContainerIterator     normals = myNormals->Begin();

  InputPointsContainerPointer      myLocations = m_Locations->GetPoints();
  myLocations->Reserve(m_NumNodes);
  InputPointsContainerIterator     locations = myLocations->Begin();

  InputCellsContainerPointer       myCells = m_Input->GetCells();
  InputCellsContainerIterator      cells = myCells->Begin(); 
  
  InputCellDataContainerPointer    myCellData = m_Input->GetCellData();
  InputCellDataContainerIterator   celldata = myCellData->Begin(); 

  m_NumNewNodes = 0;
  m_NewNodesExisted = 0;
  m_NewNodeLimit = 200;
  m_ObjectLabel = m_Potential->GetPixel(m_Center);
  PotentialSizeType PotentialSize = m_Potential->GetBufferedRegion().GetSize();

  //---------------------------------------------------------------------
  //Get the image width/height and depth
  //---------------------------------------------------------------------   
  m_imgWidth  = PotentialSize[0];
  m_imgHeight = PotentialSize[1];
  m_imgDepth  = PotentialSize[2];

  InputPointType d;
  d[0] = 0.0;
  d[1] = 0.0;
  d[2] = 0.0; 

  while( points != myPoints->End() ) {
    locations.Value() = points.Value();
    ++points;
    ++locations;
  }

  while( forces != myForces->End() ) {
    forces.Value() = d;
    ++forces;
  }

  while( normals != myNormals->End() ) {
    normals.Value() = d;
    ++normals;
  }

  for (int i=0; i<m_NumNodes-2; i++  ) {
    m_Forces->SetPointData(i, 1.0);
    m_Locations->SetPointData(i, 0.0);
    m_Derives->SetPointData(i, 0.0);
    m_Displacements->SetPointData(i, 0.0);
  }
   
  while( derives != myDerives->End() ) {
    derives.Value() = d;
    ++derives;
  }

  while( displacements != myDisplacements->End() ) {
    displacements.Value() = d;
    ++displacements;
  }

  TriCell::Pointer insertCell;
  unsigned long tripoints[3];
  const unsigned long *tp;
  double x;

  for (int i=0; i<m_NumCells; i++) {
    tp = cells.Value()->GetPointIds();
    tripoints[0] = tp[0];
    tripoints[1] = tp[1];
    tripoints[2] = tp[2];
    insertCell = TriCell::New();
    insertCell->SetPointIds(tripoints);
    m_Locations->SetCell(i, insertCell);
    x = celldata.Value();
    m_Locations->SetCellData(i, (PixelType)x);
    ++cells;
    ++celldata;
  }
} 

/**
 * set the stiffness matrix 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::SetStiffnessMatrix () 
{ 
  InputCellDataContainerPointer    myCellData = m_Locations->GetCellData();
  InputCellDataContainerIterator   celldata = myCellData->Begin();

  K = (vnl_matrix_fixed<double,4,4>**) malloc(sizeof(vnl_matrix_fixed<double,4,4>*)*m_NumCells);
  double x;

  double us = vnl_math::pi / m_XResolution; 
  double vs = 2.0*vnl_math::pi / m_YResolution; 
  double a = us*us, b = vs*vs; 
  double area = us*vs/2, k00, k01, k02, k11, k12, k22; 
 
  k00 = area * (m_StiffnessH/a + m_StiffnessH/b + m_StiffnessV); 
  k01 = area * (-m_StiffnessH/a + m_StiffnessV); 
  k02 = area * (-m_StiffnessH/b + m_StiffnessV); 
  k11 = area * (m_StiffnessH/a + m_StiffnessV); 
  k12 = area * m_StiffnessV; 
  k22 = area * (m_StiffnessH/b + m_StiffnessV); 
 
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
   
  k00 = area * (m_StiffnessH/a + m_StiffnessV); 
  k01 = area * (-m_StiffnessH/a + m_StiffnessV); 
  k02 = area * m_StiffnessV; 
  k11 = area * (m_StiffnessH/a + m_StiffnessH/b+m_StiffnessV); 
  k12 = area * (-m_StiffnessH/b + m_StiffnessV); 
  k22 = area * (m_StiffnessH/b + m_StiffnessV); 
   
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
 
  k00 = area * (m_StiffnessH/b + m_StiffnessV); 
  k01 = area * (-m_StiffnessH/b + m_StiffnessV); 
  k02 = area * m_StiffnessV; 
  k11 = area * (m_StiffnessH/a + m_StiffnessH/b + m_StiffnessV); 
  k12 = area * (-m_StiffnessH/a + m_StiffnessV); 
  k22 = area * (m_StiffnessH/a + m_StiffnessV); 
 
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

  int j = 0;
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
 
    j++;
  }
} 

/**
 * compute the model start points
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::InitialFit()
{
  int i; 
  IndexType coord = {0, 0, 0};
  IndexType extend = {0, 0, 0};
  double xs, ys, zs; 
  InputPointType x, y, z, f, n, extends;

  InputPointsContainerPointer      Points = m_Locations->GetPoints();
  InputPointsContainerIterator     points = Points->Begin();

  InputPointsContainerPointer      myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator     derives = myDerives->Begin();

  InputPointsContainerPointer      myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator     normals = myNormals->Begin();

  InputPointDataContainerPointer   myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator  forcedata = myForceData->Begin();

  InputPointDataContainerPointer   myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator  pointstatus = myPointData->Begin();

  i = 0;
  
  while( i < m_NumNodes ) {
    xs = ys = zs = 1.0; 
    x = points.Value();
    n = normals.Value();

    coord[0] = (int) x[0];
    coord[1] = (int) x[1];
    coord[2] = (int) x[2];   //slice + m_FirstSlice;

    extends[0] = x[0];
    extends[1] = x[1];
    extends[2] = x[2];
    extend[0] = (int) x[0];
    extend[1] = (int) x[1];
    extend[2] = (int) x[2];

    if ( (coord[0]>0)&&(coord[0]<m_imgWidth)&&(coord[1]>0)&&
        (coord[1]<m_imgHeight)&&(coord[2]>0)&&(coord[2]<m_imgDepth)) {
      if ( m_Potential->GetPixel(coord) != m_ObjectLabel ) {
        xs = ys = zs = 0.0;
      }
    }

    f[0] = xs*n[0]*1000;
    f[1] = ys*n[1]*1000;
    f[2] = zs*n[2]*1000;

    derives.Value() = f; 

    ++derives;
    ++forcedata;
    ++points;
    ++normals;
    ++i;
  }
}

/**
 * compute the shrink force when the model is shrink to fit to the objects.
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeShrinkForce()
{
  int i, l=0;
  unsigned short label; 
  IndexType coord = {0, 0, 0};
  IndexType extend = {0, 0, 0};
  double max, fo, t, xs, ys, zs; 
  InputPointType x, y, z, f, n, extends;

  InputPointsContainerPointer  Points = m_Locations->GetPoints();
  InputPointsContainerIterator points = Points->Begin();

  InputPointsContainerPointer  myForces = m_Forces->GetPoints();
  InputPointsContainerIterator forces = myForces->Begin();

  InputPointsContainerPointer  myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator normals = myNormals->Begin();

  InputPointDataContainerPointer  myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator forcedata = myForceData->Begin();

  InputPointDataContainerPointer  myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator pointstatus = myPointData->Begin();

  i = 0;
  
  while ( i < m_NumNodes ) {
    xs = ys = zs = -1.0; 
    x = points.Value();

    coord[0] = (int) x[0];
    coord[1] = (int) x[1];
    coord[2] = (int) x[2];

    if ( (coord[0]>0)&&(coord[0]<m_imgWidth)&&(coord[1]>0)&&
        (coord[1]<m_imgHeight)&&(coord[2]>0)&&(coord[2]<m_imgDepth)) {
      if ( m_Potential->GetPixel(coord) == m_ObjectLabel ) {
        xs = ys = zs = 0.0;
      }
    }

    extends[0] = x[0];
    extends[1] = x[1];
    extends[2] = x[2];
    extend[0] = (int) x[0];
    extend[1] = (int) x[1];
    extend[2] = (int) x[2];

    f = normals.Value();
    max = abs(f[0]);

  //---------------------------------------------------------------------
  // all the movement in z direction is now disabled for further test
  //---------------------------------------------------------------------  
    if ( abs(f[1]) > max ) max = abs(f[1]);
    if ( abs(f[2]) > max ) max = abs(f[2]);
    n[0] = f[0]/max;
    n[1] = f[1]/max;
    n[2] = f[2]/max;

    t = 0.0;

    while (t < 5.0){
      extends[0] -= n[0];
      extends[1] -= n[1];
      extends[2] -= n[2];
      extend[0] = (int) (extends[0]+1);
      extend[1] = (int) (extends[1]+1);
      extend[2] = (int) (extends[2]+1);  
      if ((extend[0] <= 0) || (extend[1] <= 0) || (extend[2] <= 0)) {
        t += 1.0;
        continue;
      }

      extend[0] = (int) (extends[0]);
      extend[1] = (int) (extends[1]);
      extend[2] = (int) (extends[2]);
      if ((extend[0] >= m_imgWidth) || (extend[1] >= m_imgHeight) || 
          (extend[2] >= m_imgDepth)) {
        t += 1.0;
        continue;
      }

      label = m_Potential->GetPixel(extend);
      if ( label == m_ObjectLabel ) break;

      t += 1.0;
    }

    if (t < 2.0) pointstatus.Value() = 1.0;
    else {
      pointstatus.Value() = 0.0;
      m_ImageOutput->SetPixel(coord, 1);
    }

    fo = sqrt(f[0]*f[0]+f[1]*f[1]+f[2]*f[2]);
    f[0] = t*400*f[0]*xs/fo; 
    f[1] = t*400*f[1]*ys/fo;
    f[2] = t*400*f[2]*zs/fo;

    forces.Value() = f;
    forcedata.Value() = 0.0;
    ++pointstatus;
    ++forces;
    ++forcedata;
    ++points;
    ++normals;
    ++i;
  }
}

/**
 * compute the balloon force when the model is expand from inside of the model.
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeForce()
{
  int i;
  unsigned short label; 
  IndexType coord = {0, 0, 0};
  IndexType extend = {0, 0, 0};
  double max, fo, t, xs, ys, zs; 

  InputPointType x, y, z, f, n, extends;

  InputPointsContainerPointer  Points = m_Locations->GetPoints();
  InputPointsContainerIterator points = Points->Begin();

  InputPointsContainerPointer  myForces = m_Forces->GetPoints();
  InputPointsContainerIterator forces = myForces->Begin();

  InputPointsContainerPointer  myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator normals = myNormals->Begin();

  InputPointDataContainerPointer  myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator forcedata = myForceData->Begin();

  InputPointDataContainerPointer  myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator pointstatus = myPointData->Begin();

  i = 0;
  
  while( i < m_NumNodes ) {
    xs = ys = zs = 1.0; 
    x = points.Value();

    coord[0] = (int) x[0];
    coord[1] = (int) x[1];
    coord[2] = (int) x[2];

    if ( m_Potential->GetPixel(coord) != m_ObjectLabel ) {
      xs = ys = zs = 0.0;
    }

  //---------------------------------------------------------------------
  // The following part should be added if the input potential are only 
  // estimation of edges
  //---------------------------------------------------------------------  
/*
 coord[0] = (int) (x[0]+1);
 coord[1] = (int) (x[1]+1);
 if ( m_Potential->GetPixel(coord) != m_ObjectLabel ) {
  xs = ys = zs = 0.0;
  }

 coord[0] = (int) (x[0]+1);
 coord[1] = (int) (x[1]);
 if ( m_Potential->GetPixel(coord) != m_ObjectLabel ) {
 xs = ys = zs = 0.0;
 }

 coord[0] = (int) (x[0]+1);
 coord[1] = (int) (x[1]-1);
 if ( m_Potential->GetPixel(coord) != m_ObjectLabel ) {
 xs = ys = zs = 0.0;
 }

 coord[0] = (int) (x[0]);
 coord[1] = (int) (x[1]+1);
 if ( m_Potential->GetPixel(coord) != m_ObjectLabel ) {
  xs = ys = zs = 0.0;
 }

 coord[0] = (int) (x[0]);
 coord[1] = (int) (x[1]-1);
 if ( m_Potential->GetPixel(coord) != m_ObjectLabel ) {
 xs = ys = zs = 0.0;
 }

 coord[0] = (int) (x[0]-1);
 coord[1] = (int) (x[1]+1);
 if ( m_Potential->GetPixel(coord) != m_ObjectLabel ) {
 xs = ys = zs = 0.0;
 }

 coord[0] = (int) (x[0]-1);
 coord[1] = (int) (x[1]);
 if ( m_Potential->GetPixel(coord) != m_ObjectLabel ) {
 xs = ys = zs = 0.0;
 }

 coord[0] = (int) (x[0]-1);
 coord[1] = (int) (x[1]-1);
 if ( m_Potential->GetPixel(coord) != m_ObjectLabel ) {
 xs = ys = zs = 0.0;
 }
*/
    extends[0] = x[0];
    extends[1] = x[1];
    extends[2] = x[2];
    extend[0] = (int) x[0];
    extend[1] = (int) x[1];
    extend[2] = (int) x[2];

    f = normals.Value();
    max = abs(f[0]);

  //---------------------------------------------------------------------
  // all the movement in z direction is now disabled for further test
  //---------------------------------------------------------------------  
    if ( abs(f[1]) > max ) max = abs(f[1]);
    if ( abs(f[2]) > max ) max = abs(f[2]);
    n[0] = f[0]/max;
    n[1] = f[1]/max;
    n[2] = f[2]/max;

    t = 0.0;

    while (t < 5.0){
      extends[0] += n[0];
      extends[1] += n[1];
      extends[2] += n[2];
      extend[0] = (int) (extends[0]+1);
      extend[1] = (int) (extends[1]+1);
      extend[2] = (int) (extends[2]+1);  //slice;
      if ((extend[0] <= 0) || (extend[1] <= 0) || (extend[2] <= 0)) break;

      extend[0] = (int) (extends[0]);
      extend[1] = (int) (extends[1]);
      extend[2] = (int) (extends[2]);
      if ((extend[0] >= m_imgWidth) || (extend[1] >= m_imgHeight) || 
          (extend[2] >= m_imgDepth)) break;

      label = m_Potential->GetPixel(extend);
      if ( label != m_ObjectLabel ) break;

      t += 1.0;
    }

    if (t < 2) pointstatus.Value() = 1.0;
    else {
      pointstatus.Value() = 0.0;
      m_ImageOutput->SetPixel(coord, 1);
    }

    fo = sqrt(f[0]*f[0]+f[1]*f[1]+f[2]*f[2]);
    f[0] = t*400*f[0]*xs/fo; 
    f[1] = t*400*f[1]*ys/fo;
    f[2] = t*400*f[2]*zs/fo;

    forces.Value() = f;
    forcedata.Value() = 0.0;
    ++pointstatus;
    ++forces;
    ++forcedata;
    ++points;
    ++normals;
    ++i;
  }
}

/**
 * compute the derivatives using d'- Kd = f 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeDt()
{
  int i; 
  const unsigned long *tp;
  typename TriCell::Pointer testCell = TriCell::New();

  InputCellsContainerPointer  myCells = m_Locations->GetCells();
  InputCellsContainerIterator cells = myCells->Begin();

  InputPointsContainerPointer  myForces = m_Forces->GetPoints();
  InputPointsContainerIterator forces = myForces->Begin();

  InputPointsContainerPointer  myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator derives = myDerives->Begin();

  double p = 1.0; 
  i = 0;
  InputPointType v1, v2, v3;
  InputPointType* v1_pt;
  InputPointType* v2_pt;
  InputPointType* v3_pt;
  v1_pt = &v1;
  v2_pt = &v2;
  v3_pt = &v3;

  while( cells != myCells->End() ) {
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
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::Reset()
{
  InputPointType x, y, z, d;
  unsigned long tripoints[3];
   
  InputPointsContainerPointer  myForces = m_Forces->GetPoints();
  InputPointsContainerIterator forces = myForces->Begin();

  InputPointsContainerPointer  myPoints = m_Locations->GetPoints();
  InputPointsContainerIterator points = myPoints->Begin();
 
  InputPointsContainerPointer  myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator normals = myNormals->Begin();

  InputPointsContainerPointer  myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator derives = myDerives->Begin();

  InputPointsContainerPointer  myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator displacements = myDisplacements->Begin();

  InputPointDataContainerPointer    myForceData = m_Forces->GetPointData();
  myForceData->Reserve(m_NumNodes);
  InputPointDataContainerIterator   forcedata = myForceData->Begin();

  InputCellsContainerPointer    myCells = m_Locations->GetCells();
  myCells->Reserve(m_NumCells);
  InputCellsContainerIterator   cells = myCells->Begin(); 
  
  InputCellDataContainerPointer    myCellData = m_Locations->GetCellData();
  myCellData->Reserve(m_NumCells);
  InputCellDataContainerIterator   celldata = myCellData->Begin(); 

  InputCellsContainerPointer       myOutCells = m_Output->GetCells();
  myOutCells->Reserve(m_NumCells);
  InputCellsContainerIterator      outcells = myOutCells->Begin(); 
  
  InputCellDataContainerPointer       myOutCellData = m_Output->GetCellData();
  myOutCellData->Reserve(m_NumCells);
  InputCellDataContainerIterator      outcelldata = myOutCellData->Begin();
   
  typename TriCell::Pointer           insertCell = TriCell::New(); 

  int p = 0, jn;

  for(int i=0; i < m_XResolution-1 ; i++) {
    for (int j=0; j<m_YResolution; j++) {
      jn = (j+1)%m_YResolution; 
      tripoints[0] = i*m_YResolution+j; 
      tripoints[1] = tripoints[0]-j+jn; 
      tripoints[2] = tripoints[0]+m_YResolution; 
      insertCell->SetPointIds(tripoints);
      m_Locations->SetCell(p, insertCell);
      m_Locations->SetCellData(p, (PixelType)3.0);
      p++;
      insertCell = TriCell::New();
      tripoints[0] = tripoints[1]; 
      tripoints[1] = tripoints[0]+m_YResolution; 
      insertCell->SetPointIds(tripoints);
      m_Locations->SetCell(p, insertCell);
      m_Locations->SetCellData(p, (PixelType)3.0);
      p++;
      insertCell = TriCell::New();
    }
  }
 
// store cells containing the south pole nodes
  for (int j=0; j<m_YResolution; j++) {
    jn = (j+1)%m_YResolution; 
    tripoints[0] = m_NumNodes-2; 
    tripoints[1] = jn; 
    tripoints[2] = j; 
    insertCell->SetPointIds(tripoints);
    m_Locations->SetCell(p, insertCell);
    m_Locations->SetCellData(p, (PixelType)1.0);
    p++;
    insertCell = TriCell::New();
  }

// store cells containing the north pole nodes
  for (int j=0; j<m_YResolution; j++) {
    jn = (j+1)%m_YResolution; 
    tripoints[2] = (m_XResolution-1)*m_YResolution+j; 
    tripoints[1] = m_NumNodes-1; 
    tripoints[0] = tripoints[2]-j+jn; 
    insertCell->SetPointIds(tripoints);
    m_Locations->SetCell(p, insertCell);
    m_Locations->SetCellData(p, (PixelType)2.0);
    p++;
    insertCell = TriCell::New();
  }
}

/**
 * update the displacements using d_{new} = d_{old} + timestep*d' 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::Advance()
{
  typename TInputMesh::PointType s, d, ds; 

  int i;

  m_ModelXUpLimit = 0;
  m_ModelXDownLimit = m_imgWidth;
  m_ModelYUpLimit = 0;
  m_ModelYDownLimit = m_imgHeight;
  m_ModelZUpLimit = 0;
  m_ModelZDownLimit = m_imgDepth;

  InputPointsContainerPointer  myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator derives = myDerives->Begin();

  InputPointsContainerPointer  myPoints = m_Locations->GetPoints();
  InputPointsContainerIterator points = myPoints->Begin();
 
  i = 0;
  while( derives != myDerives->End() ) {
    ds = derives.Value();
    s = points.Value();
    s[0] += m_TimeStep*ds[0]; 
    s[1] += m_TimeStep*ds[1]; 
    s[2] += m_TimeStep*ds[2]; 
    if ( m_ModelYDownLimit > s[1] ) m_ModelYDownLimit = s[1];
    if ( m_ModelYUpLimit < s[1] ) m_ModelYUpLimit = s[1];
    if ( m_ModelXDownLimit > s[0] ) m_ModelXDownLimit = s[0];
    if ( m_ModelXUpLimit < s[0] ) m_ModelXUpLimit = s[0];
    if ( m_ModelZDownLimit > s[2] ) m_ModelZDownLimit = s[2];
    if ( m_ModelZUpLimit < s[2] ) m_ModelZUpLimit = s[2];
//if ( i < m_NumNodes - 2 ) {
//disable for shrink test
    if (s[0] < 0) {
      s[0] = 0;
    }
    if (s[1] < 0) {
      s[1] = 0;
    }
    if (s[2] < 0) {
      s[2] = 0;
    }
    if (s[2] > m_imgDepth) {
      s[2] = m_imgDepth-0.001;
    }
    if (s[0] > m_imgWidth) {
      s[0] = m_imgWidth-0.001;
    }
    if (s[1] > m_imgHeight) {
      s[1] = m_imgHeight-0.001;
    }

    points.Value() = s;
//  }
    ++derives; 
    ++points;
    ++i;
  } 

}

/**
 * copy the content of m_Location into output 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeOutput() 
{
 
  int i;
  TriCell::Pointer insertCell;
  unsigned long tripoints[3];
  const unsigned long *tp;
  double x;

  m_Output = this->GetOutput();

  OutputPointsContainerPointer   myPoints = m_Output->GetPoints();
  myPoints->Reserve(m_NumNodes);
  OutputPointsContainerIterator  points = myPoints->Begin();

  InputPointsContainerPointer    myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator   locations = myLocations->Begin();

  InputCellsContainerPointer     myCells = m_Locations->GetCells();
  InputCellsContainerIterator    cells = myCells->Begin(); 
  
  InputCellDataContainerPointer  myCellData = m_Locations->GetCellData();
  InputCellDataContainerIterator celldata = myCellData->Begin(); 

  i = 0;
  for (; i<m_NumNodes; i++) {
    points.Value() = locations.Value();
    ++locations;
    ++points;
  } 

  for (int i=0; i<m_NumCells; i++) {
    tp = cells.Value()->GetPointIds();
    tripoints[0] = tp[0];
    tripoints[1] = tp[1];
    tripoints[2] = tp[2];
    insertCell = TriCell::New();
    insertCell->SetPointIds(tripoints);
    m_Output->SetCell(i, insertCell);
    x = celldata.Value();
    m_Output->SetCellData(i, (PixelType)x);
    ++cells;
    ++celldata;
  }
}

/**
 * copy the content of m_Location into output 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::GenerateData() 
{
  this->Initialize();
  this->SetStiffnessMatrix();

  while (m_Step < m_StepThreshold2) {
    this->ComputeNormals();
    if (m_Step > m_StepThreshold1) this->GradientFit();
    else  this->ComputeForce();
    this->ComputeDt();
    this->Advance();
    this->ACDSearch();
    this->NodesRearrange();
    this->ComputeOutput();
    m_Step++;
  }
}

/**
 * when almost all the nodes is at the estimated boundary, use
 * gapsearch to fit the model to more complicated shapes
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::GapSearch() 
{

}

/**
 * add new nodes into the model 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::NodeAddition() 
{
  m_NewNode = 1;
}

/**
 * add a new slice into the model 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::SliceAddition(int i) 
{
  typename TInputMesh::PointType s, s1, d1, d2;

  InputPointsContainerPointer   myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator  locations = myLocations->Begin();

  InputPointDataContainerPointer    myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator   pointdata = myPointData->Begin();

  InputPointsContainerPointer   myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator  displacements = myDisplacements->Begin();

  InputPointsContainerPointer   myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator  normals = myNormals->Begin();
  
  int j, k, l;

  j = 0;
  k = 0;
  while (j < i-1) {
    k = 0;
    while ( k < m_YResolution ) {
      ++locations;
      k++;
    }
    j++;
  } 
  j = 0;

  while ( j < m_YResolution ) {
    displacements.Value() = locations.Value();  
    ++displacements;
    ++locations;
    j++;
  }

  if (i == 0) {
    locations = myLocations->Begin();
    j = 0;
    while ( j < m_NumNodes - 2 ) {
      ++locations;
      j++;
    } 
    j = 0;
    
    while ( j < m_YResolution ) {
      normals.Value() = locations.Value();  
      ++normals;
      j++;
    }
  } 

  if (i == m_XResolution) {
    locations = myLocations->Begin();
    j = 0;
    
    while ( j < m_NumNodes - 1 ) {
      ++locations;
      j++;
    }
 
    j = 0;
    while ( j < m_YResolution ) {
      normals.Value() = locations.Value();  
      ++normals;
      j++;
    }
  } 

  if ( (i > 0) && ( i < m_XResolution ) ) {
    j = 0;
    while ( j < m_YResolution ) {
      normals.Value() = locations.Value();  
      ++normals;
      ++locations;
      j++;
    }
  }

  if ( (i < 0) || ( i > m_XResolution ) ) return;
    displacements = myDisplacements->Begin();
    normals = myNormals->Begin();
    j = 0;

    while ( j < m_YResolution ) {
      d1 = displacements.Value();
      d2 = normals.Value();
      s[0] = 0.5*(d1[0]+d2[0]);
      s[1] = 0.5*(d1[1]+d2[1]); 
      s[2] = 0.5*(d1[2]+d2[2]);
      normals.Value() = s;   
      ++displacements;
      ++normals;
      j++;
    }

    locations = myLocations->Begin();

    m_NumNodes = m_NumNodes+m_YResolution;
    m_NumCells = 2*m_YResolution+m_NumCells;
    m_XResolution += 1;

    OutputPointsContainerPointer     myPoints = m_Output->GetPoints();
    myPoints->Reserve(m_NumNodes);

    myDisplacements->Reserve(m_NumNodes);
    displacements = myDisplacements->Begin();

    InputPointsContainerPointer     myForces = m_Forces->GetPoints();
    myForces->Reserve(m_NumNodes);
    InputPointsContainerIterator    forces = myForces->Begin();

    InputPointsContainerPointer     myDerives = m_Derives->GetPoints();
    myDerives->Reserve(m_NumNodes);
    InputPointsContainerIterator    derives = myDerives->Begin();

    InputCellsContainerPointer      myCells = m_Locations->GetCells();
    myCells->Reserve(m_NumCells);
    InputCellsContainerIterator     cells = myCells->Begin(); 
  
    InputCellDataContainerPointer   myCellData = m_Locations->GetCellData();
    myCellData->Reserve(m_NumCells);
    InputCellDataContainerIterator  celldata = myCellData->Begin();  

    normals = myNormals->Begin();

    j = 0;
    k = 0;
    l = 0;
    while (j < m_NumNodes) {
      if ( k == i) {
        while ( l < m_YResolution ) {
          displacements.Value() = normals.Value();
          ++displacements;
          ++normals;
          l++;
          j++;
        }
      } else {
        displacements.Value() = locations.Value();
        ++displacements;
        ++locations;
        l++;
        j++;
      }

      if ( l == m_YResolution ) {
        l = 0;
        k++;
      }
    }

    myLocations->Reserve(m_NumNodes);
    locations = myLocations->Begin();
    myPointData->Reserve(m_NumNodes);
    pointdata = myPointData->Begin();

    myNormals->Reserve(m_NumNodes);
    normals = myNormals->Begin();

    s[0] = 0;
    s[1] = 0;
    s[2] = 0;
    j = 0;
    displacements = myDisplacements->Begin();

    while ( j < m_NumNodes ) {
      locations.Value() = displacements.Value();
      displacements.Value() = s;
      ++displacements;
      ++locations;
      j++;
    }

    for (j=0; j<m_NumNodes; j++) {
      m_Locations->SetPointData(j, 0);
      m_Forces->SetPointData(j, 0);
    }

    Reset();
    SetStiffnessMatrix();
}


/**
 * fit the model to the gradient information
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::GradientFit() 
{
  IndexType coord = {0, 0, 0};
//  float grad[3];
  InputPointType v1, v2;

  typename TInputMesh::PointType s, d;

  InputPointsContainerPointer       myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator      locations = myLocations->Begin();

  InputPointsContainerPointer       myForces = m_Forces->GetPoints();
  InputPointsContainerIterator      forces = myForces->Begin();

  InputPointsContainerPointer       myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator      normals = myNormals->Begin();

  InputPointDataContainerPointer    myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator   forcedata = myForceData->Begin();

  s[0] = 0;
  s[1] = 0;
  s[2] = 0;

/////////////////////
// new gradient fit method testing

  locations = myLocations->Begin();
  forces = myForces->Begin();
  forcedata = myForceData->Begin();

  while( forces != myForces->End() ) {
    s = locations.Value();
    d = forces.Value();

    coord[0] = (int) s[0];
    coord[1] = (int) s[1];
    coord[2] = (int) s[2]; 
    d[0] = m_Gradient->GetPixel(coord)[0];
    d[1] = m_Gradient->GetPixel(coord)[1]; 
    d[2] = m_Gradient->GetPixel(coord)[2]; 
    forces.Value() = d;

    ++forces;
    ++locations;
//  ++forcedata;
  }

}

/**
 * fit the model to the gradient information
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeNormals() 
{
  const unsigned long *tp;
  InputPointType v1, v2, v3, v4, d;
  InputPointType* v1_pt;
  InputPointType* v2_pt;
  InputPointType* v3_pt;
  v1_pt = &v1;
  v2_pt = &v2;
  v3_pt = &v3;
  double coa, cob, coc ;
  double absvec ;

  InputCellsContainerPointer    myCells = m_Locations->GetCells();
  InputCellsContainerIterator   cells = myCells->Begin();

  InputPointsContainerPointer   myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator  normals = myNormals->Begin();

  d[0] = 0.0;
  d[1] = 0.0;
  d[2] = 0.0;
  while( normals != myNormals->End() ) {
    normals.Value() = d;
    ++normals;
  }

  while ( cells != myCells->End() ) {
    tp = cells.Value()->GetPointIds();
    ++cells;

    m_Locations->GetPoint (tp[0], v1_pt);
    m_Locations->GetPoint (tp[1], v2_pt);
    m_Locations->GetPoint (tp[2], v3_pt);

    coa = -(v1[1]*(v2[2]-v3[2]) + 
    v2[1]*(v3[2]-v1[2]) +
    v3[1]*(v1[2]-v2[2])) ;
    cob = -(v1[2] * (v2[0]-v3[0]) +
    v2[2]*(v3[0]-v1[0]) +
    v3[2]*(v1[0]-v2[0])) ;
    coc = -(v1[0] * (v2[1]-v3[1]) +
    v2[0]*(v3[1]-v1[1]) +
    v3[0]*(v1[1]-v2[1])) ;

    absvec = -sqrt ((double) ((coa*coa) + (cob*cob) + (coc*coc))) ;
  
    assert (absvec != 0);
  
    v4[0] = coa/absvec;
    v4[1] = cob/absvec;
    v4[2] = coc/absvec;
    m_Normals->GetPoint (tp[0], v1_pt);
    m_Normals->GetPoint (tp[1], v2_pt);
    m_Normals->GetPoint (tp[2], v3_pt);

    v1[0] += v4[0];
    v1[1] += v4[1];
    v1[2] += v4[2];

    v2[0] += v4[0];
    v2[1] += v4[1];
    v2[2] += v4[2];
  
    v3[0] += v4[0];
    v3[1] += v4[1];
    v3[2] += v4[2];

    m_Normals->SetPoint (tp[0], v1);
    m_Normals->SetPoint (tp[1], v2);
    m_Normals->SetPoint (tp[2], v3);

  }

  normals = myNormals->Begin();
  while( normals != myNormals->End() ) {
    v1 = normals.Value();
    absvec = sqrt ((double) ((v1[0]*v1[0]) + (v1[1]*v1[1]) + (v1[2]*v1[2])));
    v1[0] = v1[0]/absvec;
    v1[1] = v1[1]/absvec;
    v1[2] = v1[2]/absvec;
    normals.Value() = v1;
    ++normals;
  }

}

/**
 * fit the model to the gradient information
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::NodesRearrange()
{
  int i, j, k, new_node=1;
  double dis, l1, l2, d;
  double* length;
  InputPointType v1, v2, v3, v4, v_southpole, v_northpole;

  typename TInputMesh::PointType s, s1, d1, d2;

  InputPointsContainerPointer   myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator  locations = myLocations->Begin();

  InputPointDataContainerPointer    myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator   pointdata = myPointData->Begin();

  InputPointsContainerPointer   myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator  displacements = myDisplacements->Begin();

  InputPointsContainerPointer   myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator  normals = myNormals->Begin();

  InputPointsContainerPointer   myForces = m_Forces->GetPoints();
  InputPointsContainerIterator  forces = myForces->Begin();
  InputPointsContainerIterator  forcescopy;

  length = (double*) malloc(sizeof(double)*m_XResolution);

  for (j = 0; j < m_XResolution; j++) {
    v1 = locations.Value();
    s = locations.Value();
    ++locations;
    s1 = locations.Value();
    i = j*m_YResolution + 1;
    forcescopy = forces;
    forces++;
  
    while ( (i+1)%m_YResolution != 0 ) {
      v2 = locations.Value();
      ++locations;
      v3 = locations.Value();
      d1[0] = v1[0] - v2[0];
      d2[0] = v3[0] - v2[0];
      d1[1] = v1[1] - v2[1];
      d2[1] = v3[1] - v2[1];
      d1[2] = v1[2] - v2[2];
      d2[2] = v3[2] - v2[2];
      v1[0] = v2[0];
      v1[1] = v2[1];
      v1[2] = v2[2];
      dis = d1[0]*d2[0]+d1[1]*d2[1]+d1[2]*d2[2];
      if ( dis > 0 ) {
        l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]+d1[2]*d1[2]);
        l2 = sqrt(d2[0]*d2[0]+d2[1]*d2[1]+d2[2]*d2[2]);
        dis = dis/sqrt(l1*l2);
        d1[0] = d1[0]/l1;
        d1[1] = d1[1]/l1;
        d1[2] = d1[2]/l1;
        d2[0] = d2[0]/l2;
        d2[1] = d2[1]/l2;
        d2[2] = d2[2]/l2;
        d1[0] = (d1[0]+d2[0]);
        d1[1] = (d1[1]+d2[1]);
        d1[2] = (d1[2]+d2[2]);
        l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]+d1[2]*d1[2]);
        d1[0] = d1[0]/l1;
        d1[1] = d1[1]/l1;
        d1[2] = d1[2]/l1;
        v2[0] = v2[0] + dis * d1[0];
        v2[1] = v2[1] + dis * d1[1];
        v2[2] = v2[2] + dis * d1[2];
      }

      forces.Value() = v2;
      i++;
      ++forces;
    }

    // for the last node in the slice
    v2 = locations.Value();
    ++locations;
    v3[0] = s[0];
    v3[1] = s[1];
    v3[2] = s[2];
    d1[0] = v1[0] - v2[0];
    d2[0] = v3[0] - v2[0];
    d1[1] = v1[1] - v2[1];
    d2[1] = v3[1] - v2[1];
    d1[2] = v1[2] - v2[2];
    d2[2] = v3[2] - v2[2];
    v1[0] = v2[0];
    v1[1] = v2[1];
    v1[2] = v2[2];
    dis = d1[0]*d2[0]+d1[1]*d2[1]+d1[2]*d2[2];
    if ( dis > 0 ) {
      l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]+d1[2]*d1[2]);
      l2 = sqrt(d2[0]*d2[0]+d2[1]*d2[1]+d2[2]*d2[2]);
      dis = dis/sqrt(l1*l2);
      d1[0] = d1[0]/l1;
      d1[1] = d1[1]/l1;
      d1[2] = d1[2]/l1;
      d2[0] = d2[0]/l2;
      d2[1] = d2[1]/l2;
      d2[2] = d2[2]/l2;
      d1[0] = (d1[0]+d2[0]);
      d1[1] = (d1[1]+d2[1]);
      d1[2] = (d1[2]+d2[2]);
      l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]+d1[2]*d1[2]);
      d1[0] = d1[0]/l1;
      d1[1] = d1[1]/l1;
      d1[2] = d1[2]/l1;
      v2[0] = v2[0] + dis * d1[0];
      v2[1] = v2[1] + dis * d1[1];
      v2[2] = v2[2] + dis * d1[2];
    }
  
    forces.Value() = v2;
    ++forces;

    // for the first node in the slice
    i = j*m_YResolution;
    v2[0] = s[0];
    v2[1] = s[1];
    v2[2] = s[2];
    v3[0] = s1[0];
    v3[1] = s1[1];
    v3[2] = s1[2];
    d1[0] = v1[0] - v2[0];
    d2[0] = v3[0] - v2[0];
    d1[1] = v1[1] - v2[1];
    d2[1] = v3[1] - v2[1];
    d1[2] = v1[2] - v2[2];
    d2[2] = v3[2] - v2[2];
    dis = d1[0]*d2[0]+d1[1]*d2[1]+d1[2]*d1[2];
    if ( dis > 0 ) {
      l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]+d1[2]*d1[2]);
      l2 = sqrt(d2[0]*d2[0]+d2[1]*d2[1]+d1[2]*d1[2]);
      dis = dis/sqrt(l1*l2);
      d1[0] = d1[0]/l1;
      d1[1] = d1[1]/l1;
      d1[2] = d1[2]/l1;
      d2[0] = d2[0]/l2;
      d2[1] = d2[1]/l2;
      d2[2] = d2[2]/l2;
      d1[0] = (d1[0]+d2[0]);
      d1[1] = (d1[1]+d2[1]);
      d1[2] = (d1[2]+d2[2]);
      l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]+d1[2]*d1[2]);
      d1[0] = d1[0]/l1;
      d1[1] = d1[1]/l1;
      d1[2] = d1[2]/l1;
      v2[0] = v2[0] + dis * d1[0];
      v2[1] = v2[1] + dis * d1[1];
      v2[2] = v2[2] + dis * d1[2];
    }

    forcescopy.Value() = v2;
  }

  v_southpole = locations.Value();
  ++locations;
  v_northpole = locations.Value();

  forces = myForces->Begin();

  for (j = 0; j < m_XResolution; j++) {
    dis = 0;
    i = 0;
    s = forces.Value();

    while ( i < m_YResolution - 1 ) {
      v1 = forces.Value();
      ++forces;
      v2 = forces.Value();
      dis += sqrt((v1[0]-v2[0])*(v1[0]-v2[0])+(v1[1]-v2[1])*(v1[1]-v2[1])+(v1[2]-v2[2])*(v1[2]-v2[2]));
      i++;
    }
    dis += sqrt((s[0]-v2[0])*(s[0]-v2[0])+(s[1]-v2[1])*(s[1]-v2[1])+(s[2]-v2[2])*(s[2]-v2[2]));
    length[j] = dis/m_YResolution;
    ++forces;
  }

  if ( m_NewNode == 1 ) {
    m_NumNodes = 2*(m_NumNodes-2)+2;
    m_NumCells = 2*m_NumCells;
    new_node = 2;

    OutputPointsContainerPointer    myPoints = m_Output->GetPoints();
    myPoints->Reserve(m_NumNodes);

    myLocations->Reserve(m_NumNodes);
    locations = myLocations->Begin();

    myPointData->Reserve(m_NumNodes);
    pointdata = myPointData->Begin();

    myDisplacements->Reserve(m_NumNodes);
    displacements = myDisplacements->Begin();

    myNormals->Reserve(m_NumNodes);
    normals = myNormals->Begin();

    InputPointsContainerPointer     myDerives = m_Derives->GetPoints();
    myDerives->Reserve(m_NumNodes);
    InputPointsContainerIterator    derives = myDerives->Begin();

    InputCellsContainerPointer      myCells = m_Locations->GetCells();
    myCells->Reserve(m_NumCells);
    InputCellsContainerIterator     cells = myCells->Begin(); 
  
    InputCellDataContainerPointer   myCellData = m_Locations->GetCellData();
    myCellData->Reserve(m_NumCells);
    InputCellDataContainerIterator  celldata = myCellData->Begin();

    for (j = 0; j < m_XResolution; j++) length[j] = length[j]*0.5;
  }

  forces = myForces->Begin();

  for (j = 0; j < m_XResolution; j++) {
    k = 1;
    i = 0;
    l1 = 0;
    v1 = forces.Value();
    normals.Value() = v1;
    ++normals;
    v3 = forces.Value();
    while ( i < m_YResolution - 1 ) {
      v1 = forces.Value();
      ++forces;
      v2 = forces.Value();
      dis = sqrt((v1[0]-v2[0])*(v1[0]-v2[0])+(v1[1]-v2[1])*(v1[1]-v2[1])+(v1[2]-v2[2])*(v1[2]-v2[2]));
      l2 = -1*l1;
      l1 += dis;

      while ( l1 > length[j] ) {
        if (k==m_YResolution*new_node) break;
        s[0] = v1[0] + (length[j]+l2)*(v2[0] - v1[0])/dis;
        s[1] = v1[1] + (length[j]+l2)*(v2[1] - v1[1])/dis;
        s[2] = v1[2] + (length[j]+l2)*(v2[2] - v1[2])/dis;
        normals.Value() = s;
        ++normals;
        k++;
        l2 += length[j];
        l1 -= length[j];
      }
      i++;
      if (k==m_YResolution*new_node) break;
    }

    if (k==m_YResolution*new_node) {
      while (i < m_YResolution) {
        i++;
        ++forces;
      }
      continue;
    }

    v1 = forces.Value();
    ++forces;
    dis = sqrt((v1[0]-v3[0])*(v1[0]-v3[0])+(v1[1]-v3[1])*(v1[1]-v3[1])+(v1[2]-v3[2])*(v1[2]-v3[2]));
    l2 = -1*l1;
    l1 += dis;
    while ( l1 > length[j] ) {
      if (k==m_YResolution*new_node) break;
      s[0] = v1[0] + (length[j]+l2)*(v3[0] - v1[0])/dis;
      s[1] = v1[1] + (length[j]+l2)*(v3[1] - v1[1])/dis;
      s[2] = v1[2] + (length[j]+l2)*(v3[2] - v1[2])/dis;
      normals.Value() = s;
      ++normals;
      k++;
      l2 += length[j]/d;
      l1 -= length[j]/d;
    }

  }

  locations = myLocations->Begin();
  normals = myNormals->Begin();
  displacements = myDisplacements->Begin();

  i = 0;
  while ( i < m_NumNodes-2 ) {
    v1 = normals.Value();
    v2 = locations.Value();
    v3 = displacements.Value();
    v3[0] += v1[0] - v2[0];
    v3[1] += v1[1] - v2[1];
    v3[2] += v1[2] - v2[2];
    locations.Value() = v1;
    displacements.Value() = v3;
    ++normals;
    ++locations;
    ++displacements;
    i++;
  }

  locations.Value() = v_southpole;
  ++locations;
  locations.Value() = v_northpole;

  if ( m_NewNode == 1 ) {
    m_YResolution = m_YResolution*2;

    v3[0] = 0;
    v3[1] = 0;
    v3[2] = 0;
    i = 0;
    displacements = myDisplacements->Begin();
    while ( i < m_NumNodes ) {
      displacements.Value() = v3;
      ++displacements;
      i++;
    }

    myForces->Reserve(m_NumNodes);
    for (int i=0; i<m_NumNodes; i++) {
      m_Locations->SetPointData(i, 0);
      m_Forces->SetPointData(i, 0);
    }
    m_NewNode = 0;
    Reset();
    SetStiffnessMatrix();
  }

  free(length);
}

template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::ACDSearch() 
{
  int i, j, l, m, n, pt1, pt2;
  double s;
  InputPointType v, v1, v2, v3;
  m_ACD = (int**) malloc(sizeof(int *)*m_imgHeight/2);

  for (i=0; i<m_imgHeight/2; i++) {
    m_ACD[i] = (int*) malloc(sizeof(int)*m_imgWidth/2);
  }
  
  InputPointsContainerPointer   myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator  locations = myLocations->Begin();
  InputPointsContainerIterator  locationscopy;
  InputPointsContainerIterator  locationscopy1;
  InputPointsContainerPointer   myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator  displacements = myDisplacements->Begin();
  InputPointsContainerIterator  dpcopy;
  InputPointsContainerIterator  dpcopy1;

  for (j = 0; j < m_XResolution; j++) {
    i = 0;
    locationscopy = locations;
    dpcopy = displacements;

    for (l=m_ModelYDownLimit/2; l<=m_ModelYUpLimit/2; l++) 
    for (m=m_ModelXDownLimit/2; m<=m_ModelXUpLimit/2; m++)
    m_ACD[l][m] = -1;

    for (; i < m_YResolution; i++) {
      v = locations.Value();
      ++locations;
      ++displacements;
      l = (int)(v[0]/2);
      m = (int)(v[1]/2);
      if (m_ACD[m][l] == -1) m_ACD[m][l] = i;
      else {
        if (((i - m_ACD[m][l]) > m_YResolution/10) && 
            ((m_YResolution-i+m_ACD[m][l])>m_YResolution/10)) {
          locationscopy1 = locationscopy;
          n = 0;
          v1[0] = 0;
          v1[1] = 0;
          v1[2] = 0;
          if ( (i - m_ACD[m][l]) < 0.5*m_YResolution ) {
            if (m_ACD[m][l] == 0) pt1 = m_YResolution - 1;
            else pt1 = m_ACD[m][l] - 1;
            if (i == m_YResolution - 1) pt2 = 0;
            else pt2 = i + 1;
            while (n<m_YResolution) {
              v = locationscopy1.Value();
              if ((n>m_ACD[m][l]) && (n<i)) {
                v1[0] += v[0];
                v1[1] += v[1];
              } else {
                if ( n == pt1) v2 = locationscopy1.Value();
                if ( n == pt2 ) v3 = locationscopy1.Value();
              }
              ++locationscopy1;
              n++;
            }
            v1[0] = v1[0]/(i-m_ACD[m][l]-1);
            v1[1] = v1[1]/(i-m_ACD[m][l]-1);
            s = 1;
            if (s > 0) {
              locationscopy1 = locationscopy;
              dpcopy1 = dpcopy;
              n = 0;
              while (n<m_YResolution) {
                if ((n>m_ACD[m][l]) && (n<i)) {
                  v1 = locationscopy1.Value();
                  locationscopy1.Value() = v;
                  v2 = dpcopy1.Value();
                  v2[0] += v[0] - v1[0];
                  v2[1] += v[1] - v1[1];
                  v2[2] += v[2] - v1[2];
                  dpcopy1.Value() = v2;
                } 
                if ( n == m_ACD[m][l] ) {
                  v = locationscopy1.Value();
                }
                ++locationscopy1;
                ++dpcopy1;
                n++;
              }
            }
          } else {
            while (n<m_YResolution) {
              pt1 = m_ACD[m][l] + 1;
              pt2 = i - 1;
              v = locationscopy1.Value();
              if ((n<m_ACD[m][l]) && (n>i)) {
                v1[0] += v[0];
                v1[1] += v[1];
              } else{
                if ( n == pt1 ) v2 = locationscopy1.Value();
                if ( n == pt2 ) v3 = locationscopy1.Value();
              }
              ++locationscopy1;
              n++;
            }
            v1[0] = v1[0]/(i-m_ACD[m][l]-1);
            v1[1] = v1[1]/(i-m_ACD[m][l]-1);
            s = -1;
            if (s < 0) {
              locationscopy1 = locationscopy;
              dpcopy1 = dpcopy;
              n = 0;
              while (n<m_YResolution) {
                if ( n == i ) {
                  v = locationscopy1.Value();
                }
                ++locationscopy1;
                n++;
              }

              locationscopy1 = locationscopy;
              n = 0;
              while (n<m_YResolution) {
                if ((n<m_ACD[m][l]) && (n>i)) {
                  v1 = locationscopy1.Value();
                  locationscopy1.Value() = v;
                  v2 = dpcopy1.Value();
                  v2[0] += v[0] - v1[0];
                  v2[1] += v[1] - v1[1];
                  v2[2] += v[2] - v1[2];
                  dpcopy1.Value() = v2;
                }
                ++locationscopy1;
                ++dpcopy1;
                n++;
              }
            }
          }

          m_ModelRestart = 1;
          break;
        }
      }
    }
  }

  m_ModelRestart = 0;
  for (i=0; i<m_imgHeight/2; i++) {
    free(m_ACD[i]);
  }
  free(m_ACD);
  
}

} // end namespace itk

#endif
