/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDeformableMeshFilter_txx
#define _itkDeformableMeshFilter_txx

#include "itkDeformableMeshFilter.h"

namespace itk
{
/*
 * standard 
 */
template <typename TInputMesh, typename TOutputMesh>
DeformableMeshFilter<TInputMesh, TOutputMesh>
::DeformableMeshFilter()
{
  m_Step = 0;
  m_FirstSlice = 0;
  m_NewNode = false;
  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
}

/*
 * standard 
 *
template <typename TInputMesh, typename TOutputMesh>
DeformableMeshFilter<TInputMesh, TOutputMesh>
::~DeformableMeshFilter()
{
//  m_Forces->Delete();
//  m_Displacements->Delete();
//  m_Derives->Delete();
//  m_Normals->Delete();
//  m_Locations->Delete();
}*/

/*
 * PrintSelf
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Balloon Force Filter" << std::endl;

}// end PrintSelf

/*
 * set the stiffness parameter which would help 
 * to build the stiffness matrix
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMeshFilter<TInputMesh, TOutputMesh>
::SetStiffness(double a, double b)
{
  m_Stiffness[0] = a; 
  m_Stiffness[1] = b; 
}

/*
 * set the center point which would help 
 * to identify the interested object
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMeshFilter<TInputMesh, TOutputMesh>
::SetCenter(int a, int b, int c)
{
  m_Center[0] = a; 
  m_Center[1] = b; 
  m_Center[2] = c;
}

/*
 * set the input binary image 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMeshFilter<TInputMesh, TOutputMesh>
::SetPotential(ImagePointer potential)
{
  m_Potential = potential; 
}

/*
 * set the input gradient image 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMeshFilter<TInputMesh, TOutputMesh>
::SetGradient(GradientPointer gradient)
{
  m_Gradient = gradient; 
}

/*
 * set the output model region image 
 *
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMeshFilter<TInputMesh, TOutputMesh>
::SetImageOutput(ImagePointer outputimg)
{
  m_ImageOutput = outputimg; 
}


template <typename TInputMesh, typename TOutputMesh>
ImagePointer
DeformableMeshFilter<TInputMesh, TOutputMesh>
::GetImageOutput()
{
  return m_ImageOutput; 
}*/

/*
 * set the resolutions of the model, resolution[0] is the number of slices
 * resolution[1] is the number of nodes in each slices 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMeshFilter<TInputMesh, TOutputMesh>
::SetResolution(int a, int b, int c)
{
  m_Resolution[0] = a; 
  m_Resolution[1] = b;
  m_Resolution[2] = c;
}

/*
 * set default value of parameters and initialize local data container such as forces,
 * displacements and displacement derivatives
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMeshFilter<TInputMesh, TOutputMesh>
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

  InputPointsContainerPointer     myPoints = m_Input->GetPoints();
  InputPointsContainerIterator      points = myPoints->Begin();
  
  InputPointsContainerPointer       myForces = m_Forces->GetPoints();
  myForces->Reserve(m_NumNodes);
  InputPointsContainerIterator      forces = myForces->Begin();

  InputPointsContainerPointer       myDerives = m_Derives->GetPoints();
  myDerives->Reserve(m_NumNodes);
  InputPointsContainerIterator      derives = myDerives->Begin();

  InputPointsContainerPointer       myDisplacements = m_Displacements->GetPoints();
  myDisplacements->Reserve(m_NumNodes);
  InputPointsContainerIterator      displacements = myDisplacements->Begin();

  InputPointsContainerPointer       myNormals = m_Normals->GetPoints();
  myNormals->Reserve(m_NumNodes);
  InputPointsContainerIterator      normals = myNormals->Begin();

  InputPointsContainerPointer       myLocations = m_Locations->GetPoints();
  myLocations->Reserve(m_NumNodes);
  InputPointsContainerIterator      locations = myLocations->Begin();

  InputCellsContainerPointer      myCells = m_Input->GetCells();
  InputCellsContainerIterator     cells = myCells->Begin(); 
  
  InputCellDataContainerPointer     myCellData = m_Input->GetCellData();
  InputCellDataContainerIterator    celldata = myCellData->Begin(); 
  
  m_Stiffness[0] = 0.00001; 
  m_Stiffness[1] = 0.04; 
  m_NumNewNodes = 0;
  m_NewNodesExisted = 0;
  m_NewNodeLimit = 200;
  m_ObjectLabel = m_Potential->GetPixel(m_Center);
//  m_NewNodes = (float**) malloc(sizeof(float *)*m_NewNodeLimit);

  PotentialSizeType PotentialSize = m_Potential->GetBufferedRegion().GetSize();
  //---------------------------------------------------------------------
  //Get the image width/height and depth
  //---------------------------------------------------------------------       
  m_ImageWidth  = PotentialSize[0];
  m_ImageHeight = PotentialSize[1];
  m_ImageDepth  = PotentialSize[2];

  float d[3] = {0,0,0}, fd=0; 

  TimeStep = 0.001; 

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
  m_Derives->SetPointData(i, 0.0);
  m_Displacements->SetPointData(i, 0.0);
  }

//  for (int i = 0; i < m_NewNodeLimit; i++) {
//  m_NewNodes[i] = (float*) malloc(sizeof(float)*5);
//  }
   
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

  typename TriCell::CellAutoPointer insertCell;
  unsigned long tripoints[3];
  const unsigned long *tp;
  float x;

  for (int i=0; i<m_NumCells; i++) {
  tp = cells.Value()->GetPointIds();
  tripoints[0] = tp[0];
  tripoints[1] = tp[1];
  tripoints[2] = tp[2];
  insertCell.TakeOwnership( new TriCell );
  insertCell->SetPointIds(tripoints);
  m_Locations->SetCell(i, insertCell);
  x = celldata.Value();
  m_Locations->SetCellData(i, (PT)x);
  ++cells;
  ++celldata;
  }
} 

/*
 * set the stiffness matrix 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMeshFilter<TInputMesh, TOutputMesh>
::SetStiffnessMatrix () 
{ 
  InputCellDataContainerPointer     myCellData = m_Locations->GetCellData();
  InputCellDataContainerIterator    celldata = myCellData->Begin();

  K = (vnl_matrix_fixed<double,4,4>**) malloc(sizeof(vnl_matrix_fixed<double,4,4>*)*m_NumCells);
  float x;  

  float us = vnl_math::pi / m_Resolution[0]; 
  float vs = 2.0*vnl_math::pi / m_Resolution[1]; 
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

/*
 * compute the model start points
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMeshFilter<TInputMesh, TOutputMesh>
::InitialFit()
{
  int i, p, label, l=0; 
  IndexType coord = {0, 0, 0};
  IndexType extend = {0, 0, 0};
  float extends[3], fo, t, xs, ys, zs; 
  FloatVector n1, n, vec1, vec2; 
  float max;
  IPT x, y, z, f;
  IPT* y_pt;
  IPT* z_pt;
  y_pt = &y;
  z_pt = &z;

  InputPointsContainerPointer     Points = m_Locations->GetPoints();
  InputPointsContainerIterator      points = Points->Begin();

  InputPointsContainerPointer     myForces = m_Forces->GetPoints();
  InputPointsContainerIterator      forces = myForces->Begin();

  InputPointsContainerPointer     myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator      normals = myNormals->Begin();

  InputPointDataContainerPointer    myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator   forcedata = myForceData->Begin();

  InputPointDataContainerPointer    myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator   pointstatus = myPointData->Begin();

  int slicediv = this->m_Resolution[1]; 
  double d=0.0, f1=0.0;

  i = 0;
  int slice; 
  
  while( i < m_NumNodes )
  {
  slice = i/slicediv;
    xs = ys = zs = -1.0; 
  x = points.Value();

  coord[0] = (int) x[0];
  coord[1] = (int) x[1];
  coord[2] = (int) x[2];   //slice + m_FirstSlice;

  extends[0] = x[0];
  extends[1] = x[1];
  extends[2] = x[2];
  extend[0] = (int) x[0];
  extend[1] = (int) x[1];
  extend[2] = (int) x[2];

  f[0] = x[0] - m_Center[0];
  f[1] = x[1] - m_Center[1];
  f[2] = x[2] - m_Center[2];

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

  while (t < 100.0){
    extends[0] -= n[0];
    extends[1] -= n[1];
    extends[2] -= n[2];
      extend[0] = (int) (extends[0]+1);
      extend[1] = (int) (extends[1]+1);
    extend[2] = (int) (extends[2]+1);  //slice;
    if ((extend[0] <= 0) || (extend[1] <= 0) || (extend[2] <= 0)) {
      t += 1.0;
    continue;
    }

    extend[0] = (int) (extends[0]);
      extend[1] = (int) (extends[1]);
    extend[2] = (int) (extends[2]);
    if ((extend[0] >= m_ImageWidth) || (extend[1] >= m_ImageHeight) || 
      (extend[2] >= m_ImageDepth)) {
      t += 1.0;
    continue;
    }

    label = m_Potential->GetPixel(extend);
    if ( label == m_ObjectLabel ) break;

    t += 1.0;
  }

  if (t > 5.0) {
    f[0] = extends[0] + 5.0*n[0];
    f[1] = extends[1] + 5.0*n[1];
    f[2] = extends[2] + 5.0*n[2];
    points.Value() = f; 
  }

  ++forces;
  ++forcedata;
  ++points;
  ++normals;
  ++i;
  }
}

/*
 * compute the shrink force when the model is shrink to fit to the objects.
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMeshFilter<TInputMesh, TOutputMesh>
::ComputeShrinkForce()
{
  int i, p, label, l=0; 
  IndexType coord = {0, 0, 0};
  IndexType extend = {0, 0, 0};
  float extends[3], fo, t, xs, ys, zs; 
  FloatVector n1, n, vec1, vec2; 
  float max;
  IPT x, y, z, f;
  IPT* y_pt;
  IPT* z_pt;
  y_pt = &y;
  z_pt = &z;

  InputPointsContainerPointer     Points = m_Locations->GetPoints();
  InputPointsContainerIterator      points = Points->Begin();

  InputPointsContainerPointer     myForces = m_Forces->GetPoints();
  InputPointsContainerIterator      forces = myForces->Begin();

  InputPointsContainerPointer     myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator      normals = myNormals->Begin();

  InputPointDataContainerPointer    myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator   forcedata = myForceData->Begin();

  InputPointDataContainerPointer    myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator   pointstatus = myPointData->Begin();

  int slicediv = this->m_Resolution[1]; 
  double d=0.0, f1=0.0;

  i = 0;
  int slice; 
  
  while( i < m_NumNodes )
  {
  slice = i/slicediv;
    xs = ys = zs = -1.0; 
  x = points.Value();

  coord[0] = (int) x[0];
  coord[1] = (int) x[1];
  coord[2] = (int) x[2];   //slice + m_FirstSlice;

    if ( (coord[0]>0)&&(coord[0]<m_ImageWidth)&&(coord[1]>0)&&
    (coord[1]<m_ImageHeight)&&(coord[2]>0)&&(coord[2]<m_ImageDepth)) {
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

  p = -1;
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
    extend[2] = (int) (extends[2]+1);  //slice;
    if ((extend[0] <= 0) || (extend[1] <= 0) || (extend[2] <= 0)) {
      t += 1.0;
    continue;
    }

    extend[0] = (int) (extends[0]);
      extend[1] = (int) (extends[1]);
    extend[2] = (int) (extends[2]);
    if ((extend[0] >= m_ImageWidth) || (extend[1] >= m_ImageHeight) || 
      (extend[2] >= m_ImageDepth)) {
      t += 1.0;
    continue;
    }

    label = m_Potential->GetPixel(extend);
    if ( label == m_ObjectLabel ) break;

    t += 1.0;
  }

  if (t < 2) pointstatus.Value() = 1.0;
  else {
    pointstatus.Value() = 0.0;
//    m_ImageOutput->SetPixel(coord, 1);
  }


  fo = sqrt(f[0]*f[0]+f[1]*f[1]+f[2]*f[2]);
    f[0] = t*400*f[0]*xs/fo; 
    f[1] = t*400*f[1]*ys/fo;
//  f[2] = t*400*f[2]*zs/fo;
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
}

/*
 * compute the balloon force when the model is expand from inside of the model.
 */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMeshFilter<TInputMesh, TOutputMesh>
::ComputeForce()
{
  int i, p, label, l=0; 
  IndexType coord = {0, 0, 0};
  IndexType extend = {0, 0, 0};
  float extends[3], fo, t, xs, ys, zs; 
  FloatVector n1, n, vec1, vec2; 
  float max;
  IPT x, y, z, f;
  IPT* y_pt;
  IPT* z_pt;
  y_pt = &y;
  z_pt = &z;

  InputPointsContainerPointer     Points = m_Locations->GetPoints();
  InputPointsContainerIterator      points = Points->Begin();

  InputPointsContainerPointer     myForces = m_Forces->GetPoints();
  InputPointsContainerIterator      forces = myForces->Begin();

  InputPointsContainerPointer     myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator      normals = myNormals->Begin();

  InputPointDataContainerPointer    myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator   forcedata = myForceData->Begin();

  InputPointDataContainerPointer    myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator   pointstatus = myPointData->Begin();

  int slicediv = this->m_Resolution[1]; 
  double d=0.0, f1=0.0;

  i = 0;
  int slice; 
  
  while( i < m_NumNodes )
  {
  slice = i/slicediv;
    xs = ys = zs = 1.0; 
  x = points.Value();

  coord[0] = (int) x[0];
  coord[1] = (int) x[1];
  coord[2] = (int) x[2];   //slice + m_FirstSlice;

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

  p = -1;
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
    if ((extend[0] >= m_ImageWidth) || (extend[1] >= m_ImageHeight) || 
      (extend[2] >= m_ImageDepth)) break;

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
//  f[2] = 0;

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

/*
 * compute the derivatives using d'- Kd = f 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::ComputeDt()
{
  int i; 
  const unsigned long *tp;
  typename TriCell::Pointer testCell;
  testCell.TakeOwnership( new TriCell );

  int npts = 3;

  InputCellsContainerPointer    myCells = m_Locations->GetCells();
  InputCellsContainerIterator   cells = myCells->Begin();

  InputPointsContainerPointer     myForces = m_Forces->GetPoints();
  InputPointsContainerIterator    forces = myForces->Begin();

  InputPointsContainerPointer   myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator    derives = myDerives->Begin();

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

/*
 * When there is new nodes added, must do a reset to reallocate
 * the memory and redistribute the nodes and reconstruct the cells,
 * now the mthod is only suitable for 2D models, it will be a much
 * different case for 3D model 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::Reset()
{
  int i, j, cell=0, slice, numnewnodes, res; 
  float status, d[3] = {0,0,0}, w;
  IPT x, y, z;
  IPT* x_pt;
  IPT* y_pt; 
  x_pt = &x;
  const unsigned long *tp;

  unsigned long tripoints[3];
   
  InputPointsContainerPointer     myForces = m_Forces->GetPoints();
//  myForces->Reserve(m_NumNodes);
  InputPointsContainerIterator      forces = myForces->Begin();

  InputPointsContainerPointer     myPoints = m_Locations->GetPoints();
//  myPoints->Reserve(m_NumNodes);
  InputPointsContainerIterator      points = myPoints->Begin();
 
  InputPointsContainerPointer     myNormals = m_Normals->GetPoints();
//  myNormals->Reserve(m_NumNodes);
  InputPointsContainerIterator      normals = myNormals->Begin();

  InputPointsContainerPointer     myDerives = m_Derives->GetPoints();
//  myDerives->Reserve(m_NumNodes);
  InputPointsContainerIterator      derives = myDerives->Begin();

  InputPointsContainerPointer     myDisplacements = m_Displacements->GetPoints();
//  myDisplacements->Reserve(m_NumNodes);
  InputPointsContainerIterator      displacements = myDisplacements->Begin();

  InputPointDataContainerPointer    myForceData = m_Forces->GetPointData();
  myForceData->Reserve(m_NumNodes);
  InputPointDataContainerIterator   forcedata = myForceData->Begin();

  InputCellsContainerPointer      myCells = m_Locations->GetCells();
  myCells->Reserve(m_NumCells);
  InputCellsContainerIterator     cells = myCells->Begin(); 
  
  InputCellDataContainerPointer     myCellData = m_Locations->GetCellData();
  myCellData->Reserve(m_NumCells);
  InputCellDataContainerIterator    celldata = myCellData->Begin(); 

  InputCellsContainerPointer      myOutCells = m_Output->GetCells();
  myOutCells->Reserve(m_NumCells);
  InputCellsContainerIterator     outcells = myOutCells->Begin(); 
  
  InputCellDataContainerPointer     myOutCellData = m_Output->GetCellData();
  myOutCellData->Reserve(m_NumCells);
  InputCellDataContainerIterator    outcelldata = myOutCellData->Begin();
   
  typename TriCell::Pointer       insertCell;
  insertCell.TakeOwnership( new TriCell ); 

  i = 0;
  j = 0;

  int p = 0, jn;

    for(int i=0; i < m_Resolution[0]-1 ; i++) {
    for (int j=0; j<m_Resolution[1]; j++) {
        jn = (j+1)%m_Resolution[1]; 
        tripoints[0] = i*m_Resolution[1]+j; 
        tripoints[1] = tripoints[0]-j+jn; 
        tripoints[2] = tripoints[0]+m_Resolution[1]; 
      insertCell->SetPointIds(tripoints);
      m_Locations->SetCell(p, insertCell);
      m_Locations->SetCellData(p, (PT)3.0);
      p++;
      insertCell.TakeOwnership( new TriCell );
      tripoints[0] = tripoints[1]; 
      tripoints[1] = tripoints[0]+m_Resolution[1]; 
      insertCell->SetPointIds(tripoints);
      m_Locations->SetCell(p, insertCell);
      m_Locations->SetCellData(p, (PT)3.0);
      p++;
      insertCell.TakeOwnership( new TriCell );
    }
    }
 
// store cells containing the south pole nodes
    for (int j=0; j<m_Resolution[1]; j++) {
      jn = (j+1)%m_Resolution[1]; 
      tripoints[0] = m_NumNodes-2; 
      tripoints[1] = jn; 
      tripoints[2] = j; 
    insertCell->SetPointIds(tripoints);
    m_Locations->SetCell(p, insertCell);
    m_Locations->SetCellData(p, (PT)1.0);
    p++;
    insertCell.TakeOwnership( new TriCell );
    }

// store cells containing the north pole nodes
    for (int j=0; j<m_Resolution[1]; j++) {
      jn = (j+1)%m_Resolution[1]; 
      tripoints[2] = (m_Resolution[0]-1)*m_Resolution[1]+j; 
    tripoints[1] = m_NumNodes-1; 
      tripoints[0] = tripoints[2]-j+jn; 
    insertCell->SetPointIds(tripoints);
    m_Locations->SetCell(p, insertCell);
    m_Locations->SetCellData(p, (PT)2.0);
    p++;
    insertCell.TakeOwnership( new TriCell );
    }
  
/*  m_NumCells = p;

  K = (vnl_matrix_fixed<double,4,4>**) 
    malloc(sizeof(vnl_matrix_fixed<double,4,4>*)*m_NumCells);
  
//  InputCellDataContainerIterator    celldata = m_Locations->GetCellData()->Begin();

//  int j=0;
  while (celldata != m_Locations->GetCellData()->End()){
    w = celldata.Value();
    ++celldata;
    switch ((int)(w)) { 
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
*/ 
}

/*
 * update the displacements using d_{new} = d_{old} + timestep*d' 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::Advance()
{
  typename TInputMesh::PointType s, d, ds; 

  int i;

  m_ModelXUpLimit = 0;
  m_ModelXDownLimit = m_ImageWidth;
  m_ModelYUpLimit = 0;
  m_ModelYDownLimit = m_ImageHeight;
  m_ModelZUpLimit = 0;
  m_ModelZDownLimit = m_ImageDepth;


  InputPointsContainerPointer   myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator    derives = myDerives->Begin();

  InputPointsContainerPointer   myPoints = m_Locations->GetPoints();
  InputPointsContainerIterator    points = myPoints->Begin();
 
  i = 0;
  while( derives != myDerives->End() ) {
    ds = derives.Value();
  s = points.Value();
    s[0] += TimeStep*ds[0]; 
    s[1] += TimeStep*ds[1]; 
    s[2] += TimeStep*ds[2]; 
  if ( m_ModelYDownLimit > s[1] ) m_ModelYDownLimit = s[1];
  if ( m_ModelYUpLimit < s[1] ) m_ModelYUpLimit = s[1];
  if ( m_ModelXDownLimit > s[0] ) m_ModelXDownLimit = s[0];
  if ( m_ModelXUpLimit < s[0] ) m_ModelXUpLimit = s[0];
  if ( m_ModelZDownLimit > s[2] ) m_ModelZDownLimit = s[2];
  if ( m_ModelZUpLimit < s[2] ) m_ModelZUpLimit = s[2];
//  if ( i < m_NumNodes - 2 ) {
/* disabled for shrink test
    if (s[0] < 0) {
    s[0] = 0;
    }
    if (s[1] < 0) {
    s[1] = 0;
    }
    if (s[2] < 0) {
    s[2] = 0;
    }
    if (s[2] > m_ImageDepth) {
    s[2] = m_ImageDepth-0.001;
    }
    if (s[0] > m_ImageWidth) {
    s[0] = m_ImageWidth-0.001;
    }
    if (s[1] > m_ImageHeight) {
    s[1] = m_ImageHeight-0.001;
    }
*/
    points.Value() = s;
//  }
  ++derives; 
  ++points;
  ++i;
  } 

}

/*
 * copy the content of m_Location into output 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::ComputeOutput() 
{
 
  int i;
  typename TriCell::CellAutoPointer  insertCell;
  unsigned long tripoints[3];
  const unsigned long *tp;
  float x;

  m_Output = this->GetOutput();

  OutputPointsContainerPointer    myPoints = m_Output->GetPoints();
  myPoints->Reserve(m_NumNodes);
  OutputPointsContainerIterator   points = myPoints->Begin();

  InputPointsContainerPointer   myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator    locations = myLocations->Begin();

  InputCellsContainerPointer    myCells = m_Locations->GetCells();
  InputCellsContainerIterator   cells = myCells->Begin(); 
  
  InputCellDataContainerPointer   myCellData = m_Locations->GetCellData();
  InputCellDataContainerIterator  celldata = myCellData->Begin(); 

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
  insertCell.TakeOwnership( new TriCell );
  insertCell->SetPointIds(tripoints);
  m_Output->SetCell(i, insertCell);
  x = celldata.Value();
  m_Output->SetCellData(i, (PT)x);
  ++cells;
  ++celldata;
  }

  m_Output->SetCellsAllocationMethod( 
     OutputMeshType::CellsAllocatedDynamicallyCellByCell );

}

/*
 * copy the content of m_Location into output 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::GenerateData() 
{
  while (m_Step < m_StepThreshold2) 
    {
    this->ComputeNormals();
    if (m_Step > m_StepThreshold1) 
      {
      this->GradientFit();
      }
    else  
      {
      this->ComputeForce();
      }
    this->ComputeDt();
    this->Advance();
    this->ACDSearch();
    this->NodesRearrange();
    this->ComputeOutput();
    m_Step++;
    }
}

/*
 * when almost all the nodes is at the estimated boundary, use
 * gapsearch to fit the model to more complicated shapes
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::GapSearch() 
{
        
}

/*
 * add new nodes into the model 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::NodeAddition() 
{
  m_NewNode = true;
}

/*
 * add a new slice into the model 
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::SliceAddition(int i) 
{
  typename TInputMesh::PointType s, s1, d1, d2;

  InputPointsContainerPointer   myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator  locations = myLocations->Begin();

  InputPointDataContainerPointer  myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator pointdata = myPointData->Begin();

  InputPointsContainerPointer   myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator  displacements = myDisplacements->Begin();

  InputPointsContainerPointer   myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator  normals = myNormals->Begin();
  
  int j, k, l;

  j = 0;
  k = 0;
  while (j < i-1) {
    k = 0;
  while ( k < m_Resolution[1] ) {
    ++locations;
    k++;
  }
  j++;
  } 

  j = 0;
  while ( j < m_Resolution[1] ) {
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
  while ( j < m_Resolution[1] ) {
      normals.Value() = locations.Value();    
    ++normals;
    j++;
  }
  } 

  if (i == m_Resolution[0]) {
  locations = myLocations->Begin();
  j = 0;
  while ( j < m_NumNodes - 1 ) {
    ++locations;
    j++;
  } 
    j = 0;
  while ( j < m_Resolution[1] ) {
      normals.Value() = locations.Value();    
    ++normals;
    j++;
  }
  } 

  if ( (i > 0) && ( i < m_Resolution[0]) ) {
  j = 0;
  while ( j < m_Resolution[1] ) {
    normals.Value() = locations.Value();    
    ++normals;
    ++locations;
    j++;
    }
  }

  if ( (i < 0) || ( i > m_Resolution[0]) ) return;

  displacements = myDisplacements->Begin();
  normals = myNormals->Begin();
  j = 0;
  while ( j < m_Resolution[1] ) {
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

  m_NumNodes = m_NumNodes+m_Resolution[1];
  m_NumCells = 2*m_Resolution[1]+m_NumCells;
  m_Resolution[0] += 1;

  OutputPointsContainerPointer    myPoints = m_Output->GetPoints();
  myPoints->Reserve(m_NumNodes);

  myDisplacements->Reserve(m_NumNodes);
    displacements = myDisplacements->Begin();

    InputPointsContainerPointer     myForces = m_Forces->GetPoints();
  myForces->Reserve(m_NumNodes);
    InputPointsContainerIterator  forces = myForces->Begin();

  InputPointsContainerPointer     myDerives = m_Derives->GetPoints();
    myDerives->Reserve(m_NumNodes);
    InputPointsContainerIterator  derives = myDerives->Begin();

    InputCellsContainerPointer    myCells = m_Locations->GetCells();
  myCells->Reserve(m_NumCells);
    InputCellsContainerIterator   cells = myCells->Begin(); 
  
    InputCellDataContainerPointer myCellData = m_Locations->GetCellData();
  myCellData->Reserve(m_NumCells);
    InputCellDataContainerIterator  celldata = myCellData->Begin();    

    normals = myNormals->Begin();

  j = 0;
  k = 0;
  l = 0;
  while (j < m_NumNodes) {
    if ( k == i) {
    while ( l < m_Resolution[1] ) {
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

    if ( l == m_Resolution[1] ) {
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


/*
 * fit the model to the gradient information
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::GradientFit() 
{
  int i, j, k, node, slice, xlowlimit, xhighlimit, ylowlimit, yhighlimit;
  float dis;
  IndexType coord = {0, 0, 0};
  float grad[3];
  IPT v1, v2;
  IPT* v1_pt;
  IPT* v2_pt;
  v1_pt = &v1;
  v2_pt = &v2;
  int slicediv = this->m_Resolution[1]; 

  typename TInputMesh::PointType s, d;

//  GradientIterator  it(m_Gradient, m_Gradient->GetBufferedRegion());
//  it.Begin();

  InputPointsContainerPointer   myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator  locations = myLocations->Begin();

  InputPointsContainerPointer myForces = m_Forces->GetPoints();
  InputPointsContainerIterator  forces = myForces->Begin();

  InputPointsContainerPointer myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator  normals = myNormals->Begin();

  InputPointDataContainerPointer    myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator   forcedata = myForceData->Begin();

  s[0] = 0;
  s[1] = 0;
  s[2] = 0;

/*
  while ( forces != myForces->End() ) {
  s = normals.Value();
  dis = sqrt(s[0]*s[0]+s[1]*s[1]);
  s[0] = 15*s[0]/dis;
  s[1] = 15*s[1]/dis;
  s[2] = 0;
  forces.Value() = s;
  forcedata.Value() = 0;
  ++forces;
  ++normals;
  ++forcedata;
  }
*/

  i = 0;
  j = 0;
  k = 0;
  dis = 0;
  node = 0;
  slice = 0;

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
  d[0] = 10*m_Gradient->GetPixel(coord)[0];
  d[1] = 10*m_Gradient->GetPixel(coord)[1]; 
  d[2] = 10*m_Gradient->GetPixel(coord)[2]; 
  forces.Value() = d;

  ++forces;
  ++locations;
//  ++forcedata;
  }

}

/*
 * fit the model to the gradient information
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::ComputeNormals() 
{
  const unsigned long *tp;
  IPT v1, v2, v3, v4;
  IPT* v1_pt;
  IPT* v2_pt;
  IPT* v3_pt;
  v1_pt = &v1;
  v2_pt = &v2;
  v3_pt = &v3;
  float coa, cob, coc ;
  float absvec ;

  InputCellsContainerPointer      myCells = m_Locations->GetCells();
  InputCellsContainerIterator     cells = myCells->Begin();

  InputPointsContainerPointer       myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator      normals = myNormals->Begin();

  static float d[3]={0, 0, 0};
  while( normals != myNormals->End() )
  {
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
  while( normals != myNormals->End() )
  {
  v1 = normals.Value();
  absvec = sqrt ((double) ((v1[0]*v1[0]) + (v1[1]*v1[1]) + (v1[2]*v1[2])));
  v1[0] = v1[0]/absvec;
  v1[1] = v1[1]/absvec;
  v1[2] = v1[2]/absvec;
  normals.Value() = v1;
  ++normals;
  }

}

/*
 * fit the model to the gradient information
 */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::NodesRearrange()
{
  int i, j, k, new_node=1;
  float dis, l1, l2, *d_ptr, d;
  float* length;
  IPT v1, v2, v3, v4, v_southpole, v_northpole;
  IPT* v1_pt;
  IPT* v2_pt;
  IPT* v3_pt;
  d_ptr = &d;
  v1_pt = &v1;
  v2_pt = &v2;
  v3_pt = &v3;

  typename TInputMesh::PointType s, s1, d1, d2;

  InputPointsContainerPointer   myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator  locations = myLocations->Begin();

  InputPointDataContainerPointer  myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator pointdata = myPointData->Begin();

  InputPointsContainerPointer   myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator  displacements = myDisplacements->Begin();

  InputPointsContainerPointer   myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator  normals = myNormals->Begin();

  InputPointsContainerPointer   myForces = m_Forces->GetPoints();
  InputPointsContainerIterator  forces = myForces->Begin();
  InputPointsContainerIterator  forcescopy;

  length = (float*) malloc(sizeof(float)*m_Resolution[0]);

  for (j = 0; j < m_Resolution[0]; j++) {
    
  v1 = locations.Value();
  s = locations.Value();
  ++locations;
  s1 = locations.Value();
  i = j*m_Resolution[1] + 1;
  forcescopy = forces;
  forces++;
  
  while ( (i+1)%m_Resolution[1] != 0 ) {
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
  i = j*m_Resolution[1];
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
//  v1[0] = v2[0];
//  v1[1] = v2[1];
//  v1[2] = v2[2];
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
/*
  forces = myForces->Begin();

  for (j = 0; j < m_Resolution[0]; j++) {
  dis = 0;
  i = 0;
  s = forces.Value();
//  forcescopy = forces;

  while ( i < m_Resolution[1] - 1 ) {
      v1 = forces.Value();
    ++forces;
      v2 = forces.Value();
//    m_Displacements->GetPointData(i+j*m_Resolution[1], d_ptr);
    dis += sqrt((v1[0]-v2[0])*(v1[0]-v2[0])+(v1[1]-v2[1])*(v1[1]-v2[1])+(v1[2]-v2[2])*(v1[2]-v2[2]));
    i++;
  }
//  m_Displacements->GetPointData(i+j*m_Resolution[1], d_ptr);
  dis += sqrt((s[0]-v2[0])*(s[0]-v2[0])+(s[1]-v2[1])*(s[1]-v2[1])+(s[2]-v2[2])*(s[2]-v2[2]));
  length[j] = dis/m_Resolution[1];
  ++forces;
  }
*/
  if ( m_NewNode ) {
    m_NumNodes = 2*(m_NumNodes-2)+2;
  m_NumCells = 2*m_NumCells;
//  m_Resolution[1] = m_Resolution[1]*2;
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
    InputPointsContainerIterator  derives = myDerives->Begin();

    InputCellsContainerPointer    myCells = m_Locations->GetCells();
  myCells->Reserve(m_NumCells);
    InputCellsContainerIterator   cells = myCells->Begin(); 
  
    InputCellDataContainerPointer myCellData = m_Locations->GetCellData();
  myCellData->Reserve(m_NumCells);
    InputCellDataContainerIterator  celldata = myCellData->Begin();
  
  for (j = 0; j < m_Resolution[0]; j++) length[j] = length[j]*0.5;
  }

  forces = myForces->Begin();
/*
  for (j = 0; j < m_Resolution[0]; j++) {
    k = 1;
  i = 0;
  l1 = 0;
  v1 = forces.Value();
  normals.Value() = v1;
  ++normals;
  v3 = forces.Value();
  while ( i < m_Resolution[1] - 1 ) {
    v1 = forces.Value();
    ++forces;
    v2 = forces.Value();
    dis = sqrt((v1[0]-v2[0])*(v1[0]-v2[0])+(v1[1]-v2[1])*(v1[1]-v2[1])+(v1[2]-v2[2])*(v1[2]-v2[2]));
    l2 = -1*l1;
    l1 += dis;
//    m_Displacements->GetPointData(i+j*m_Resolution[1], d_ptr);
    while ( l1 > length[j] ) {
      if (k==m_Resolution[1]*new_node) break;
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
    if (k==m_Resolution[1]*new_node) break;
  }

  if (k==m_Resolution[1]*new_node) {
    while (i < m_Resolution[1]) {
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
//  m_Displacements->GetPointData(i+j*m_Resolution[1], d_ptr);
  while ( l1 > length[j] ) {
    if (k==m_Resolution[1]*new_node) break;
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
*/
  locations = myLocations->Begin();
  normals = myNormals->Begin();
  displacements = myDisplacements->Begin();

  i = 0;
  while ( i < m_NumNodes-2 ) {
//  v1 = normals.Value();
  v1 = forces.Value();
  v2 = locations.Value();
  v3 = displacements.Value();
  v3[0] += v1[0] - v2[0];
  v3[1] += v1[1] - v2[1];
  v3[2] += v1[2] - v2[2];
  locations.Value() = v1;
  displacements.Value() = v3;
//  ++normals;
  ++forces;
  ++locations;
  ++displacements;
  i++;
  }

  if ( m_NewNode ) {
    m_Resolution[1] = m_Resolution[1]*2;
    locations.Value() = v_southpole;
  ++locations;
  locations.Value() = v_northpole;
  
  v3[0] = 0;
  v3[1] = 0;
  v3[2] = 0;
  i = 0;
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
  m_NewNode = false;
  Reset();
  SetStiffnessMatrix();
  }

/*
  locations = myLocations->Begin();
  j = 0;
  for (; j < m_Resolution[0]; j++) {
  dis = 0;
  i = 0;
  s = locations.Value();
  while ( i < m_Resolution[1] - 1 ) {
      v1 = locations.Value();
    ++locations;
      v2 = locations.Value();
    dis = sqrt((v1[0]-v2[0])*(v1[0]-v2[0])+(v1[1]-v2[1])*(v1[1]-v2[1])+(v1[2]-v2[2])*(v1[2]-v2[2]));
    i++;
  }
  dis = sqrt((s[0]-v2[0])*(s[0]-v2[0])+(s[1]-v2[1])*(s[1]-v2[1])+(s[2]-v2[2])*(s[2]-v2[2]));
  ++locations;
  }
*/
  free(length);
}

template <typename TInputMesh, typename TOutputMesh>
void
DeformableMeshFilter<TInputMesh, TOutputMesh>
::ACDSearch() 
{
  int i, j, l, m, n, pt1, pt2;
  float s;
  IPT v, v1, v2, v3;
  m_ACD = (int**) malloc(sizeof(int *)*m_ImageHeight/2);

  for (i=0; i<m_ImageHeight/2; i++) {
  m_ACD[i] = (int*) malloc(sizeof(int)*m_ImageWidth/2);
  }
  
  InputPointsContainerPointer   myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator  locations = myLocations->Begin();
  InputPointsContainerIterator  locationscopy;
  InputPointsContainerIterator  locationscopy1;
  InputPointsContainerPointer   myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator  displacements = myDisplacements->Begin();
  InputPointsContainerIterator  dpcopy;
  InputPointsContainerIterator  dpcopy1;

  for (j = 0; j < m_Resolution[0]; j++) {
  i = 0;
  locationscopy = locations;
  dpcopy = displacements;

  for (l=m_ModelYDownLimit/2; l<=m_ModelYUpLimit/2; l++) 
    for (m=m_ModelXDownLimit/2; m<=m_ModelXUpLimit/2; m++)
    m_ACD[l][m] = -1;

  for (; i < m_Resolution[1]; i++) {
    v = locations.Value();
    ++locations;
    ++displacements;
    l = (int)(v[0]/2);
    m = (int)(v[1]/2);
    if (m_ACD[m][l] == -1) m_ACD[m][l] = i;
    else {
    if (((i - m_ACD[m][l]) > m_Resolution[1]/10) && 
      ((m_Resolution[1]-i+m_ACD[m][l])>m_Resolution[1]/10)) {
      locationscopy1 = locationscopy;
      n = 0;
      v1[0] = 0;
      v1[1] = 0;
      v1[2] = 0;
      if ( (i - m_ACD[m][l]) < 0.5*m_Resolution[1] ) {
      if (m_ACD[m][l] == 0) pt1 = m_Resolution[1] - 1;
      else pt1 = m_ACD[m][l] - 1;
      if (i == m_Resolution[1] - 1) pt2 = 0;
      else pt2 = i + 1;
      while (n<m_Resolution[1]) {
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
        while (n<m_Resolution[1]) {
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
        while (n<m_Resolution[1]) {
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
        while (n<m_Resolution[1]) {
          if ( n == i ) {
          v = locationscopy1.Value();
        }
          ++locationscopy1;
          n++;
        }
        locationscopy1 = locationscopy;
        n = 0;
        while (n<m_Resolution[1]) {
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
  for (i=0; i<m_ImageHeight/2; i++) {
  free(m_ACD[i]);
  }
  free(m_ACD);
  
}

} // end namespace itk

#endif
