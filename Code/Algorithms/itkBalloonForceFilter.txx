/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBalloonForceFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBalloonForceFilter_txx
#define _itkBalloonForceFilter_txx

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkBalloonForceFilter.h"

namespace itk
{
/*
 * standard 
 */
template <typename TInputMesh, typename TOutputMesh>
BalloonForceFilter<TInputMesh, TOutputMesh>
::BalloonForceFilter()
{
  m_Step = 0;
  m_GradientBegin = 0;
  m_DistanceToBoundary = 100;
  m_Center.Fill(0);
  m_Stiffness.Fill(0);
  m_TimeStep = 0.0;
  m_DistanceForGradient = 0.0;
  m_Resolution = 0;
  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
}

template <typename TInputMesh, typename TOutputMesh>
BalloonForceFilter<TInputMesh, TOutputMesh>
::~BalloonForceFilter()
{
  unsigned int i;
  for (i = 0; i < m_NewNodeLimit; i++)
    {
    delete m_NewNodes[i];
    }
  delete []m_NewNodes;
}

/*
 * PrintSelf
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Balloon Force Filter" << std::endl;

}// end PrintSelf

/* Set default value of parameters and initialize local data container 
 *  such as forces, displacements and displacement derivatives. */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::Initialize()
{
  m_NumberOfNodes = this->GetInput(0)->GetNumberOfPoints();
  m_NumberOfCells = this->GetInput(0)->GetNumberOfCells();

  m_Forces = InputMeshType::New();
  m_Displacements = InputMeshType::New();
  m_Derives = InputMeshType::New();
  m_Normals = InputMeshType::New();
  m_Locations = InputMeshType::New();

  InputPointsContainerPointer      myPoints = this->GetInput(0)->GetPoints();
  InputPointsContainerIterator     points = myPoints->Begin();

  InputPointsContainerPointer      myForces = m_Forces->GetPoints();
  myForces->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator     forces = myForces->Begin();

  InputPointsContainerPointer      myDerives = m_Derives->GetPoints();
  myDerives->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator     derives = myDerives->Begin();

  InputPointsContainerPointer      myDisplacements = m_Displacements->GetPoints();
  myDisplacements->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator     displacements = myDisplacements->Begin();

  InputPointsContainerPointer      myNormals = m_Normals->GetPoints();
  myNormals->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator     normals = myNormals->Begin();

  InputPointsContainerPointer      myLocations = m_Locations->GetPoints();
  myLocations->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator     locations = myLocations->Begin();

  InputCellsContainerPointer       myCells = this->GetInput(0)->GetCells();
  InputCellsContainerIterator      cells = myCells->Begin(); 

  InputCellDataContainerPointer    myCellData = this->GetInput(0)->GetCellData();
  InputCellDataContainerIterator   celldata = myCellData->Begin(); 

  ImageSizeType ImageSize = m_Gradient->GetBufferedRegion().GetSize();

  m_NumNewNodes = 0;
  m_NewNodesExisted = 0;
  m_NewNodeLimit = 200;
  m_ObjectLabel = m_Potential->GetPixel(m_Center);
  m_NewNodes = (float**) malloc(sizeof(float *)*m_NewNodeLimit);

  //---------------------------------------------------------------------
  //Get the image width/height and dePixelTypeh
  //---------------------------------------------------------------------   
  m_ImageWidth  = ImageSize[0];
  m_ImageHeight = ImageSize[1];
//  m_ImageDePixelTypeh  = ImageSize[2];

  float d[3] = {0,0,0}, fd=0; 

//  TimeStep = 0.001; 

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

  for (int i=0; i<m_NumberOfNodes-2; i++  )
  {
  m_Forces->SetPointData(i, 1.0);
  m_Locations->SetPointData(i, 0.0);
  m_Derives->SetPointData(i, 0.0);
  m_Displacements->SetPointData(i, 0.0);
  }

  for (int i = 0; i < m_NewNodeLimit; i++) {
  m_NewNodes[i] = (float*) malloc(sizeof(float)*5);
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

  typename TriCell::CellAutoPointer insertCell;
  unsigned long tripoints[3];
  const unsigned long *tp;
  float x;

  for (int i=0; i<m_NumberOfCells; i++) {
  tp = cells.Value()->GetPointIds();
  tripoints[0] = tp[0];
  tripoints[1] = tp[1];
  tripoints[2] = tp[2];
  insertCell.TakeOwnership( new TriCell );
  insertCell->SetPointIds(tripoints);
  m_Locations->SetCell(i, insertCell);
  x = celldata.Value();
  m_Locations->SetCellData(i, (PixelType)x);
  ++cells;
  ++celldata;
  }

} 

/*
 * set the stiffness matrix 
 */
template <typename TInputMesh, typename TOutputMesh>
void 
BalloonForceFilter<TInputMesh, TOutputMesh>
::SetStiffnessMatrix () 
{ 
  InputCellDataContainerPointer     myCellData = m_Locations->GetCellData();
  InputCellDataContainerIterator    celldata = myCellData->Begin();

  K = (vnl_matrix_fixed<double,4,4>**) malloc(sizeof(vnl_matrix_fixed<double,4,4>*)*m_NumberOfCells);
  float x;  

  float us = vnl_math::pi / 1; 
  float vs = 2.0*vnl_math::pi / m_Resolution; 
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
  int i, p, l=0; 
  IndexType coord = {0, 0};
  IndexType extend = {0, 0};
  float extends[2], fo, t, xs, ys, zs; 
  FloatVector n1, n, vec1, vec2; 
  float max;
  IPixelType x, y, z, f;
  IPixelType* y_PixelType;
  IPixelType* z_PixelType;
  y_PixelType = &y;
  z_PixelType = &z;
  float dist = 0.0;
  unsigned short label;

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

  int slicediv = this->m_Resolution; 
  double d=0.0, f1=0.0;

  i = 0;
  int slice; 
  
  while( i != m_NumberOfNodes - 2 )
  {
  slice = i/slicediv;
  xs = ys = zs = 1.0; 
  x = points.Value();

  coord[0] = (int) x[0];
  coord[1] = (int) x[1];
  coord[2] = 0;

  label = (unsigned short)m_Potential->GetPixel(coord);
  if ( label != m_ObjectLabel ) {
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
//  extends[2] = x[2];
  extend[0] = (int) x[0];
  extend[1] = (int) x[1];
//  extend[2] = (int) x[2];

  f = normals.Value();

  p = -1;
  max = abs(f[0]);

  //---------------------------------------------------------------------
  // all the movement in z direction is now disabled for further test
  //---------------------------------------------------------------------  
  if ( abs(f[1]) > max ) max = abs(f[1]);
  n[0] = f[0]/max;
  n[1] = f[1]/max;

  t = 0.0;

  while (t < 5.0){
    extends[0] += n[0];
    extends[1] += n[1];
    extend[0] = (int) extends[0];
    extend[1] = (int) extends[1];
//    extend[2] = 0;
    if ((extend[0] < 0) || (extend[1] < 0) 
      || (extend[0] >= m_ImageWidth) || (extend[1] >= m_ImageHeight) 
    ) break;

    label = (unsigned short) m_Potential->GetPixel(extend);
    if ( label != m_ObjectLabel ) break;

    t += 1.0;
  }

  dist = dist + t;

  if (t < 2) pointstatus.Value() = 1.0;
  else {
    pointstatus.Value() = 0.0;
 //   m_ImageOutput->SetPixel(coord, 1);
  }
    fo = sqrt(f[0]*f[0]+f[1]*f[1]);
    f[0] = t*100*f[0]*xs/fo; 
    f[1] = t*100*f[1]*ys/fo;
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

  m_DistanceToBoundary = dist/((float)(m_NumberOfNodes-2));
  if (m_DistanceToBoundary < m_DistanceForGradient) m_GradientBegin = 1;
}

/*
 * compute the derivatives using d'- Kd = f 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::ComputeDt()
{
  int i; 
  const unsigned long *tp;
  int nPixelTypes = 3;

  InputCellsContainerPointer    myCells = m_Locations->GetCells();
  InputCellsContainerIterator   cells = myCells->Begin();

  InputPointsContainerPointer     myForces = m_Forces->GetPoints();
  InputPointsContainerIterator    forces = myForces->Begin();

  InputPointsContainerPointer   myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator    derives = myDerives->Begin();

  float p = 1; 
  i = 0;
  IPixelType v1, v2, v3;
  IPixelType* v1_PixelType;
  IPixelType* v2_PixelType;
  IPixelType* v3_PixelType;
  v1_PixelType = &v1;
  v2_PixelType = &v2;
  v3_PixelType = &v3;

  while( cells != myCells->End() )
  {
  tp = cells.Value()->GetPointIds();
  ++cells;
    m_Displacements->GetPoint (tp[0], v1_PixelType); 
    m_Displacements->GetPoint (tp[1], v2_PixelType); 
    m_Displacements->GetPoint (tp[2], v3_PixelType); 
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
    m_Forces->GetPoint (tp[0], v2_PixelType); 

  v2[0] -= v1[0]; 
  v2[1] -= v1[1]; 
  v2[2] -= v1[2];

    m_Forces->SetPoint (tp[0], v2); 
 
    m_Displacements->GetPoint (tp[0], v1_PixelType); 
    m_Displacements->GetPoint (tp[1], v2_PixelType); 
    m_Displacements->GetPoint (tp[2], v3_PixelType); 
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
    m_Forces->GetPoint (tp[1], v2_PixelType);  

  v2[0] -= v1[0]; 
  v2[1] -= v1[1]; 
  v2[2] -= v1[2];

    m_Forces->SetPoint (tp[1], v2); 
 
    m_Displacements->GetPoint (tp[0], v1_PixelType); 
    m_Displacements->GetPoint (tp[1], v2_PixelType); 
    m_Displacements->GetPoint (tp[2], v3_PixelType); 
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
    m_Forces->GetPoint (tp[2], v2_PixelType); 

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
BalloonForceFilter<TInputMesh, TOutputMesh>
::Reset()
{
  int i, j, cell=0, slice, numnewnodes, res; 
  float status, d[3] = {0,0,0}, w;
  IPixelType x, y, z;
  IPixelType* x_PixelType;
  IPixelType* y_PixelType; 
  x_PixelType = &x;
  const unsigned long *tp;

  unsigned long tripoints[3];
 
  if (m_NumNewNodes == 0) return;
  else cell = 1;

  m_NumberOfNodes = m_NumberOfNodes + m_NumNewNodes;
  m_NumberOfCells = m_NumberOfCells + m_NumNewNodes*2;
  m_NumNewNodes = 0;
  
  InputPointsContainerPointer     myForces = m_Forces->GetPoints();
  myForces->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator      forces = myForces->Begin();

  InputPointsContainerPointer     myPoints = m_Locations->GetPoints();
  myPoints->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator      points = myPoints->Begin();
 
  InputPointsContainerPointer     myNormals = m_Normals->GetPoints();
  myNormals->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator      normals = myNormals->Begin();

  InputPointsContainerPointer     myDerives = m_Derives->GetPoints();
  myDerives->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator      derives = myDerives->Begin();

  InputPointsContainerPointer     myDisplacements = m_Displacements->GetPoints();
  myDisplacements->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator      displacements = myDisplacements->Begin();

  InputPointDataContainerPointer    myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator   forcedata = myForceData->Begin();

  InputCellsContainerPointer      myCells = m_Locations->GetCells();
  myCells->Reserve(m_NumberOfCells);
  InputCellsContainerIterator     cells = myCells->Begin(); 
  
  InputCellDataContainerPointer     myCellData = m_Locations->GetCellData();
  myCellData->Reserve(m_NumberOfCells);
  InputCellDataContainerIterator    celldata = myCellData->Begin(); 

  InputCellsContainerPointer      myOutCells = m_Output->GetCells();
  myOutCells->Reserve(m_NumberOfCells);
  InputCellsContainerIterator     outcells = myOutCells->Begin(); 
  
  InputCellDataContainerPointer     myOutCellData = m_Output->GetCellData();
  myOutCellData->Reserve(m_NumberOfCells);
  InputCellDataContainerIterator    outcelldata = myOutCellData->Begin();
   
  typename TriCell::CellAutoPointer insertCell = new TriCell;
  insertCell.TakeOwnership();

  i = 0;
  j = 0;
  while (normals != myNormals->End()) {
  if (i > (int) m_NewNodes[j][3]) {
    z[0] = m_NewNodes[j][0];
    z[1] = m_NewNodes[j][1];
    z[2] = m_NewNodes[j][2];
    j++;
    normals.Value() = z;
  } else {
    x = points.Value();
    i++;
    normals.Value() = x;
    ++points;
  }

  ++normals;
  }

  normals = myNormals->Begin();
  points = myPoints->Begin();

  while (points != myNormals->End()) {
  points.Value() = normals.Value();
  ++points;
  ++normals;
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

  i = 0;
  j = 0;
  res = 0;
  while ( outcells != myOutCells->End() ) {
  tp = cells.Value()->GetPointIds();

  if ( tp[0] > (unsigned long) m_NewNodes[j][3] ) {
  } else {
    outcells.Value() = cells.Value();
    ++cells;
    ++outcells;
    outcells.Value() = cells.Value();
    ++cells;
    ++outcells;
  }

  i++;
  if (tp[0] != tp[1] - 1) {
    res = i;
    i = 0;
  }
  }

  if (cell == 1) {
  int p = 0, jn;

  // store cells containing the south pole nodes
    for (int j=0; j<m_Resolution; j++) {
      jn = (j+1)%m_Resolution; 
      tripoints[0] = m_NumberOfNodes-2; 
      tripoints[1] = jn; 
      tripoints[2] = j; 
    insertCell->SetPointIds(tripoints);
    m_Locations->SetCell(p, insertCell);
    m_Locations->SetCellData(p, (PixelType)1.0);
    p++;
    insertCell.TakeOwnership( new TriCell );
    }

  // store cells containing the north pole nodes
    for (int j=0; j<m_Resolution; j++) {
      jn = (j+1)%m_Resolution; 
      tripoints[2] = (1-1)*m_Resolution+j; 
    tripoints[1] = m_NumberOfNodes-1; 
      tripoints[0] = tripoints[2]-j+jn; 
    insertCell->SetPointIds(tripoints);
    m_Locations->SetCell(p, insertCell);
    m_Locations->SetCellData(p, (PixelType)2.0);
    p++;
      insertCell = TriCell::New();
    }
  
  m_NumberOfCells = p;

  K = (vnl_matrix_fixed<double,4,4>**) 
    malloc(sizeof(vnl_matrix_fixed<double,4,4>*)*m_NumberOfCells);
  
  InputCellDataContainerIterator    celldata = m_Locations->GetCellData()->Begin();

  int j=0;
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
  }
}

/*
 * update the displacements using d_{new} = d_{old} + timestep*d' 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::Advance()
{
  typename TInputMesh::PointType s, d, ds; 
  float dist=0.0;
  int i;

  InputPointsContainerPointer  myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator derives = myDerives->Begin();
  InputPointsContainerPointer  myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator displacements = myDisplacements->Begin();
  InputPointsContainerPointer  myPoints = m_Locations->GetPoints();
  InputPointsContainerIterator points = myPoints->Begin();

  i=0;
  while( i < m_NumberOfNodes-2 ) {
    ds = derives.Value();
    s = points.Value();
    d = displacements.Value();
    s[0] += m_TimeStep*ds[0]; 
    s[1] += m_TimeStep*ds[1]; 
    s[2] += m_TimeStep*ds[2]; 
    if ( m_GradientBegin ) {
      d[0] += m_TimeStep*ds[0]; 
      d[1] += m_TimeStep*ds[1]; 
      d[2] += m_TimeStep*ds[2]; 
    } else {
      d[0] = 0;
      d[1] = 0;
      d[2] = 0;
    }

    /** do not update the displacements if the nodes is moving out of the image region. */
    if ( (s[0] > 0) && (s[1] > 0) && 
      (s[0] < m_ImageWidth) && (s[1] < m_ImageHeight) ) {
      points.Value() = s;
      displacements.Value() = d;
    }

    if ( m_GradientBegin ) {
      dist += sqrt(ds[0]*ds[0]+ds[1]*ds[1])*m_TimeStep;
      m_DistanceToBoundary = dist/((float)(m_NumberOfNodes-2));
    }

    i++;
    ++derives; 
    ++points;
    ++displacements;
  } 

  s[0] = 0;

}

/*
 * copy the content of m_Location into output 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::ComputeOutput() 
{
  int i;
  typename TriCell::CellAutoPointer insertCell;
  unsigned long tripoints[3];
  const unsigned long *tp;
  double x;

  m_Output = this->GetOutput();

  OutputPointsContainerPointer   myPoints = m_Output->GetPoints();
  myPoints->Reserve(m_NumberOfNodes);
  OutputPointsContainerIterator  points = myPoints->Begin();

  InputPointsContainerPointer    myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator   locations = myLocations->Begin();

  InputCellsContainerPointer     myCells = this->GetInput(0)->GetCells();
  InputCellsContainerIterator    cells = myCells->Begin(); 
  
  InputCellDataContainerPointer  myCellData = this->GetInput(0)->GetCellData();
  InputCellDataContainerIterator celldata = myCellData->Begin(); 

  i = 0;
  for (; i<m_NumberOfNodes; i++) {
    points.Value() = locations.Value();
    ++locations;
    ++points;
  } 

  for (int i=0; i<m_NumberOfCells; i++) {
    tp = cells.Value()->GetPointIds();
    tripoints[0] = tp[0];
    tripoints[1] = tp[1];
    tripoints[2] = tp[2];
    insertCell.TakeOwnership( new TriCell );
    insertCell->SetPointIds(tripoints);
    m_Output->SetCell(i, insertCell );
    x = celldata.Value();
    m_Output->SetCellData(i, (PixelType)x);
    ++cells;
    ++celldata;
  }

}

/*
 * copy the content of m_Location into output 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::GenerateData() 
{
  this->Initialize();
  this->SetStiffnessMatrix();
//  while ( m_DistanceToBoundary > m_DistanceToStop ) {
  while (m_Step < 300) {
  this->ComputeNormals();
  if ( m_GradientBegin ) this->GradientFit();
  else this->ComputeForce();
  this->ComputeDt();
  this->Advance();
//  this->ACDSearch();
//  this->NodesRearrange();
  m_Step++;
  }

  this->ComputeOutput();
 }

/*
 * when almost all the nodes is at the estimated boundary, use
 * gapsearch to fit the model to more complicated shapes
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::GapSearch() 
{
  InputPointsContainerPointer   Points = m_Locations->GetPoints();
  InputPointsContainerIterator    points = Points->Begin();

  InputPointDataContainerPointer  myPointData = m_Locations->GetPointData();
  InputPointDataContainerIterator pointstatus = myPointData->Begin();

  InputCellsContainerPointer    myCells = m_Locations->GetCells();
  InputCellsContainerIterator   cells = myCells->Begin(); 

  int i, q, res = 1;
  IPixelType x, y, z;
  IPixelType* y_PixelType;
  IPixelType* z_PixelType;
  y_PixelType = &y;
  z_PixelType = &z;
  float gap, dis[3]={0, 0, 0}, st, *st_PixelType;
  st_PixelType = &st;
  const unsigned long *tp;

  i = 0;
  while( i != m_NumberOfNodes - 2 )
  { 
  x = points.Value();
  ++points;

    tp = cells.Value()->GetPointIds();
  ++cells;
  ++cells;

  if (tp[0] == tp[1] - 1) y = points.Value();
  else {
    m_Locations->GetPoint (tp[1], y_PixelType);
    res++;
  }
  
  dis[0] = x[0] - y[0];
  dis[1] = x[1] - y[1];
  dis[2] = x[2] - y[2];
  gap = sqrt(dis[0]*dis[0]+dis[1]*dis[1]+dis[2]*dis[2]);
  
  m_Locations->GetPointData(q, st_PixelType);
  if (gap > 3) {
    if ( pointstatus.Value() == 1.0) {
    ++pointstatus;
    if ( pointstatus.Value() == 1.0) {
      z[0] = 0.5*(x[0]+y[0]);
      z[1] = 0.5*(x[1]+y[1]);
      z[2] = 0.5*(x[2]+y[2]);
      this->NodeAddition(i, res, z);
    }
    } else ++pointstatus;
  } else ++pointstatus;

  i++;
  }

  this->Reset();        
}

/*
 * add new nodes into the model 
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::NodeAddition(int node, int res, IPixelType z) 
{
  m_NumNewNodes++;

  if (m_NumNewNodes > m_NewNodeLimit) {
  realloc( m_NewNodes, m_NewNodeLimit*sizeof(float*)*2 );
  for (int i = m_NewNodeLimit; i < 2*m_NewNodeLimit; i++) {
    m_NewNodes[i] = (float*) malloc(sizeof(float)*5);
  }
  m_NewNodeLimit = m_NewNodeLimit*2;
  }

  m_NewNodes[m_NumNewNodes-1][0] = z[0];
  m_NewNodes[m_NumNewNodes-1][1] = z[1];
  m_NewNodes[m_NumNewNodes-1][2] = z[2];
  m_NewNodes[m_NumNewNodes-1][3] = (float) node;
  m_NewNodes[m_NumNewNodes-1][4] = (float) res;
}

/*
 * fit the model to the gradient information
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::GradientFit() 
{
  typedef typename GradientIndexType::IndexValueType   IndexValueType;

  GradientIndexType coord, coord2, tmp_co_1, tmp_co_2, tmp_co_3;
  IPixelType v1, v2;
  PixelType mag, num_for;

  typename TInputMesh::PointType vec_nor, vec_loc, vec_for, tmp_vec_1, tmp_vec_2, tmp_vec_3;

  InputPointsContainerPointer       myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator      locations = myLocations->Begin();

  InputPointsContainerPointer       myForces = m_Forces->GetPoints();
  InputPointsContainerIterator      forces = myForces->Begin();

  InputPointDataContainerPointer    myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator   forcedata = myForceData->Begin();

  InputPointsContainerPointer       myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator      normals = myNormals->Begin();

  /* New gradient fit method testing. */
  while( forces != myForces->End() ) {
    vec_loc = locations.Value();
    vec_nor = normals.Value();
    num_for = 1/forcedata.Value();

    coord[0] = static_cast<IndexValueType>(vec_loc[0]);
    coord[1] = static_cast<IndexValueType>(vec_loc[1]);

    coord2[0] = static_cast<IndexValueType>( (ceil) (vec_loc[0]) );
    coord2[1] = static_cast<IndexValueType>( (ceil) (vec_loc[1]) );

    tmp_co_1[0] = coord2[0];
    tmp_co_1[1] = coord[1];
    tmp_co_2[0] = coord[0];
    tmp_co_2[1] = coord2[1];


    if ( (coord[0] >= 0) && (coord[1] >= 0) && 
        (coord2[0] < m_ImageWidth) && (coord2[1] < m_ImageHeight) ) {      
      vec_for[0] = m_Gradient->GetPixel(coord)[0];
      vec_for[1] = m_Gradient->GetPixel(coord)[1];

      tmp_vec_1[0] = m_Gradient->GetPixel(tmp_co_1)[0] - m_Gradient->GetPixel(coord)[0];
      tmp_vec_1[1] = m_Gradient->GetPixel(tmp_co_1)[1] - m_Gradient->GetPixel(coord)[1];
      tmp_vec_2[0] = m_Gradient->GetPixel(tmp_co_2)[0] - m_Gradient->GetPixel(coord)[0];
      tmp_vec_2[1] = m_Gradient->GetPixel(tmp_co_2)[1] - m_Gradient->GetPixel(coord)[1];

      vec_for[0] = vec_for[0] + (vec_loc[0]-coord[0])*tmp_vec_1[0] + (vec_loc[1]-coord[1])*tmp_vec_2[0];
      vec_for[1] = vec_for[1] + (vec_loc[1]-coord[1])*tmp_vec_2[1] + (vec_loc[0]-coord[0])*tmp_vec_1[1];

    } else {
      vec_for[0] = 0;
      vec_for[1] = 0;
    }
//  for test
//  vec_for[0] = 0;

    mag = vec_for[0]*vec_nor[0] + vec_for[1]*vec_nor[1];
    vec_for[0] = mag*vec_nor[0];
    vec_for[1] = mag*vec_nor[1];

    forces.Value() = vec_for;

    ++forces;
    ++forcedata;
    ++locations;
    ++normals;
  }

  mag = 0.0;

}

/*
 * fit the model to the gradient information
 */
template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::ComputeNormals() 
{
  const unsigned long *tp;
  IPixelType v1, v2, v3, v4;
  IPixelType* v1_PixelType;
  IPixelType* v2_PixelType;
  IPixelType* v3_PixelType;
  v1_PixelType = &v1;
  v2_PixelType = &v2;
  v3_PixelType = &v3;
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

    m_Locations->GetPoint (tp[0], v1_PixelType);
    m_Locations->GetPoint (tp[1], v2_PixelType);
    m_Locations->GetPoint (tp[2], v3_PixelType);

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
    m_Normals->GetPoint (tp[0], v1_PixelType);
    m_Normals->GetPoint (tp[1], v2_PixelType);
    m_Normals->GetPoint (tp[2], v3_PixelType);

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
BalloonForceFilter<TInputMesh, TOutputMesh>
::NodesRearrange()
{
  int i, j, k;
  float dis, l1, l2, *d_PixelTyper, d;
  float length;
  IPixelType v1, v2, v3, v4;
  IPixelType* v1_PixelType;
  IPixelType* v2_PixelType;
  IPixelType* v3_PixelType;
  d_PixelTyper = &d;
  v1_PixelType = &v1;
  v2_PixelType = &v2;
  v3_PixelType = &v3;

  typename TInputMesh::PointType s, s1, d1, d2;

  InputPointsContainerPointer   myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator  locations = myLocations->Begin();

  InputPointsContainerPointer   myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator  displacements = myDisplacements->Begin();

  InputPointsContainerPointer   myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator  normals = myNormals->Begin();

  InputPointsContainerPointer   myForces = m_Forces->GetPoints();
  InputPointsContainerIterator  forces = myForces->Begin();
  InputPointsContainerIterator  forcescopy;



  v1 = locations.Value();
  s = locations.Value();
  ++locations;
  s1 = locations.Value();
  i = 1;
  forcescopy = forces;
  forces++;
  
  while ( (i+1)%m_Resolution != 0 ) {
    v2 = locations.Value();
    ++locations;
    v3 = locations.Value();
    d1[0] = v1[0] - v2[0];
    d2[0] = v3[0] - v2[0];
    d1[1] = v1[1] - v2[1];
    d2[1] = v3[1] - v2[1];
//  d1[2] = v1[2] - v2[2];
//  d2[2] = v3[2] - v2[2];
    v1[0] = v2[0];
    v1[1] = v2[1];
//  v1[2] = v2[2];
    dis = d1[0]*d2[0]+d1[1]*d2[1]/*+d1[2]*d2[2]*/;
    if ( dis > 0 ) {
      l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]/*+d1[2]*d1[2]*/);
      l2 = sqrt(d2[0]*d2[0]+d2[1]*d2[1]/*+d2[2]*d2[2]*/);
      dis = dis/sqrt(l1*l2);
      d1[0] = d1[0]/l1;
      d1[1] = d1[1]/l1;
//    d1[2] = d1[2]/l1;
      d2[0] = d2[0]/l2;
      d2[1] = d2[1]/l2;
//    d2[2] = d2[2]/l2;
      d1[0] = (d1[0]+d2[0]);
      d1[1] = (d1[1]+d2[1]);
//    d1[2] = (d1[2]+d2[2]);
      l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]/*+d1[2]*d1[2]*/);
      d1[0] = d1[0]/l1;
      d1[1] = d1[1]/l1;
//    d1[2] = d1[2]/l1;
      v2[0] = v2[0] + dis * d1[0];
      v2[1] = v2[1] + dis * d1[1];
//    v2[2] = v2[2]/* + dis * d1[2]*/;
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
//  d1[2] = v1[2] - v2[2];
//  d2[2] = v3[2] - v2[2];
    v1[0] = v2[0];
    v1[1] = v2[1];
//  v1[2] = v2[2];
    dis = d1[0]*d2[0]+d1[1]*d2[1]/*+d1[2]*d2[2]*/;
    if ( dis > 0 ) {
      l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]/*+d1[2]*d1[2]*/);
      l2 = sqrt(d2[0]*d2[0]+d2[1]*d2[1]/*+d2[2]*d2[2]*/);
      dis = dis/sqrt(l1*l2);
      d1[0] = d1[0]/l1;
      d1[1] = d1[1]/l1;
//  d1[2] = d1[2]/l1;
      d2[0] = d2[0]/l2;
      d2[1] = d2[1]/l2;
//  d2[2] = d2[2]/l2;
      d1[0] = (d1[0]+d2[0]);
      d1[1] = (d1[1]+d2[1]);
//  d1[2] = (d1[2]+d2[2]);
      l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]/*+d1[2]*d1[2]*/);
      d1[0] = d1[0]/l1;
      d1[1] = d1[1]/l1;
//  d1[2] = d1[2]/l1;
      v2[0] = v2[0] + dis * d1[0];
      v2[1] = v2[1] + dis * d1[1];
//  v2[2] = v2[2]/* + dis * d1[2]*/;
    }
  
    forces.Value() = v2;
    ++forces;

// fro the first node in the slice
    i = 0;
    v2[0] = s[0];
    v2[1] = s[1];
//  v2[2] = s[2];
    v3[0] = s1[0];
    v3[1] = s1[1];
//  v3[2] = s1[2];
    d1[0] = v1[0] - v2[0];
    d2[0] = v3[0] - v2[0];
    d1[1] = v1[1] - v2[1];
    d2[1] = v3[1] - v2[1];
//  d1[2] = v1[2] - v2[2];
//  d2[2] = v3[2] - v2[2];
//  v1[0] = v2[0];
//  v1[1] = v2[1];
//  v1[2] = v2[2];
    dis = d1[0]*d2[0]+d1[1]*d2[1];
    if ( dis > 0 ) {
      l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]);
      l2 = sqrt(d2[0]*d2[0]+d2[1]*d2[1]);
      dis = dis/sqrt(l1*l2);
      d1[0] = d1[0]/l1;
      d1[1] = d1[1]/l1;
//  d1[2] = d1[2]/l1;
      d2[0] = d2[0]/l2;
//  d2[1] = d2[1]/l2;
//  d2[2] = d2[2]/l2;
      d1[0] = (d1[0]+d2[0]);
      d1[1] = (d1[1]+d2[1]);
//  d1[2] = (d1[2]+d2[2]);
      l1 = sqrt(d1[0]*d1[0]+d1[1]*d1[1]);
      d1[0] = d1[0]/l1;
      d1[1] = d1[1]/l1;
//  d1[2] = d1[2]/l1;
      v2[0] = v2[0] + dis * d1[0];
      v2[1] = v2[1] + dis * d1[1];
//  v2[2] = v2[2]/* + dis * d1[2]*/;
    }

    forcescopy.Value() = v2;

  forces = myForces->Begin();

  dis = 0;
  i = 0;
  s = forces.Value();
//  forcescopy = forces;

  while ( i < m_Resolution - 1 ) {
    v1 = forces.Value();
    ++forces;
    v2 = forces.Value();
//    m_Displacements->GetPointData(i+j*m_Resolution, d_PixelTyper);
    dis += sqrt((v1[0]-v2[0])*(v1[0]-v2[0])+(v1[1]-v2[1])*(v1[1]-v2[1]));
    i++;
  }
//  m_Displacements->GetPointData(i+j*m_Resolution, d_PixelTyper);
  dis += sqrt((s[0]-v2[0])*(s[0]-v2[0])+(s[1]-v2[1])*(s[1]-v2[1]));
  length = dis/m_Resolution;
  ++forces;

  forces = myForces->Begin();

  k = 1;
  i = 0;
  l1 = 0;
  v1 = forces.Value();
  normals.Value() = v1;
  ++normals;
  v3 = forces.Value();
  while ( i < m_Resolution - 1 ) {
    v1 = forces.Value();
    ++forces;
    v2 = forces.Value();
    dis = sqrt((v1[0]-v2[0])*(v1[0]-v2[0])+(v1[1]-v2[1])*(v1[1]-v2[1]));
    l2 = -1*l1;
    l1 += dis;
//    m_Displacements->GetPointData(i+j*m_Resolution, d_PixelTyper);
    while ( l1 > length ) {
      if (k==m_Resolution) break;
      s[0] = v1[0] + (length+l2)*(v2[0] - v1[0])/dis;
      s[1] = v1[1] + (length+l2)*(v2[1] - v1[1])/dis;
//      s[2] = v1[2];
      normals.Value() = s;
      ++normals;
      k++;
      l2 += length;
      l1 -= length;
    }
    i++;
    if (k==m_Resolution) break;
  }

  if (k==m_Resolution) {
    while (i < m_Resolution) {
    i++;
    ++forces;
    }
  }

  v1 = forces.Value();
  ++forces;
  dis = sqrt((v1[0]-v3[0])*(v1[0]-v3[0])+(v1[1]-v3[1])*(v1[1]-v3[1]));
  l2 = -1*l1;
  l1 += dis;
//  m_Displacements->GetPointData(i+j*m_Resolution, d_PixelTyper);
  while ( l1 > length ) {
    if (k==m_Resolution) break;
    s[0] = v1[0] + (length+l2)*(v3[0] - v1[0])/dis;
    s[1] = v1[1] + (length+l2)*(v3[1] - v1[1])/dis;
//    s[2] = v1[2];
    normals.Value() = s;
    ++normals;
    k++;
    l2 += length/d;
    l1 -= length/d;
  }

  locations = myLocations->Begin();
  normals = myNormals->Begin();
  displacements = myDisplacements->Begin();
  forces = myForces->Begin();

  i = 0;
  while ( i < m_NumberOfNodes - 2 ) {
  v1 = normals.Value();
//  v1 = forces.Value();
  v2 = locations.Value();
  v3 = displacements.Value();
  v3[0] += v1[0] - v2[0];
  v3[1] += v1[1] - v2[1];
//  v3[2] += v1[2] - v2[2];
  locations.Value() = v1;
  if ( m_GradientBegin ) displacements.Value() = v3;
  ++normals;
//  ++forces;
  ++locations;
  ++displacements;
  i++;
  }

  locations = myLocations->Begin();
  j = 0;
  for (; j < 1; j++) {
  dis = 0;
  i = 0;
  s = locations.Value();
  while ( i < m_Resolution - 1 ) {
      v1 = locations.Value();
    ++locations;
      v2 = locations.Value();
    dis = sqrt((v1[0]-v2[0])*(v1[0]-v2[0])+(v1[1]-v2[1])*(v1[1]-v2[1]));
    i++;
  }
  dis = sqrt((s[0]-v2[0])*(s[0]-v2[0])+(s[1]-v2[1])*(s[1]-v2[1]));
  ++locations;
  }

}

template <typename TInputMesh, typename TOutputMesh>
void
BalloonForceFilter<TInputMesh, TOutputMesh>
::ACDSearch() 
{
  int i, j, l, m, n, PixelType1, PixelType2;
  float s;
  IPixelType v, v1, v2, v3;
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

  for (j = 0; j < 1; j++) {
  i = 0;
  locationscopy = locations;
  dpcopy = displacements;

  for (l=0; l<m_ImageHeight/2; l++) 
    for (m=0; m<m_ImageWidth/2; m++)
    m_ACD[l][m] = -1;

  for (; i < m_Resolution; i++) {
    v = locations.Value();
    ++locations;
    ++displacements;
    l = (int)(v[0]/2);
    m = (int)(v[1]/2);
    if (m_ACD[m][l] == -1) m_ACD[m][l] = i;
    else {
    if (((i - m_ACD[m][l]) > m_Resolution/10) && 
      ((m_Resolution-i+m_ACD[m][l])>m_Resolution/10)) {
      locationscopy1 = locationscopy;
      n = 0;
      v1[0] = 0;
      v1[1] = 0;
//      v1[2] = 0;
      if ( (i - m_ACD[m][l]) < 0.5*m_Resolution ) {
      if (m_ACD[m][l] == 0) PixelType1 = m_Resolution - 1;
      else PixelType1 = m_ACD[m][l] - 1;
      if (i == m_Resolution - 1) PixelType2 = 0;
      else PixelType2 = i + 1;
      while (n<m_Resolution) {
        v = locationscopy1.Value();
        if ((n>m_ACD[m][l]) && (n<i)) {
        v1[0] += v[0];
        v1[1] += v[1];
        } else {
        if ( n == PixelType1) v2 = locationscopy1.Value();
        if ( n == PixelType2 ) v3 = locationscopy1.Value();
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
        while (n<m_Resolution) {
        if ((n>m_ACD[m][l]) && (n<i)) {
          v1 = locationscopy1.Value();
          locationscopy1.Value() = v;
          v2 = dpcopy1.Value();
          v2[0] += v[0] - v1[0];
          v2[1] += v[1] - v1[1];
//          v2[2] += v[2] - v1[2];
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
        while (n<m_Resolution) {
        PixelType1 = m_ACD[m][l] + 1;
        PixelType2 = i - 1;
        v = locationscopy1.Value();
        if ((n<m_ACD[m][l]) && (n>i)) {
        v1[0] += v[0];
        v1[1] += v[1];
        } else{
        if ( n == PixelType1 ) v2 = locationscopy1.Value();
        if ( n == PixelType2 ) v3 = locationscopy1.Value();
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
        while (n<m_Resolution) {
          if ( n == i ) {
          v = locationscopy1.Value();
        }
          ++locationscopy1;
          n++;
        }
        locationscopy1 = locationscopy;
        n = 0;
        while (n<m_Resolution) {
        if ((n<m_ACD[m][l]) && (n>i)) {
          v1 = locationscopy1.Value();
          locationscopy1.Value() = v;
          v2 = dpcopy1.Value();
          v2[0] += v[0] - v1[0];
          v2[1] += v[1] - v1[1];
//          v2[2] += v[2] - v1[2];
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
