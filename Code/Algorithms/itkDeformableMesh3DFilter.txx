/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableMesh3DFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDeformableMesh3DFilter_txx
#define _itkDeformableMesh3DFilter_txx

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkDeformableMesh3DFilter.h"

namespace itk
{
/* Constructor. */
template <typename TInputMesh, typename TOutputMesh>
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::DeformableMesh3DFilter()
{
  m_Step = 0;
  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
}


/* PrintSelf. */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Deformable Mesh 3D Filter" << std::endl;

}/* End PrintSelf. */

/* Set default value of parameters and initialize local data container 
 *  such as forces, displacements and displacement derivatives. */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::Initialize()
{
  m_NumberOfNodes = this->GetInput(0)->GetNumberOfPoints();
  m_NumberOfCells = this->GetInput(0)->GetNumberOfCells();

  m_Forces          = InputMeshType::New();
  m_Displacements   = InputMeshType::New();
  m_Derives         = InputMeshType::New();
  m_Normals         = InputMeshType::New();
  m_Locations       = InputMeshType::New();

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

  ImageSizeType ImageSize = m_Gradient->GetBufferedRegion().GetSize();

  //---------------------------------------------------------------------
  //Get the image width/height and depth
  //---------------------------------------------------------------------   
  m_ImageWidth  = ImageSize[0];
  m_ImageHeight = ImageSize[1];
  m_ImageDepth  = ImageSize[2];

  m_ModelXUpLimit = 0;
  m_ModelXDownLimit = m_ImageWidth;
  m_ModelYUpLimit = 0;
  m_ModelYDownLimit = m_ImageHeight;
  m_ModelZUpLimit = 0;
  m_ModelZDownLimit = m_ImageDepth;

  InputPointType d;
  d[0] = 0.0;
  d[1] = 0.0;
  d[2] = 0.0; 

  int j = 0;
  while( points != myPoints->End() ) {
    for (int i=0; i<3; i++ ) locations.Value()[i] = m_Scale[i] * points.Value()[i];
    ++points;
    ++locations;
    forces.Value() = d;
    ++forces;
    normals.Value() = d;
    ++normals;
    derives.Value() = d;
    ++derives;
    displacements.Value() = d;
    ++displacements;
    m_Forces->SetPointData(j, 0.0);
    j++;
  }

  const unsigned long *tp;
  PixelType x = 0.0;
  PixelType* x_pt;
  x_pt = &x;
  std::cout << "myCells size = " << myCells->Size() << std::endl;
  while( cells != myCells->End() ) {
    typename InputMeshType::CellType * cellPtr = cells.Value();
    tp = cellPtr->GetPointIds();
    for ( int i=0; i<3; i++ ) {
      m_Forces->GetPointData((int)(tp[i]), x_pt);
      x = x + 1.0;
      m_Forces->SetPointData((int)(tp[i]), x);
    }
    ++cells;
  }

  this->SetDefaultStiffnessMatrix();
} 

/* Set the stiffness matrix. */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::SetDefaultStiffnessMatrix () 
{ 
  double us = 0.5; 
  double vs = 0.5; 
  double a = us*us, b = vs*vs; 
  double area = us*vs/2, k00, k01, k02, k11, k12, k22; 
  
  k00 = area * (m_Stiffness[1]/b + m_Stiffness[0]); 
  k01 = area * (-m_Stiffness[1]/b + m_Stiffness[0]); 
  k02 = area * m_Stiffness[0]; 
  k11 = area * (m_Stiffness[1]/a + m_Stiffness[1]/b + m_Stiffness[0]); 
  k12 = area * (-m_Stiffness[1]/a + m_Stiffness[0]); 
  k22 = area * (m_Stiffness[1]/a + m_Stiffness[0]); 
 
  Stiffness[0][0][0] = k00; 
  Stiffness[0][0][1] = k01; 
  Stiffness[0][0][2] = k02; 
  Stiffness[0][0][3] = 0.0; 
  Stiffness[0][1][0] = k01; 
  Stiffness[0][1][1] = k11; 
  Stiffness[0][1][2] = k12; 
  Stiffness[0][1][3] = 0.0; 
  Stiffness[0][2][0] = k02; 
  Stiffness[0][2][1] = k12; 
  Stiffness[0][2][2] = k22; 
  Stiffness[0][2][3] = 0.0; 
  Stiffness[0][3][0] = 0.0; 
  Stiffness[0][3][1] = 0.0; 
  Stiffness[0][3][2] = 0.0; 
  Stiffness[0][3][3] = 1.0; 

} 

/** Set the stiffness matrix. */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::SetStiffnessMatrix ( vnl_matrix_fixed<double, 4, 4> *stiff, int i ) 
{ 
  InputCellDataContainerPointer    myCellData = m_Locations->GetCellData();
  InputCellDataContainerIterator   celldata = myCellData->Begin();

  double x;
 
  Stiffness[i][0][0] = stiff[0][0]; 
  Stiffness[i][0][1] = stiff[0][1]; 
  Stiffness[i][0][2] = stiff[0][2]; 
  Stiffness[i][0][3] = stiff[0][3]; 
  Stiffness[i][1][0] = stiff[1][0]; 
  Stiffness[i][1][1] = stiff[1][1]; 
  Stiffness[i][1][2] = stiff[1][2]; 
  Stiffness[i][1][3] = stiff[1][3]; 
  Stiffness[i][2][0] = stiff[2][0]; 
  Stiffness[i][2][1] = stiff[2][1]; 
  Stiffness[i][2][2] = stiff[2][2]; 
  Stiffness[i][2][3] = stiff[2][3]; 
  Stiffness[i][3][0] = stiff[3][0]; 
  Stiffness[i][3][1] = stiff[3][1]; 
  Stiffness[i][3][2] = stiff[3][2]; 
  Stiffness[i][3][3] = stiff[3][3]; 
} 

/** Set the stiffness matrix. */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::SetMeshStiffness () 
{ 
  InputCellDataContainerPointer    myCellData = this->GetInput(0)->GetCellData();
  InputCellDataContainerIterator   celldata = myCellData->Begin();

  K = (vnl_matrix_fixed<double,4,4>**) malloc(sizeof(vnl_matrix_fixed<double,4,4>*)*m_NumberOfCells);
  double x;

  int j = 0;
  while (celldata != myCellData->End()){
    x = celldata.Value();
    K[j] = Stiffness+((int) x);
    ++celldata; 
    j++;
  }
} 


/* Compute the derivatives using d'- Kd = f. */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeDt()
{
  int i; 
  const unsigned long *tp;

  InputCellsContainerPointer  myCells = this->GetInput(0)->GetCells();
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

/** Update the displacements using d_{new} = d_{old} + timestep*d'. */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::Advance()
{
  typename TInputMesh::PointType s, d, ds; 

  InputPointsContainerPointer  myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator derives = myDerives->Begin();
  InputPointsContainerPointer  myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator displacements = myDisplacements->Begin();
  InputPointsContainerPointer  myPoints = m_Locations->GetPoints();
  InputPointsContainerIterator points = myPoints->Begin();

  while( derives != myDerives->End() ) {
    ds = derives.Value();
    s = points.Value();
    d = displacements.Value();
    s[0] += m_TimeStep*ds[0]; 
    s[1] += m_TimeStep*ds[1]; 
    s[2] += m_TimeStep*ds[2]; 
    d[0] += m_TimeStep*ds[0]; 
    d[1] += m_TimeStep*ds[1]; 
    d[2] += m_TimeStep*ds[2]; 

    /** do not update the displacements if the nodes is moving out of the image region. */
    if ( (s[0] > 0) && (s[1] > 0) && (s[2] > 0) && 
        (s[2] < m_ImageDepth) && (s[0] < m_ImageWidth) && (s[1] < m_ImageHeight) ) {
      points.Value() = s;
      displacements.Value() = d;
    }

    ++derives; 
    ++points;
    ++displacements;
  } 

  s[0] = 0;
}

/* Copy the content of m_Location into the Output. */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeOutput() 
{
 
  int i;
  typename TriCell::CellAutoPointer insertCell;
  unsigned long tripoints[3];
  const unsigned long *tp;
  double x;

  OutputMeshType * output = this->GetOutput();

  output->GetCells()->Reserve( m_NumberOfCells );
  output->SetCellsAllocationMethod( OutputMeshType::CellsAllocatedDynamicallyCellByCell );

  OutputPointsContainerPointer   myPoints = output->GetPoints();
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
    output->SetCell(i, insertCell );
    x = celldata.Value();
    output->SetCellData(i, (PixelType)x);
    ++cells;
    ++celldata;
  }

  output->DebugOn();
}

/* Generate Data */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::GenerateData() 
{
  this->Initialize();
  this->SetMeshStiffness();
  
  while (m_Step < m_StepThreshold) {
    this->ComputeNormals();
    this->GradientFit();
    this->ComputeDt();
    this->Advance();
    m_Step++;
  }

  this->ComputeOutput();
}

/** Fit the model using the gradient information. */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableMesh3DFilter<TInputMesh, TOutputMesh>
::GradientFit() 
{
  typedef typename ImageIndexType::IndexValueType   IndexValueType;

  ImageIndexType coord, coord2, tmp_co_1, tmp_co_2, tmp_co_3;
  InputPointType v1, v2;
  PixelType mag;

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

    coord[0] = static_cast<IndexValueType>(vec_loc[0]);
    coord[1] = static_cast<IndexValueType>(vec_loc[1]);
    coord[2] = static_cast<IndexValueType>(vec_loc[2]);

    coord2[0] = static_cast<IndexValueType>( (ceil) (vec_loc[0]) );
    coord2[1] = static_cast<IndexValueType>( (ceil) (vec_loc[1]) );
    coord2[2] = static_cast<IndexValueType>( (ceil) (vec_loc[2]) );

    tmp_co_1[0] = coord2[0];
    tmp_co_1[1] = coord[1];
    tmp_co_1[2] = coord[2];

    tmp_co_2[0] = coord[0];
    tmp_co_2[1] = coord2[1];
    tmp_co_2[2] = coord[2];

    tmp_co_3[0] = coord[0];
    tmp_co_3[1] = coord[1];
    tmp_co_3[2] = coord2[2];

    if ( (coord[0] >= 0) && (coord[1] >= 0) && (coord[2] >= 0) && 
        (coord2[0] < m_ImageWidth) && (coord2[1] < m_ImageHeight) && (coord2[2] < m_ImageDepth) ) {      
      vec_for[0] = m_Gradient->GetPixel(coord)[0];
      vec_for[1] = m_Gradient->GetPixel(coord)[1];
      vec_for[2] = m_Gradient->GetPixel(coord)[2];

      tmp_vec_1[0] = m_Gradient->GetPixel(tmp_co_1)[0] - m_Gradient->GetPixel(coord)[0];
      tmp_vec_1[1] = m_Gradient->GetPixel(tmp_co_1)[1] - m_Gradient->GetPixel(coord)[1];
      tmp_vec_1[2] = m_Gradient->GetPixel(tmp_co_1)[2] - m_Gradient->GetPixel(coord)[2];
      tmp_vec_2[0] = m_Gradient->GetPixel(tmp_co_2)[0] - m_Gradient->GetPixel(coord)[0];
      tmp_vec_2[1] = m_Gradient->GetPixel(tmp_co_2)[1] - m_Gradient->GetPixel(coord)[1];
      tmp_vec_2[2] = m_Gradient->GetPixel(tmp_co_2)[2] - m_Gradient->GetPixel(coord)[2];
      tmp_vec_3[0] = m_Gradient->GetPixel(tmp_co_3)[0] - m_Gradient->GetPixel(coord)[0];
      tmp_vec_3[1] = m_Gradient->GetPixel(tmp_co_3)[1] - m_Gradient->GetPixel(coord)[1];
      tmp_vec_3[2] = m_Gradient->GetPixel(tmp_co_3)[2] - m_Gradient->GetPixel(coord)[2];

      vec_for[0] = vec_for[0] + (vec_loc[0]-coord[0])*tmp_vec_1[0];
      vec_for[1] = vec_for[1] + (vec_loc[1]-coord[1])*tmp_vec_2[1];
      vec_for[2] = vec_for[2] + (vec_loc[2]-coord[2])*tmp_vec_3[2];
    } else {
      vec_for[0] = 0;
      vec_for[1] = 0;
      vec_for[2] = 0;
    }

    mag = vec_for[0]*vec_nor[0] + vec_for[1]*vec_nor[1]+ vec_for[2]*vec_nor[2];
    vec_for[0] = mag*vec_nor[0];
    vec_for[1] = mag*vec_nor[1];
    vec_for[2] = mag*vec_nor[2];

    mag = sqrt (vec_for[0]*vec_for[0] + vec_for[1]*vec_for[1]+ vec_for[2]*vec_for[2]);
    if (mag > 0.5) 
      for (int i=0; i<3; i++) vec_for[i] = (0.5 * vec_for[i])/mag;
    forces.Value() = vec_for;

    ++forces;
    ++forcedata;
    ++locations;
    ++normals;
  }

  mag = 0.0;
}

/* Compute normals. */
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

  InputCellsContainerPointer    myCells = this->GetInput(0)->GetCells();
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

    absvec = sqrt ((double) ((v1[0]*v1[0]) + (v1[1]*v1[1]) + 
        (v1[2]*v1[2])));
    v1[0] = v1[0]/absvec;
    v1[1] = v1[1]/absvec;
    v1[2] = v1[2]/absvec;

    normals.Value() = v1;
    ++normals;
  }
}

} /* end namespace itk. */

#endif
