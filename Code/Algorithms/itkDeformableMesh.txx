/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableMesh.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkDeformableMesh_txx
#define _itkDeformableMesh_txx

#include <iostream>
#include <string>
#include <math.h>
#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkDeformableMesh.h"
#include "vnl/vnl_math.h"

namespace itk
{
template <typename TPixelType/*, typename TMeshTraits*/>
DeformableMesh<TPixelType/*, TMeshTraits*/>
::DeformableMesh()
{
}

/**
 * Set the resoluion of the model.
 */
template <typename TPixelType/*, typename TMeshTraits*/>
void
DeformableMesh<TPixelType/*, TMeshTraits*/>
::SetResolution(int a, int b)
{
  m_Resolution[0] = a;
  m_Resolution[1] = b;
}

/**
 * Set the scale of the model.
 */
template <typename TPixelType/*, typename TMeshTraits*/>
void
DeformableMesh<TPixelType/*, TMeshTraits*/>
::SetScale(float a, float b, float c)
{
  m_Scale[0] = a;
  m_Scale[1] = b;
  m_Scale[2] = c;
}

/**
 * Set the center point of the model.
 */
template <typename TPixelType/*, typename TMeshTraits*/>
void
DeformableMesh<TPixelType/*, TMeshTraits*/>
::SetCenter(int a, int b, int c)
{
  m_Center[0] = a;
  m_Center[1] = b;
  m_Center[2] = c;
}

/**
 * Set the default value to the scale and resolution.
 */
template <typename TPixelType/*, typename TMeshTraits*/>
void
DeformableMesh<TPixelType/*, TMeshTraits*/>
::SetDefault()
{
  m_Scale[0] = 1.0;
  m_Scale[1] = 1.0;
  m_Scale[2] = 1.0;
  m_Resolution[0] = 8;
  m_Resolution[1] = 18;
}

/**
 * Calculate and insert the nodes into the pointscontainer.
 * and store the connectness of the model into the cellscontainer.
 * store the type of cells in the celldatacontainer, these will be used
 * when calculate the local stiffness matrix of the model
 */
template <typename TPixelType/*, typename TMeshTraits*/>
void
DeformableMesh<TPixelType/*, TMeshTraits*/>
::Allocate()
{
  unsigned long i, j, jn, p, numpts, numcells;
  float ustep, vstep, ubeg, vbeg, u, v; 
  int signu, signv; 

// calculate the number os cells and points
  numpts = m_Resolution[0]*m_Resolution[1] + 2; 
  numcells = 2 * (m_Resolution[0]-1) *m_Resolution[1] + 2*m_Resolution[1]; 

// calculate the steps using resolution
  ustep = vnl_math::pi / (m_Resolution[0]+1); 
  vstep = 2.0*vnl_math::pi / m_Resolution[1]; 
  ubeg = (-vnl_math::pi/2.0) + ustep; 
  vbeg = -vnl_math::pi; 

///////////////////////////////////////////////////////////////////////////
// nodes allocation

// the temporary container of nodes' connectness
  unsigned long tripoints[3] = {0,1,2};
  
// memory allocation for nodes
  this->GetPoints()->Reserve(numpts);

  PointsContainerPointer      myPoints = GetPoints();
  typename PointsContainer::Iterator   point    = myPoints->Begin();

  PointType p1;

// calculate all regular nodes
  while(  point != myPoints->End() ) { 
	for (u=ubeg, i=0; i < m_Resolution[0]; u += ustep, i++) { 
	  for (v=vbeg, j=0; j < m_Resolution[1]; v += vstep, j++) { 
		if (cos(u) > 0) {signu = 1;} else {signu = -1;}
		if (cos(v) > 0) {signv = 1;} else {signv = -1;}

		p1[0] = m_Scale[0]*signu*(pow((float)(fabs(cos(u))), 1.0f))*signv* 
			(pow((float)(fabs(cos(v))), 1.0f)) + m_Center[0]; 

		if (sin(v) > 0) {signv = 1;} else {signv = -1;}

		p1[1] = m_Scale[1]*signu*(pow((float)(fabs(cos(u))), 1.0f))*signv* 
			(pow((float)(fabs(sin(v))), 1.0f)) + m_Center[1]; 
				  
		if (sin(u) > 0) {signu = 1;} else {signu = -1;}

		p1[2] = m_Scale[2]*signu*(pow((float)(fabs(sin(u))),1.0f)) + 
			m_Center[2];
	  
		point.Value() = p1;
		++point;
	} 
  }   

// calculate the south pole node
  p1[0] = (m_Scale[0]*(pow((float)(fabs(cos(-vnl_math::pi/2))),1.0f))* 
	  (pow((float)(fabs(cos(0.0))),1.0f)) + m_Center[0]); 
  p1[1] = (m_Scale[1]*(pow((float)(fabs(cos(-vnl_math::pi/2))),1.0f))* 
      (pow((float)(fabs(sin(0.0))),1.0f)) + m_Center[1]); 
  p1[2] = (m_Scale[2]*-1*(pow((float)(fabs(sin(-vnl_math::pi/2))),1.0f)) 
	  + m_Center[2]);
  point.Value() = p1;
  ++point;

// calculate the north pole node
  p1[0] = (m_Scale[0]*(pow((float)(fabs(cos(vnl_math::pi/2))),1.0f))* 
	  (pow(fabs(cos(0.0)),1.0)) + m_Center[0]); 
  p1[1] = (m_Scale[1]*(pow((float)(fabs(cos(vnl_math::pi/2))),1.0f))* 
	  (pow(fabs(sin(0.0)),1.0)) + m_Center[1]); 
  p1[2] = (m_Scale[2]*(pow((float)(fabs(sin(vnl_math::pi/2))),1.0f)) 
	  + m_Center[2]);
  point.Value() = p1;
  ++point;
  }

///////////////////////////////////////////////////////////////////////////
// cells allocation
  p = 0;
  TriCellPointer testCell(TriCell::New());

// store all regular cells
  for(int i=0; i < m_Resolution[0]-1 ; i++) {
	for (int j=0; j<m_Resolution[1]; j++) {
      jn = (j+1)%m_Resolution[1]; 
      tripoints[0] = i*m_Resolution[1]+j; 
      tripoints[1] = tripoints[0]-j+jn; 
      tripoints[2] = tripoints[0]+m_Resolution[1]; 
	  testCell->SetPointIds(tripoints);
	  SetCell(p, testCell);
	  SetCellData(p, (PixelType)3.0);
	  p++;
	  testCell = TriCell::New();
      tripoints[0] = tripoints[1]; 
      tripoints[1] = tripoints[0]+m_Resolution[1]; 
	  testCell->SetPointIds(tripoints);
	  SetCell(p, testCell);
	  SetCellData(p, (PixelType)3.0);
	  p++;
	  testCell = TriCell::New();
	}
  }
 
// store cells containing the south pole nodes
  for (int j=0; j<m_Resolution[1]; j++) {
    jn = (j+1)%m_Resolution[1]; 
    tripoints[0] = numpts-2; 
    tripoints[1] = jn; 
    tripoints[2] = j; 
	testCell->SetPointIds(tripoints);
	SetCell(p, testCell);
	SetCellData(p, (PixelType)1.0);
	p++;
	testCell = TriCell::New();
  }

// store cells containing the north pole nodes
  for (int j=0; j<m_Resolution[1]; j++) {
    jn = (j+1)%m_Resolution[1]; 
    tripoints[2] = (m_Resolution[0]-1)*m_Resolution[1]+j; 
	tripoints[1] = numpts-1; 
    tripoints[0] = tripoints[2]-j+jn; 
	testCell->SetPointIds(tripoints);
	SetCell(p, testCell);
	SetCellData(p, (PixelType)2.0);
	p++;
	testCell = TriCell::New();
  }
}

} // end namespace itk

#endif
