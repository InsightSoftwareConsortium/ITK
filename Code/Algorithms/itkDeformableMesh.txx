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
#include <iostream>
#include <string>
#include <math.h>
#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkDeformableMeshTest.h"
#include "vnl_math.h"

namespace itk
{
template <typename PixelType>
DeformableMeshTest<PixelType>
::DeformableMeshTest()
{
}


template <typename PixelType>
void
DeformableMeshTest<PixelType>
::SetCenter(int a, int b, int c)
{
  Center[0] = a;
  Center[1] = b;
  Center[2] = c;
}

template <typename PixelType>
void
DeformableMeshTest<PixelType>
::SetResolution(int a, int b)
{
  Resolution[0] = a;
  Resolution[1] = b;
}

template <typename PixelType>
void
DeformableMeshTest<PixelType>
::SetScale(float a, float b, float c)
{
  Scale[0] = a;
  Scale[1] = b;
  Scale[2] = c;
}

template <typename PixelType>
void
DeformableMeshTest<PixelType>
::SetDefault()
{
  Scale[0] = 1.0;
  Scale[1] = 1.0;
  Scale[2] = 1.0;
  Center[0] = 0.0;
  Center[1] = 0.0;
  Center[2] = 0.0;
  Resolution[0] = 8;
  Resolution[1] = 18;
}

template <typename PixelType>
void
DeformableMeshTest<PixelType>
::Allocate()
{
  float PointCoords[3];
  
  /**
   * List the points that the tetrahedron will use from the mesh.
   */
    
  unsigned long ii, i, j, jn, e, p, numpts, numcells;
  float x[3], ustep, vstep, ubeg, vbeg, u, v; 
  int pts[3], signu, signv; 
  numpts = Resolution[0]*Resolution[1] + 2; 

  numcells = 2 * (Resolution[0]-1) *Resolution[1] + 2*Resolution[1]; 

  ustep = vnl_math::pi / (Resolution[0]+1); 
  vstep = 2.0*vnl_math::pi / Resolution[1]; 
  ubeg = (-vnl_math::pi/2.0) + ustep; 
  vbeg = -vnl_math::pi; 
  /**
   * List the points that the hexahedron will use from the mesh.
   */
  unsigned long tripoints[3] = {0,1,2};
  const unsigned long *tp;
  
  p = 0; 
  for (u=ubeg, i=0; i < Resolution[0]; u += ustep, i++) { 
	for (v=vbeg, j=0; j < Resolution[1]; v += vstep, j++) { 
	  if (cos(u) > 0) {signu = 1;} else {signu = -1;}
	  if (cos(v) > 0) {signv = 1;} else {signv = -1;}
      PointCoords[0] = Scale[0]*signu*(pow(fabs(cos(u)),1.0))*signv* 
		(pow(fabs(cos(v)),1.0)) + Center[0]; 
	  if (sin(v) > 0) {signv = 1;} else {signv = -1;}
	  PointCoords[1] = Scale[1]*signu*(pow(fabs(cos(u)),1.0))*signv* 
		(pow(fabs(sin(v)),1.0)) + Center[1]; 
	  if (sin(u) > 0) {signu = 1;} else {signu = -1;}
	  PointCoords[2] = Scale[2]*signu*(pow(fabs(sin(u)),1.0)) + Center[2];
	  this->SetPoint(p, PointType(PointCoords));
      p++;
	} 
  }   

  PointCoords[0] = Scale[0]*(pow(fabs(cos(-vnl_math::pi/2)),1.0))* 
	(pow(fabs(cos(0.0)),1.0)) + Center[0]; 
  PointCoords[1] = Scale[1]*(pow(fabs(cos(-vnl_math::pi/2)),1.0))* 
	(pow(fabs(sin(0.0)),1.0)) + Center[1]; 
  PointCoords[2] = Scale[2]*-1*(pow(fabs(sin(-vnl_math::pi/2)),1.0)) + Center[2];
  this->SetPoint(p, (PointCoords));
  p++;
  PointCoords[0] = Scale[0]*(pow(fabs(cos(vnl_math::pi/2)),1.0))* 
	(pow(fabs(cos(0.0)),1.0)) + Center[0]; 
  PointCoords[1] = Scale[1]*(pow(fabs(cos(vnl_math::pi/2)),1.0))* 
	(pow(fabs(sin(0.0)),1.0)) + Center[1]; 
  PointCoords[2] = Scale[2]*(pow(fabs(sin(vnl_math::pi/2)),1.0)) + Center[2];
  this->SetPoint(p, (PointCoords));
  p++;

//////////////////////////////////////////////////
// for test

  CoordRepType d[2][3];
  float ds[3];
//  SetPoint(1, (PointType)d);
  this->GetPoint(1, (PointType*)(d[0]));
  this->GetPoint(2, (PointType*)(d[1]));

//////////////////////////////////////////////////

  p = 1;
  Cell::Pointer testCell(TriCell::New());

  for(int i=0; i < Resolution[0]-1 ; i++) {
	for (int j=0; j<Resolution[1]; j++) {
      jn = (j+1)%Resolution[1]; 
      tripoints[0] = i*Resolution[1]+j; 
      tripoints[1] = tripoints[0]-j+jn; 
      tripoints[2] = tripoints[0]+Resolution[1]; 
	  testCell->SetPointIds(tripoints);
	  SetCell(p, testCell);
	  SetCellData(p, (PixelType)3.0);
	  p++;
	  testCell = TriCell::New();
      tripoints[0] = tripoints[1]; 
      tripoints[1] = tripoints[0]+Resolution[1]; 
	  testCell->SetPointIds(tripoints);
	  SetCell(p, testCell);
	  SetCellData(p, (PixelType)3.0);
	  p++;
	  testCell = TriCell::New();
	}
  }
//////////////////////////////////////////////////
// for test
  float *xx, x1;
  xx = &x1;

  SetCellData(0, (PixelType)3.0);
  GetCellData(0, (PixelType*)xx);
//////////////////////////////////////////////////
  for (int j=0; j<Resolution[1]; j++)
	  {
      jn = (j+1)%Resolution[1]; 
      tripoints[0] = numpts-2; 
      tripoints[1] = jn; 
      tripoints[2] = j; 
	  testCell->SetPointIds(tripoints);
	  SetCell(p, testCell);
	  SetCellData(p, (PixelType)1.0);
	  p++;
	  testCell = TriCell::New();
  }

  for (int j=0; j<Resolution[1]; j++)
	  {
      jn = (j+1)%Resolution[1]; 
      tripoints[2] = (Resolution[0]-1)*Resolution[1]+j; 
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

  