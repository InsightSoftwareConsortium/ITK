/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement3DC0LinearTetrahedron.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMElement3DC0LinearTetrahedron.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




void
Element3DC0LinearTetrahedron
::GetIntegrationPointAndWeight( unsigned int, VectorType& pt, Float& w, unsigned int ) const
{
  // FIXME: Write rules for other integration orders
  pt.resize(3);

  Float d = 1.0/sqrt(3.0);

  pt[0] = d;
  pt[1] = d;
  pt[2] = d;

  w=1.0;

}




unsigned int
Element3DC0LinearTetrahedron
::GetNumberOfIntegrationPoints( unsigned int ) const
{
  return 1;
}




Element3DC0LinearTetrahedron::VectorType
Element3DC0LinearTetrahedron
::ShapeFunctions( const VectorType& pt ) const
{
  /* Linear tetrahedral element has four shape functions  */
  VectorType shapeF(4);

  /**
   * Linear tetrahedral element has local coordinates
   * (0,0,0), (1,0,0), (0,1,0), (0,0,1)
   */
  
  /** given local point x=(r,s,t), where 0 <= r,s,t <= 1 */

  /** N_1 = 1 - r - s - t; */
  shapeF[0] = 1 - pt[0] - pt[1] - pt[2];

  /** N_2 = r */
  shapeF[1] = pt[0];

  /** N_3 = s */
  shapeF[2] = pt[1];

  /** N_4 = t */
  shapeF[3] = pt[2];

  return shapeF;
}




void
Element3DC0LinearTetrahedron
::ShapeFunctionDerivatives( const VectorType&, MatrixType& shapeD ) const
{
  /** functions at directions r and s.  */
  shapeD.resize(3,4);
  shapeD.fill(0.0);

  /** d(N_1) / d(r,s,t) = -1 */
  for (int j=0; j < 3; j++)
    shapeD[j][0] = -1;

  /** d(N_2) / dr, d(N_3) / ds, d(N_4) / dt = 1 */
  for (int j=1; j < 4; j++)
    shapeD[j-1][j] = 1;

}




bool
Element3DC0LinearTetrahedron
::GetLocalFromGlobalCoordinates( const VectorType& globalPt , VectorType& localPt ) const
{

//   Float x1, x2, x3, x4, y1, y2, y3, y4, xce, yce, xb, yb, xcn, ycn,
//         A, J1, J2, x0, y0, dx, dy, be, bn, ce, cn;

  localPt=globalPt;
  localPt.resize(3);
  localPt.fill(0.0);

  // FIXME!

//   x1 = this->m_node[0]->GetCoordinates()[0];   y1 = this->m_node[0]->GetCoordinates()[1];
//   x2 = this->m_node[1]->GetCoordinates()[0];   y2 = this->m_node[1]->GetCoordinates()[1];
//   x3 = this->m_node[2]->GetCoordinates()[0];   y3 = this->m_node[2]->GetCoordinates()[1];
//   x4 = this->m_node[3]->GetCoordinates()[0];   y4 = this->m_node[3]->GetCoordinates()[1];

//   xb = x1 - x2 + x3 - x4;
//   yb = y1 - y2 + y3 - y4;

//   xce = x1 + x2 - x3 - x4;
//   yce = y1 + y2 - y3 - y4;

//   xcn = x1 - x2 - x3 + x4;
//   ycn = y1 - y2 - y3 + y4;

//   A  = 0.5 * (((x3 - x1) * (y4 - y2)) - ((x4 - x2) * (y3 - y1)));
//   J1 = ((x3 - x4) * (y1 - y2)) - ((x1 - x2) * (y3 - y4));
//   J2 = ((x2 - x3) * (y1 - y4)) - ((x1 - x4) * (y2 - y3));

//   x0 = 0.25 * (x1 + x2 + x3 + x4);
//   y0 = 0.25 * (y1 + y2 + y3 + y4);

//   dx = globalPt[0] - x0;
//   dy = globalPt[1] - y0;

//   be =  A - (dx * yb) + (dy * xb);
//   bn = -A - (dx * yb) + (dy * xb);
//   ce = (dx * yce) - (dy * xce);
//   cn = (dx * ycn) - (dy * xcn);

//   localPt[0] = (2 * ce) / (-sqrt((be * be) - (2 * J1 * ce)) - be);
//   localPt[1] = (2 * cn) / ( sqrt((bn * bn) + (2 * J2 * cn)) - bn);

  bool IsInside=false;

  return IsInside;
}




/*
 * Draw the element on device context pDC.
 */
#ifdef FEM_BUILD_VISUALIZATION
void
Element3DC0LinearTetrahedron
::Draw(CDC* pDC, Solution::ConstPointer sol) const 
{

  int x1=m_node[0]->GetCoordinates()[0]*DC_Scale;
  int y1=m_node[0]->GetCoordinates()[1]*DC_Scale;
  int z1=m_node[0]->GetCoordinates()[2]*DC_Scale;

  int x2=m_node[1]->GetCoordinates()[0]*DC_Scale;
  int y2=m_node[1]->GetCoordinates()[1]*DC_Scale;
  int z2=m_node[1]->GetCoordinates()[2]*DC_Scale;
  
  int x3=m_node[2]->GetCoordinates()[0]*DC_Scale;
  int y3=m_node[2]->GetCoordinates()[1]*DC_Scale;
  int z3=m_node[2]->GetCoordinates()[2]*DC_Scale;
  
  int x4=m_node[3]->GetCoordinates()[0]*DC_Scale;
  int y4=m_node[3]->GetCoordinates()[1]*DC_Scale;
  int z4=m_node[3]->GetCoordinates()[2]*DC_Scale;

  x1+=sol->GetSolutionValue(this->m_node[0]->GetDegreeOfFreedom(0))*DC_Scale;
  y1+=sol->GetSolutionValue(this->m_node[0]->GetDegreeOfFreedom(1))*DC_Scale;
  z1+=sol->GetSolutionValue(this->m_node[0]->GetDegreeOfFreedom(2))*DC_Scale;

  x2+=sol->GetSolutionValue(this->m_node[1]->GetDegreeOfFreedom(0))*DC_Scale;
  y2+=sol->GetSolutionValue(this->m_node[1]->GetDegreeOfFreedom(1))*DC_Scale;
  z2+=sol->GetSolutionValue(this->m_node[1]->GetDegreeOfFreedom(2))*DC_Scale;

  x3+=sol->GetSolutionValue(this->m_node[2]->GetDegreeOfFreedom(0))*DC_Scale;
  y3+=sol->GetSolutionValue(this->m_node[2]->GetDegreeOfFreedom(1))*DC_Scale;
  z3+=sol->GetSolutionValue(this->m_node[2]->GetDegreeOfFreedom(2))*DC_Scale;

  x4+=sol->GetSolutionValue(this->m_node[3]->GetDegreeOfFreedom(0))*DC_Scale;
  y4+=sol->GetSolutionValue(this->m_node[3]->GetDegreeOfFreedom(1))*DC_Scale;
  z4+=sol->GetSolutionValue(this->m_node[3]->GetDegreeOfFreedom(2))*DC_Scale;

  // FIXME: this may not be the correct drawing scheme
/*  pDC->MoveTo(x1,y1,z1);
  pDC->LineTo(x2,y2,z2);
  pDC->LineTo(x3,y3,z3);
  pDC->LineTo(x4,y4,z4);
  pDC->LineTo(x1,y1,z1);
  pDC->MoveTo(x4,y4,z4);
  pDC->LineTo(x2,y2,z2);
  */

}
#endif




}} // end namespace itk::fem
