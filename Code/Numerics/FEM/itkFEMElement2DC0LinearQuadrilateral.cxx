/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0LinearQuadrilateral.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMElement2DC0LinearQuadrilateral.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




void
Element2DC0LinearQuadrilateral
::GetIntegrationPointAndWeight(unsigned int i, VectorType& pt, Float& w, unsigned int order) const
{
  // FIXME: range checking

  // default integration order
  if (order==0) { order=DefaultIntegrationOrder; }

  pt.set_size(2);

  pt[0]=gaussPoint[order][i%order];
  pt[1]=gaussPoint[order][i/order];

  w=gaussWeight[order][i%order]*gaussWeight[order][i/order];

}




unsigned int
Element2DC0LinearQuadrilateral
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // FIXME: range checking

  // default integration order
  if (order==0) { order=DefaultIntegrationOrder; }

  return order*order;
}




Element2DC0LinearQuadrilateral::VectorType
Element2DC0LinearQuadrilateral
::ShapeFunctions( const VectorType& pt ) const
{
  /* Linear quadrilateral element has four shape functions  */
  VectorType shapeF(4);

  /*
   * Linear quadrilateral element has local coordinates
   * (-1,-1), (1,-1), (1,1), and (-1,1)
   */
  
  /* given local point x=(r,s), where -1 <= r,s <= 1 and */

  /* shape function 1: ((1 - r) * (1 - s)) / 4  (node 1) */
  shapeF[0] = (1 - pt[0]) * (1 - pt[1]) * .25;

  /* shape function 2: ((1 + r) * (1 - s)) / 4  (node 2) */
  shapeF[1] = (1 + pt[0]) * (1 - pt[1]) * .25;

  /* shape function 3: ((1 + r) * (1 + s)) / 4  (node 3) */
  shapeF[2] = (1 + pt[0]) * (1 + pt[1]) * .25;

  /* shape function 1: ((1 - r) * (1 + s)) / 4  (node 4) */
  shapeF[3] = (1 - pt[0]) * (1 + pt[1]) * .25;

  return shapeF;
}




void
Element2DC0LinearQuadrilateral
::ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const
{
  /** functions at directions r and s.  */
  shapeD.set_size(2,4);

  /** Derivative w.r.t r for shape function 1 (node 1) */
  shapeD[0][0] = -(1 - pt[1]) * .25;

  /** Derivative w.r.t s for shape function 1 (node 1) */
  shapeD[1][0] = -(1 - pt[0]) * .25;

  /** Derivative w.r.t r for shape function 2 (node 2) */
  shapeD[0][1] = +(1 - pt[1]) * .25;

  /** Derivative w.r.t s for shape function 2 (node 2) */
  shapeD[1][1] = -(1 + pt[0]) * .25;

  /** Derivative w.r.t r for shape function 3 (node 3) */
  shapeD[0][2] = +(1 + pt[1]) * .25;

  /** Derivative w.r.t s for shape function 3 (node 3) */
  shapeD[1][2] = +(1 + pt[0]) * .25;

  /** Derivative w.r.t r for shape function 4 (node 4) */
  shapeD[0][3] = -(1 + pt[1]) * .25;

  /** Derivative w.r.t s for shape function 4 (node 4) */
  shapeD[1][3] = +(1 - pt[0]) * .25;

}



bool
Element2DC0LinearQuadrilateral
::GetLocalFromGlobalCoordinates( const VectorType& globalPt , VectorType& localPt) const
{

  Float x1, x2, x3, x4, y1, y2, y3, y4, xce, yce, xb, yb, xcn, ycn,
        A, J1, J2, x0, y0, dx, dy, be, bn, ce, cn;

  localPt.set_size(2);
  localPt.fill(0.0);

  x1 = this->m_node[0]->GetCoordinates()[0];   y1 = this->m_node[0]->GetCoordinates()[1];
  x2 = this->m_node[1]->GetCoordinates()[0];   y2 = this->m_node[1]->GetCoordinates()[1];
  x3 = this->m_node[2]->GetCoordinates()[0];   y3 = this->m_node[2]->GetCoordinates()[1];
  x4 = this->m_node[3]->GetCoordinates()[0];   y4 = this->m_node[3]->GetCoordinates()[1];

  xb = x1 - x2 + x3 - x4;
  yb = y1 - y2 + y3 - y4;

  xce = x1 + x2 - x3 - x4;
  yce = y1 + y2 - y3 - y4;

  xcn = x1 - x2 - x3 + x4;
  ycn = y1 - y2 - y3 + y4;

  A  = 0.5 * (((x3 - x1) * (y4 - y2)) - ((x4 - x2) * (y3 - y1)));
  J1 = ((x3 - x4) * (y1 - y2)) - ((x1 - x2) * (y3 - y4));
  J2 = ((x2 - x3) * (y1 - y4)) - ((x1 - x4) * (y2 - y3));

  x0 = 0.25 * (x1 + x2 + x3 + x4);
  y0 = 0.25 * (y1 + y2 + y3 + y4);

  dx = globalPt[0] - x0;
  dy = globalPt[1] - y0;

  be =  A - (dx * yb) + (dy * xb);
  bn = -A - (dx * yb) + (dy * xb);
  ce = (dx * yce) - (dy * xce);
  cn = (dx * ycn) - (dy * xcn);

  localPt[0] = (2 * ce) / (-sqrt((be * be) - (2 * J1 * ce)) - be);
  localPt[1] = (2 * cn) / ( vcl_sqrt((bn * bn) + (2 * J2 * cn)) - bn);

  bool isInside=true;

  if (localPt[0] < -1.0 || localPt[0] > 1.0 || localPt[1] < -1.0 || localPt[1] > 1.0 )
  {
    isInside=false;
  }

  return isInside;
}




/*
 * Draw the element on device context pDC.
 */
#ifdef FEM_BUILD_VISUALIZATION
void
Element2DC0LinearQuadrilateral
::Draw(CDC* pDC, Solution::ConstPointer sol) const
{

  int x1=m_node[0]->GetCoordinates()[0]*DC_Scale;
  int y1=m_node[0]->GetCoordinates()[1]*DC_Scale;
  
  int x2=m_node[1]->GetCoordinates()[0]*DC_Scale;
  int y2=m_node[1]->GetCoordinates()[1]*DC_Scale;
  
  int x3=m_node[2]->GetCoordinates()[0]*DC_Scale;
  int y3=m_node[2]->GetCoordinates()[1]*DC_Scale;
  
  int x4=m_node[3]->GetCoordinates()[0]*DC_Scale;
  int y4=m_node[3]->GetCoordinates()[1]*DC_Scale;

  x1+=sol->GetSolutionValue(this->m_node[0]->GetDegreeOfFreedom(0))*DC_Scale;
  y1+=sol->GetSolutionValue(this->m_node[0]->GetDegreeOfFreedom(1))*DC_Scale;
  x2+=sol->GetSolutionValue(this->m_node[1]->GetDegreeOfFreedom(0))*DC_Scale;
  y2+=sol->GetSolutionValue(this->m_node[1]->GetDegreeOfFreedom(1))*DC_Scale;
  x3+=sol->GetSolutionValue(this->m_node[2]->GetDegreeOfFreedom(0))*DC_Scale;
  y3+=sol->GetSolutionValue(this->m_node[2]->GetDegreeOfFreedom(1))*DC_Scale;
  x4+=sol->GetSolutionValue(this->m_node[3]->GetDegreeOfFreedom(0))*DC_Scale;
  y4+=sol->GetSolutionValue(this->m_node[3]->GetDegreeOfFreedom(1))*DC_Scale;

  pDC->MoveTo(x1,y1);
  pDC->LineTo(x2,y2);
  pDC->LineTo(x3,y3);
  pDC->LineTo(x4,y4);
  pDC->LineTo(x1,y1);

}
#endif




}} // end namespace itk::fem
