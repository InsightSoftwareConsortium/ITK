/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0LinearQuadrilateral.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DC0LinearQuadrilateral_txx
#define __itkFEMElement2DC0LinearQuadrilateral_txx

#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




template<unsigned int VNumberOfDegreesOfFreedomPerNode>
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>::VectorType
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>
::GetIntegrationPoint(unsigned int i) const
{
  VectorType ipts(2);

  Float pt = (Float)1.0 / (Float)sqrt(3.0);

  switch(i)
  {
  case 0:
    ipts[0] = -pt;
    ipts[1] = -pt;
    break;

  case 1:
    ipts[0] =  pt;
    ipts[1] = -pt;
    break;

  case 2:
    ipts[0] =  pt;
    ipts[1] =  pt;
    break;

  case 3:
    ipts[0] = -pt;
    ipts[1] =  pt;
    break;

  default:
    // i was out of range
    throw FEMException(__FILE__, __LINE__, "FEM error");
  }

  return ipts;

}



template<unsigned int VNumberOfDegreesOfFreedomPerNode>
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>::Float
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>
::GetWeightAtIntegrationPoint(unsigned int) const
{
  return (Float)1.0;
}



template<unsigned int VNumberOfDegreesOfFreedomPerNode>
unsigned int
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>
::GetNumberOfIntegrationPoints() const
{
  return 4;
}



template<unsigned int VNumberOfDegreesOfFreedomPerNode>
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>::VectorType
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>
::GetNodalCoordinates( unsigned int n ) const
{
  vnl_vector<Float> p(2);
  p[0]=m_point[n]->X;
  p[1]=m_point[n]->Y;
  return p;
}



template<unsigned int VNumberOfDegreesOfFreedomPerNode>
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>::VectorType
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>
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



template<unsigned int VNumberOfDegreesOfFreedomPerNode>
void
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>
::ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const
{
  /** functions at directions r and s.  */
  shapeD.resize(2,4);

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




template<unsigned int VNumberOfDegreesOfFreedomPerNode>
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>::VectorType
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>
::GetLocalFromGlobalCoordinates( const VectorType& pt ) const
{

  Float x1, x2, x3, x4, y1, y2, y3, y4, xce, yce, xb, yb, xcn, ycn,
        A, J1, J2, x0, y0, dx, dy, be, bn, ce, cn;

  VectorType lpt(2);

  x1 = m_point[0]->X;   y1 = m_point[0]->Y;
  x2 = m_point[1]->X;   y2 = m_point[1]->Y;
  x3 = m_point[2]->X;   y3 = m_point[2]->Y;
  x4 = m_point[3]->X;   y4 = m_point[3]->Y;

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

  dx = pt[0] - x0;
  dy = pt[1] - y0;

  be =  A - (dx * yb) + (dy * xb);
  bn = -A - (dx * yb) + (dy * xb);
  ce = (dx * yce) - (dy * xce);
  cn = (dx * ycn) - (dy * xcn);

  lpt[0] = (2 * ce) / (-sqrt((be * be) - (2 * J1 * ce)) - be);
  lpt[1] = (2 * cn) / ( sqrt((bn * bn) + (2 * J2 * cn)) - bn);

  return lpt;
}




/*
 * Draw the element on device context pDC.
 */
#ifdef FEM_BUILD_VISUALIZATION
template<unsigned int VNumberOfDegreesOfFreedomPerNode>
void
Element2DC0LinearQuadrilateral<VNumberOfDegreesOfFreedomPerNode>
::Draw(CDC* pDC, Solution::ConstPointer sol) const 
{

  int x1=m_point[0]->X*DC_Scale;
  int y1=m_point[0]->Y*DC_Scale;
  
  int x2=m_point[1]->X*DC_Scale;
  int y2=m_point[1]->Y*DC_Scale;
  
  int x3=m_point[2]->X*DC_Scale;
  int y3=m_point[2]->Y*DC_Scale;
  
  int x4=m_point[3]->X*DC_Scale;
  int y4=m_point[3]->Y*DC_Scale;

  x1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(0))*DC_Scale;
  y1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(1))*DC_Scale;
  x2+=sol->GetSolutionValue(this->GetDegreeOfFreedom(2))*DC_Scale;
  y2+=sol->GetSolutionValue(this->GetDegreeOfFreedom(3))*DC_Scale;
  x3+=sol->GetSolutionValue(this->GetDegreeOfFreedom(4))*DC_Scale;
  y3+=sol->GetSolutionValue(this->GetDegreeOfFreedom(5))*DC_Scale;
  x4+=sol->GetSolutionValue(this->GetDegreeOfFreedom(6))*DC_Scale;
  y4+=sol->GetSolutionValue(this->GetDegreeOfFreedom(7))*DC_Scale;

  pDC->MoveTo(x1,y1);
  pDC->LineTo(x2,y2);
  pDC->LineTo(x3,y3);
  pDC->LineTo(x4,y4);
  pDC->LineTo(x1,y1);

}
#endif




#ifdef _MSC_VER
// Declare a static dummy function to prevent a MSVC 6.0 SP5 from crashing.
// I have no idea why things don't work when this is not declared, but it
// looks like this declaration makes compiler forget about some of the
// troubles it has with templates.
static void Dummy( void );
#endif // #ifdef _MSC_VER

}} // end namespace itk::fem

#endif // #ifndef __itkFEMElement2DC0LinearQuadrilateral_txx
