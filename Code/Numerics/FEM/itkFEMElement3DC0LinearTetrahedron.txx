/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement3DC0LinearTetrahedron.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement3DC0LinearTetrahedron_txx
#define __itkFEMElement3DC0LinearTetrahedron_txx

#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




template<unsigned int VNumberOfDegreesOfFreedomPerNode>
Element3DC0LinearTetrahedron<VNumberOfDegreesOfFreedomPerNode>::VectorType
Element3DC0LinearTetrahedron<VNumberOfDegreesOfFreedomPerNode>
::GetIntegrationPoint(unsigned int i) const
{
  VectorType ipts(3);

  Float pt = (Float)1.0 / (Float)sqrt(3.0);

  ipts[0] = pt;
  ipts[1] = pt;
  ipts[2] = pt;

  return ipts;

}



template<unsigned int VNumberOfDegreesOfFreedomPerNode>
Element3DC0LinearTetrahedron<VNumberOfDegreesOfFreedomPerNode>::Float
Element3DC0LinearTetrahedron<VNumberOfDegreesOfFreedomPerNode>
::GetWeightAtIntegrationPoint(unsigned int) const
{
  return (Float)1.0;
}



template<unsigned int VNumberOfDegreesOfFreedomPerNode>
unsigned int
Element3DC0LinearTetrahedron<VNumberOfDegreesOfFreedomPerNode>
::GetNumberOfIntegrationPoints() const
{
  return 1;
}




template<unsigned int VNumberOfDegreesOfFreedomPerNode>
Element3DC0LinearTetrahedron<VNumberOfDegreesOfFreedomPerNode>::VectorType
Element3DC0LinearTetrahedron<VNumberOfDegreesOfFreedomPerNode>
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



template<unsigned int VNumberOfDegreesOfFreedomPerNode>
void
Element3DC0LinearTetrahedron<VNumberOfDegreesOfFreedomPerNode>
::ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const
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




template<unsigned int VNumberOfDegreesOfFreedomPerNode>
Element3DC0LinearTetrahedron<VNumberOfDegreesOfFreedomPerNode>::VectorType
Element3DC0LinearTetrahedron<VNumberOfDegreesOfFreedomPerNode>
::GetLocalFromGlobalCoordinates( const VectorType& pt ) const
{

//   Float x1, x2, x3, x4, y1, y2, y3, y4, xce, yce, xb, yb, xcn, ycn,
//         A, J1, J2, x0, y0, dx, dy, be, bn, ce, cn;

  VectorType lpt(3);

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

//   dx = pt[0] - x0;
//   dy = pt[1] - y0;

//   be =  A - (dx * yb) + (dy * xb);
//   bn = -A - (dx * yb) + (dy * xb);
//   ce = (dx * yce) - (dy * xce);
//   cn = (dx * ycn) - (dy * xcn);

//   lpt[0] = (2 * ce) / (-sqrt((be * be) - (2 * J1 * ce)) - be);
//   lpt[1] = (2 * cn) / ( sqrt((bn * bn) + (2 * J2 * cn)) - bn);

  return lpt;
}




/*
 * Draw the element on device context pDC.
 */
#ifdef FEM_BUILD_VISUALIZATION
template<unsigned int VNumberOfDegreesOfFreedomPerNode>
void
Element3DC0LinearTetrahedron<VNumberOfDegreesOfFreedomPerNode>
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




#ifdef _MSC_VER
// Declare a static dummy function to prevent a MSVC 6.0 SP5 from crashing.
// I have no idea why things don't work when this is not declared, but it
// looks like this declaration makes compiler forget about some of the
// troubles it has with templates.
static void Dummy( void );
#endif // #ifdef _MSC_VER

}} // end namespace itk::fem

#endif // #ifndef __itkFEMElement3DC0LinearTetrahedron_txx
