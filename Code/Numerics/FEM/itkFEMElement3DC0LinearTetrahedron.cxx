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
  pt.set_size(3);

  Float d = 1.0/vcl_sqrt(3.0);

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
  shapeD.set_size(3,4);
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
  Float x = globalPt[0];
  Float y = globalPt[1];
  Float z = globalPt[2];

  Float x0, x1, x2, x3;
  Float y0, y1, y2, y3;
  Float z0, z1, z2, z3;
  Float A;

  localPt.set_size(3);
  localPt.fill(0.0);

  x0 = this->m_node[0]->GetCoordinates()[0];   
  y0 = this->m_node[0]->GetCoordinates()[1];
  z0 = this->m_node[0]->GetCoordinates()[2];

  x1 = this->m_node[1]->GetCoordinates()[0];   
  y1 = this->m_node[1]->GetCoordinates()[1];
  z1 = this->m_node[1]->GetCoordinates()[2];

  x2 = this->m_node[2]->GetCoordinates()[0];   
  y2 = this->m_node[2]->GetCoordinates()[1];
  z2 = this->m_node[2]->GetCoordinates()[2];

  x3 = this->m_node[3]->GetCoordinates()[0];   
  y3 = this->m_node[3]->GetCoordinates()[1];
  z3 = this->m_node[3]->GetCoordinates()[2];

  A = (x1-x0) * ((y2-y0)*(z3-z0)-(z2-z0)*(y3-y0))
    - (x2-x0) * ((y1-y0)*(z3-z0)-(z1-z0)*(y3-y0))
    + (x3-x0) * ((y1-y0)*(z2-z0)-(z1-z0)*(y2-y0));

  localPt[0] = 1/A *
    (
       (x-x0)*((y2-y0)*(z3-z0)-(z2-z0)*(y3-y0))
     - (y-y0)*((x2-x0)*(z3-z0)-(z2-z0)*(x3-x0))
     + (z-z0)*((x2-x0)*(y3-y0)-(y2-y0)*(x3-x0))
    );
  
  localPt[1] = 1/A *
    (
     - (x-x0)*((y1-y0)*(z3-z0)-(z1-z0)*(y3-y0))
     + (y-y0)*((x1-x0)*(z3-z0)-(z1-z0)*(x3-x0))
     - (z-z0)*((x1-x0)*(y3-y0)-(y1-y0)*(x3-x0))
    );
  
  localPt[2] = 1/A *
    (
       (x-x0)*((y1-y0)*(z2-z0)-(z1-z0)*(y2-y0))
     - (y-y0)*((x1-x0)*(z2-z0)-(z1-z0)*(x2-x0))
     + (z-z0)*((x1-x0)*(y2-y0)-(y1-y0)*(x2-x0))
    );
  
  const double FEM_TETRA_EPSILON = 1e-5;

  if (localPt[0] < (0.0 - FEM_TETRA_EPSILON)
      || localPt[0] > (1.0 + FEM_TETRA_EPSILON)
      || localPt[1] < (0.0 - FEM_TETRA_EPSILON)
      || localPt[1] > (1.0 + FEM_TETRA_EPSILON)
      || localPt[2] < (0.0 - FEM_TETRA_EPSILON)
      || localPt[2] > (1.0 + FEM_TETRA_EPSILON)
      || ( (localPt[0]+localPt[1]+localPt[2]) > (1.0 + FEM_TETRA_EPSILON) ) 
      )
  {
    return false;
  }
  else
  {
    return true;
  }
  
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
