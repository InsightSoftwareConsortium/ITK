/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0LinearLine.cxx
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

#include "itkFEMElement2DC0LinearLine.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




void
Element2DC0LinearLine
::GetIntegrationPointAndWeight(unsigned int i, VectorType& pt, Float& w, unsigned int order) const
{
  // FIXME: range checking

  // default integration order
  if (order==0) { order=DefaultIntegrationOrder; }

  pt.resize(1);

  pt[0]=gaussPoint[order][i];
  w=gaussWeight[order][i];

}




unsigned int
Element2DC0LinearLine
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // FIXME: range checking

  // default integration order
  if (order==0) { order=DefaultIntegrationOrder; }

  return order;
}




Element2DC0LinearLine::VectorType
Element2DC0LinearLine
::ShapeFunctions( const VectorType& pt ) const
{
  /* Linear Line element has two shape functions  */
  VectorType shapeF(2);

  shapeF[0] = 0.5 - pt[0]/2.0;
  shapeF[1] = 0.5 + pt[0]/2.0;

  return shapeF;
}




void
Element2DC0LinearLine
::ShapeFunctionDerivatives( const VectorType&, MatrixType& shapeD ) const
{
  shapeD.resize(1,2);

  shapeD[0][0] = -0.5;
  shapeD[0][1] =  0.5;
}




void
Element2DC0LinearLine
::Jacobian( const VectorType&, MatrixType& J, const MatrixType*) const
{
  // Since the line element defines only one global coordinate
  // and lives in 2D space, we need to provide a custom Jacobian.
  J.resize(1,1);

  // Get the length of the element
  // Note: This simple implementation is only valid for linear line elements.
  //       For higher order elements we must integrate to obtain the exact
  //       element length
  Float l=(this->m_node[1]->GetCoordinates() - this->m_node[0]->GetCoordinates()).magnitude();
  J[0][0]=l/2;
}




bool
Element2DC0LinearLine
::GetLocalFromGlobalCoordinates( const VectorType& globalPt , VectorType& localPt ) const
{

  // FIXME: write proper implementation
  localPt=globalPt;

  return false;
}




/*
 * Draw the element on device context pDC.
 */
#ifdef FEM_BUILD_VISUALIZATION
void
Element2DC0LinearLine
::Draw(CDC* pDC, Solution::ConstPointer sol) const
{

  int x1=m_node[0]->GetCoordinates()[0]*DC_Scale;
  int y1=m_node[0]->GetCoordinates()[1]*DC_Scale;
  int x2=m_node[1]->GetCoordinates()[0]*DC_Scale;
  int y2=m_node[1]->GetCoordinates()[1]*DC_Scale;
  

  x1+=sol->GetSolutionValue(this->m_node[0]->GetDegreeOfFreedom(0))*DC_Scale;
  y1+=sol->GetSolutionValue(this->m_node[0]->GetDegreeOfFreedom(1))*DC_Scale;
  x2+=sol->GetSolutionValue(this->m_node[1]->GetDegreeOfFreedom(0))*DC_Scale;
  y2+=sol->GetSolutionValue(this->m_node[1]->GetDegreeOfFreedom(1))*DC_Scale;

  pDC->MoveTo(x1,y1);
  pDC->LineTo(x2,y2);

}
#endif




}} // end namespace itk::fem
