/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkFEMElement2DC0QuadraticTriangular.h"
#include "itkFEMElement2DC0LinearTriangular.h"

namespace itk {
namespace fem {

void
Element2DC0QuadraticTriangular
::GetIntegrationPointAndWeight(unsigned int i, VectorType& pt, Float& w, unsigned int order) const
{
  // FIXME: range checking

  // default integration order
  if (order==0 || order>5) { order=DefaultIntegrationOrder; }

  pt.set_size(3);

  /**
   * We provide implementation for 5 different integration rules
   * as defined in chapter 24 - Implementation of Iso-P Truangular
   * Elements, of http://titan.colorado.edu/courses.d/IFEM.d/.
   *
   * Note that the order parameter here does not correspond to the
   * actual order of integration, but rather the degree of polynomials
   * that are exactly integrated. In addition, there are two integration
   * rules for polynomials of 2nd degree. In order to allow using both of
   * them, we assign the index number 3 to the second one. Note that this
   * does not mean that the rule is capable of integrating the polynomials
   * of 3rd degree. It's just an index of a rule.
   */
  pt.copy_in(Element2DC0LinearTriangular::trigGaussRuleInfo[order][i]);

  // We scale the weight by 0.5, to take into account
  // the factor that must be applied when integrating.
  w=0.5*Element2DC0LinearTriangular::trigGaussRuleInfo[order][i][3];
}

unsigned int
Element2DC0QuadraticTriangular
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // FIXME: range checking

  // default integration order
  if (order==0) { order=DefaultIntegrationOrder; }

  return Element2DC0LinearTriangular::Nip[order];
}

Element2DC0QuadraticTriangular::VectorType
Element2DC0QuadraticTriangular
::ShapeFunctions( const VectorType& pt ) const
{
  // Quadratic triangular element has 6 shape functions
  VectorType shapeF(6);

  // Shape functions are equal to coordinates
  VectorType::element_type p2 = 1.0 - pt[0] - pt[1];
  shapeF[0]=pt[0]*(2*pt[0]-1);
  shapeF[1]=pt[1]*(2*pt[1]-1);
  shapeF[2]=p2*(2*p2-1);
  shapeF[3]=4*pt[0]*pt[1];
  shapeF[4]=4*pt[1]*p2;
  shapeF[5]=4*p2*pt[0];

  return shapeF;
}

void
Element2DC0QuadraticTriangular
::ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const
{
  VectorType::element_type p2 = 1.0 - pt[0] - pt[1];

  shapeD.set_size(3,6);
  shapeD.fill(0.0);

  shapeD[0][0]=4*pt[0]-1;
  shapeD[0][3]=4*pt[1];
  shapeD[0][5]=4*p2;

  shapeD[1][1]=4*pt[1]-1;
  shapeD[1][3]=4*pt[0];
  shapeD[1][4]=4*p2;

  shapeD[2][2]=4*p2-1;
  shapeD[2][4]=4*pt[1];
  shapeD[2][5]=4*pt[0];

}

Element2DC0QuadraticTriangular::Float
Element2DC0QuadraticTriangular
::JacobianDeterminant( const VectorType& pt, const MatrixType* pJ ) const
{

//  return Superclass::JacobianDeterminant( pt, pJ );

  MatrixType* pJlocal=0;

  // If Jacobian was not provided, we
  // need to compute it here
  if(pJ==0)
    {
    pJlocal=new MatrixType();
    this->Jacobian( pt, *pJlocal );
    pJ=pJlocal;
    }

  Float det=(((*pJ)[1][0]-(*pJ)[0][0]) * ((*pJ)[2][1]-(*pJ)[0][1])) -
            (((*pJ)[0][1]-(*pJ)[1][1]) * ((*pJ)[0][0]-(*pJ)[2][0]));

  delete pJlocal;

  return det;

}

void
Element2DC0QuadraticTriangular
::JacobianInverse( const VectorType& pt, MatrixType& invJ, const MatrixType* pJ ) const
{

  MatrixType* pJlocal=0;

  // If Jacobian was not provided, we
  // need to compute it here
  if(pJ==0)
    {
    pJlocal=new MatrixType();
    this->Jacobian( pt, *pJlocal );
    pJ=pJlocal;
    }

  // Note that inverse of Jacobian is not quadratic matrix
  invJ.set_size(2,3);

  Float idet=1.0/this->JacobianDeterminant( pt, pJ );

  invJ[0][0]=idet*((*pJ)[1][1]-(*pJ)[2][1]); invJ[0][1]=idet*((*pJ)[2][1]-(*pJ)[0][1]); invJ[0][2]=idet*((*pJ)[0][1]-(*pJ)[1][1]);
  invJ[1][0]=idet*((*pJ)[2][0]-(*pJ)[1][0]); invJ[1][1]=idet*((*pJ)[0][0]-(*pJ)[2][0]); invJ[1][2]=idet*((*pJ)[1][0]-(*pJ)[0][0]);

  delete pJlocal;
}

/**
 * Draw the element on device context pDC.
 */
#ifdef FEM_BUILD_VISUALIZATION
void
Element2DC0QuadraticTriangular
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

  int x5=m_node[4]->GetCoordinates()[0]*DC_Scale;
  int y5=m_node[4]->GetCoordinates()[1]*DC_Scale;

  int x6=m_node[5]->GetCoordinates()[0]*DC_Scale;
  int y6=m_node[5]->GetCoordinates()[1]*DC_Scale;

  x1 += sol->GetSolutionValue(this->m_node[0]->GetDegreeOfFreedom(0))*DC_Scale;
  y1 += sol->GetSolutionValue(this->m_node[0]->GetDegreeOfFreedom(1))*DC_Scale;
  x2 += sol->GetSolutionValue(this->m_node[1]->GetDegreeOfFreedom(0))*DC_Scale;
  y2 += sol->GetSolutionValue(this->m_node[1]->GetDegreeOfFreedom(1))*DC_Scale;
  x3 += sol->GetSolutionValue(this->m_node[2]->GetDegreeOfFreedom(0))*DC_Scale;
  y3 += sol->GetSolutionValue(this->m_node[2]->GetDegreeOfFreedom(1))*DC_Scale;
  x4 += sol->GetSolutionValue(this->m_node[3]->GetDegreeOfFreedom(0))*DC_Scale;
  y4 += sol->GetSolutionValue(this->m_node[3]->GetDegreeOfFreedom(1))*DC_Scale;
  x5 += sol->GetSolutionValue(this->m_node[4]->GetDegreeOfFreedom(0))*DC_Scale;
  y5 += sol->GetSolutionValue(this->m_node[4]->GetDegreeOfFreedom(1))*DC_Scale;
  x6 += sol->GetSolutionValue(this->m_node[5]->GetDegreeOfFreedom(0))*DC_Scale;
  y6 += sol->GetSolutionValue(this->m_node[5]->GetDegreeOfFreedom(1))*DC_Scale;

  pDC->MoveTo(x1,y1);
  pDC->LineTo(x4,y4);
  pDC->LineTo(x2,y2);
  pDC->LineTo(x5,y5);
  pDC->LineTo(x3,y3);
  pDC->LineTo(x6,y6);
  pDC->LineTo(x1,y1);

}
#endif

}} // end namespace itk::fem
