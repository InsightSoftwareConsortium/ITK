/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0LinearTriangular.cxx
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

#include "itkFEMElement2DC0LinearTriangular.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




const double
Element2DC0LinearTriangular
::trigGaussRuleInfo[6][7][4] = 
{
  { // order=0, never used
    { 0.0 } 
  },
  { // order=1
    //<-------------------------- point ---------------------------> <-------weight----->
    {  0.33333333333333333, 0.33333333333333333, 0.33333333333333333, 1.00000000000000000}
  },
  
  { // order=2
    {  0.66666666666666667, 0.16666666666666667, 0.16666666666666667, 0.33333333333333333},
    {  0.16666666666666667, 0.66666666666666667, 0.16666666666666667, 0.33333333333333333},
    {  0.16666666666666667, 0.16666666666666667, 0.66666666666666667, 0.33333333333333333}
  },

  { // order=3, p=-3 in the book
    {  0.00000000000000000, 0.50000000000000000, 0.50000000000000000, 0.33333333333333333},
    {  0.50000000000000000, 0.00000000000000000, 0.50000000000000000, 0.33333333333333333},
    {  0.50000000000000000, 0.50000000000000000, 0.00000000000000000, 0.33333333333333333}
  },

  { // order=4, p=6 in the book
    {  0.10810301816807023, 0.44594849091596489, 0.44594849091596489, 0.22338158967801147},
    {  0.44594849091596489, 0.10810301816807023, 0.44594849091596489, 0.22338158967801147},
    {  0.44594849091596489, 0.44594849091596489, 0.10810301816807023, 0.22338158967801147},
    {  0.81684757298045851, 0.09157621350977074, 0.09157621350977074, 0.10995174365532187},
    {  0.09157621350977074, 0.81684757298045851, 0.09157621350977074, 0.10995174365532187},
    {  0.09157621350977074, 0.09157621350977074, 0.81684757298045851, 0.10995174365532187}
  },

  { // order=5, p=7 in the book
    {  0.33333333333333333, 0.33333333333333333, 0.33333333333333333, 0.22500000000000000},
    {  0.79742698535308732, 0.10128650732345634, 0.10128650732345634, 0.12593918054482715},
    {  0.10128650732345634, 0.79742698535308732, 0.10128650732345634, 0.12593918054482715},
    {  0.10128650732345634, 0.10128650732345634, 0.79742698535308732, 0.12593918054482715},
    {  0.05971587178976982, 0.47014206410511509, 0.47014206410511509, 0.13239415278850618},
    {  0.47014206410511509, 0.05971587178976982, 0.47014206410511509, 0.13239415278850618},
    {  0.47014206410511509, 0.47014206410511509, 0.05971587178976982, 0.13239415278850618}
  }
};

const unsigned int 
Element2DC0LinearTriangular
::Nip[6]=
{
  0,1,3,3,6,7
};



void
Element2DC0LinearTriangular
::GetIntegrationPointAndWeight(unsigned int i, VectorType& pt, Float& w, unsigned int order) const
{
  // FIXME: range checking

  // default integration order
  if (order==0 || order>5) { order=DefaultIntegrationOrder; }

  pt.resize(3);

  /*
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
  pt.copy_in(trigGaussRuleInfo[order][i]);

  // We scale the weight by 0.5, to take into account
  // the factor that must be applied when integrating.
  w=0.5*trigGaussRuleInfo[order][i][3];
}




unsigned int
Element2DC0LinearTriangular
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // FIXME: range checking

  // default integration order
  if (order==0) { order=DefaultIntegrationOrder; }

  return Nip[order];
}




Element2DC0LinearTriangular::VectorType
Element2DC0LinearTriangular
::ShapeFunctions( const VectorType& pt ) const
{
  // Linear triangular element has 3 shape functions
  VectorType shapeF(3);

  // Shape functions are equal to coordinates
  shapeF=pt;

  return shapeF;
}




void
Element2DC0LinearTriangular
::ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const
{
  // Matrix of shape functions derivatives is an
  // identity matrix for linear triangular element.
  shapeD.resize(3,3);
  shapeD.fill(0.0);
  shapeD[0][0]=1.0;
  shapeD[1][1]=1.0;
  shapeD[2][2]=1.0;
}




bool
Element2DC0LinearTriangular
::GetLocalFromGlobalCoordinates( const VectorType& globalPt , VectorType& localPt) const
{

  Float x, x1, x2, x3, 
        y, y1, y2, y3,
        A;

  localPt.resize(3);

  x=globalPt[0]; y=globalPt[1];
  x1 = this->m_node[0]->GetCoordinates()[0];   y1 = this->m_node[0]->GetCoordinates()[1];
  x2 = this->m_node[1]->GetCoordinates()[0];   y2 = this->m_node[1]->GetCoordinates()[1];
  x3 = this->m_node[2]->GetCoordinates()[0];   y3 = this->m_node[2]->GetCoordinates()[1];

  A=x1*y2 - x2*y1 + x3*y1 - x1*y3 + x2*y3 - x3*y2;
  localPt[0]=((y2 - y3)*x + (x3 - x2)*y + x2*y3 - x3*y2)/A;
  localPt[1]=((y3 - y1)*x + (x1 - x3)*y + x3*y1 - x1*y3)/A;
  localPt[2]=((y1 - y2)*x + (x2 - x1)*y + x1*y2 - x2*y1)/A;

  if (localPt[0] < 0.0 || localPt[0] > 1.0 || localPt[1] < 0.0 || localPt[1] > 1.0 || localPt[2] < 0.0 || localPt[2] > 1.0 )
  {
    return false;
  }
  else
  {
    return true;
  }
}




Element2DC0LinearTriangular::Float
Element2DC0LinearTriangular
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
Element2DC0LinearTriangular
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
  invJ.resize(2,3);

  Float idet=1.0/this->JacobianDeterminant( pt, pJ );

  invJ[0][0]=idet*((*pJ)[1][1]-(*pJ)[2][1]); invJ[0][1]=idet*((*pJ)[2][1]-(*pJ)[0][1]); invJ[0][2]=idet*((*pJ)[0][1]-(*pJ)[1][1]);
  invJ[1][0]=idet*((*pJ)[2][0]-(*pJ)[1][0]); invJ[1][1]=idet*((*pJ)[0][0]-(*pJ)[2][0]); invJ[1][2]=idet*((*pJ)[1][0]-(*pJ)[0][0]);

  delete pJlocal;
}




/*
 * Draw the element on device context pDC.
 */
#ifdef FEM_BUILD_VISUALIZATION
void
Element2DC0LinearTriangular
::Draw(CDC* pDC, Solution::ConstPointer sol) const 
{

  int x1=m_node[0]->GetCoordinates()[0]*DC_Scale;
  int y1=m_node[0]->GetCoordinates()[1]*DC_Scale;
  
  int x2=m_node[1]->GetCoordinates()[0]*DC_Scale;
  int y2=m_node[1]->GetCoordinates()[1]*DC_Scale;
  
  int x3=m_node[2]->GetCoordinates()[0]*DC_Scale;
  int y3=m_node[2]->GetCoordinates()[1]*DC_Scale;
  
  x1+=sol->GetSolutionValue(this->m_node[0]->GetDegreeOfFreedom(0))*DC_Scale;
  y1+=sol->GetSolutionValue(this->m_node[0]->GetDegreeOfFreedom(1))*DC_Scale;
  x2+=sol->GetSolutionValue(this->m_node[1]->GetDegreeOfFreedom(0))*DC_Scale;
  y2+=sol->GetSolutionValue(this->m_node[1]->GetDegreeOfFreedom(1))*DC_Scale;
  x3+=sol->GetSolutionValue(this->m_node[2]->GetDegreeOfFreedom(0))*DC_Scale;
  y3+=sol->GetSolutionValue(this->m_node[2]->GetDegreeOfFreedom(1))*DC_Scale;

  pDC->MoveTo(x1,y1);
  pDC->LineTo(x2,y2);
  pDC->LineTo(x3,y3);
  pDC->LineTo(x1,y1);

}
#endif




}} // end namespace itk::fem
