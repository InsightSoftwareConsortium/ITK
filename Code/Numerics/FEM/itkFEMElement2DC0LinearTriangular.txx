/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0LinearTriangular.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DC0LinearTriangular_txx
#define __itkFEMElement2DC0LinearTriangular_txx

#include "vnl/vnl_math.h"

namespace itk {
namespace fem {


template<unsigned int VNumberOfDegreesOfFreedomPerNode>
const double
Element2DC0LinearTriangular<VNumberOfDegreesOfFreedomPerNode>
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

template<unsigned int VNumberOfDegreesOfFreedomPerNode>
const unsigned int 
Element2DC0LinearTriangular<VNumberOfDegreesOfFreedomPerNode>
::Nip[6]=
{
  0,1,3,3,6,7
};



template<unsigned int VNumberOfDegreesOfFreedomPerNode>
void
Element2DC0LinearTriangular<VNumberOfDegreesOfFreedomPerNode>
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




template<unsigned int VNumberOfDegreesOfFreedomPerNode>
unsigned int
Element2DC0LinearTriangular<VNumberOfDegreesOfFreedomPerNode>
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // FIXME: range checking

  // default integration order
  if (order==0) { order=DefaultIntegrationOrder; }

  return Nip[order];
}




template<unsigned int VNumberOfDegreesOfFreedomPerNode>
Element2DC0LinearTriangular<VNumberOfDegreesOfFreedomPerNode>::VectorType
Element2DC0LinearTriangular<VNumberOfDegreesOfFreedomPerNode>
::ShapeFunctions( const VectorType& pt ) const
{
  // Linear triangular element has 3 shape functions
  VectorType shapeF(3);

  // Shape functions are equal to coordinates
  shapeF=pt;

  return shapeF;
}




template<unsigned int VNumberOfDegreesOfFreedomPerNode>
void
Element2DC0LinearTriangular<VNumberOfDegreesOfFreedomPerNode>
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




template<unsigned int VNumberOfDegreesOfFreedomPerNode>
Element2DC0LinearTriangular<VNumberOfDegreesOfFreedomPerNode>::Float
Element2DC0LinearTriangular<VNumberOfDegreesOfFreedomPerNode>
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




template<unsigned int VNumberOfDegreesOfFreedomPerNode>
void
Element2DC0LinearTriangular<VNumberOfDegreesOfFreedomPerNode>
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
template<unsigned int VNumberOfDegreesOfFreedomPerNode>
void
Element2DC0LinearTriangular<VNumberOfDegreesOfFreedomPerNode>
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




#ifdef _MSC_VER
// Declare a static dummy function to prevent a MSVC 6.0 SP5 from crashing.
// I have no idea why things don't work when this is not declared, but it
// looks like this declaration makes compiler forget about some of the
// troubles it has with templates.
static void Dummy( void );
#endif // #ifdef _MSC_VER

}} // end namespace itk::fem

#endif // #ifndef __itkFEMElement2DC0LinearTriangular_txx
