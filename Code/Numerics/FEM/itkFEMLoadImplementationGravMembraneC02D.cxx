/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationGravMembraneC02D.cxx
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

#include "itkFEMElementMembraneC02D.h"
#include "itkFEMLoadGrav.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {

vnl_vector<Element::Float>
GravityLoad(const vnl_vector<Element::Float>& f, MembraneC02D::ConstPointer element)
{
  typedef Element::Float Float;

  vnl_vector<Float> GL(8), shapeF(4);
  vnl_matrix<Float> J(2,2);
  Float gxcom, gycom, detJ;

  // Talk to Aljaz about that
  // Material properties !!!
  Float dense = 1;
  
  /** Defines gaussian integration points */
  Float pt = 1.0 / sqrt(3.0);
  Float GPoints[][2] = {{-pt, -pt}, {pt, -pt}, {pt, pt}, {-pt, pt}};

  /** Declares auxiliary variables */
  Float x[2];
  int p;

  /** Initialize load vector */
  GL.fill(0.0);

  /** Computes constants
    * theta should be in radians
    */
  gxcom = dense * f[0];
  gycom = dense * f[1];
  
  /** Carry out numerical integration */
  for (int i=0; i<4; i++) {
    
    /** Gets integration point */
    x[0] = GPoints[i][0];
    x[1] = GPoints[i][1];

    /** Computes the shape function derivatives at integration point */
    shapeF = element->ComputeShapeFunctionsAt(x);

    /** Computes the Jacobian matrix and its determinant
      * at the i-th integration point
      */
    J = element->ComputeJacobianMatrixAt(x);
    detJ = element->JacobianMatrixDeterminant(J);

    /** Calculates loads and associate with element nodal points */
    for (int j=0; j<4; j++) {
      p = j<<1;

      GL[  p] += gxcom * shapeF[j] * detJ;
      GL[p+1] += gycom * shapeF[j] * detJ;
    }
  }

  return GL;
}




/**
 * Return the force vector for MembraneC02D element
 */
Element::LoadVectorType
LoadGravImplementationMembraneC02D
(MembraneC02D::ConstPointer element, Element::LoadElementPointer load)
{
    
  typedef Element::Float Float;
  LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*load);
  if ( !l0 ) throw FEMException(__FILE__, __LINE__, "FEM error");
  // for gravity loads
  vnl_vector<Float> pt(4,0.0), f(2,0.0);
  Solution::ConstPointer   S=l0->GetSolution();
  // Computes point at which gravity load acts on 
  // and also gives the current displacement
  pt[0] = (element->m_node[0]->X + element->m_node[1]->X + element->m_node[2]->X + element->m_node[3]->X)/4;
  pt[1] = (element->m_node[0]->Y + element->m_node[1]->Y + element->m_node[2]->Y + element->m_node[3]->Y)/4;
  pt[2] = ( S->GetSolutionValue(element->GetDegreeOfFreedom(0)) + 
            S->GetSolutionValue(element->GetDegreeOfFreedom(2)) +
            S->GetSolutionValue(element->GetDegreeOfFreedom(4)) +
            S->GetSolutionValue(element->GetDegreeOfFreedom(6)) ) / 4;
 pt[3] = ( S->GetSolutionValue(element->GetDegreeOfFreedom(1)) + 
            S->GetSolutionValue(element->GetDegreeOfFreedom(3)) +
            S->GetSolutionValue(element->GetDegreeOfFreedom(5)) +
            S->GetSolutionValue(element->GetDegreeOfFreedom(7)) ) / 4;
/* */
  f=l0->Fg(pt);    

  return GravityLoad(f , element);
 
}




}} // end namespace itk::fem
