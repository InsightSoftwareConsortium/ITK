/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationGrav_QuadC02D.cxx
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

#include "itkFEMElementQuadC02D.h"
#include "itkFEMLoadGrav.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {



vnl_vector<Element::Float>
GravityLoad(const vnl_vector<Element::Float>& f, QuadC02D::ConstPointer element)
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
 * Handle LoadGrav in QuadC02D element
 */
Element::LoadVectorType LoadGravImplementation(QuadC02D::ConstPointer element, Element::LoadElementPointer load)
{

  typedef Element::Float Float;
  LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*load);
  if ( !l0 ) throw;

  /**
   * Handle gravity loads
   */
  vnl_vector<Float> pt(2), f(2);

  /** Computes point at which gravity load acts on */
  pt[0] = (element->m_node1->X + element->m_node2->X + element->m_node3->X + element->m_node4->X)/4;
  pt[1] = (element->m_node1->Y + element->m_node2->Y + element->m_node3->Y + element->m_node4->Y)/4;

  /** Gets gravity load */
  return GravityLoad(f = l0->Fg(pt),element);

}




}} // end namespace itk::fem
