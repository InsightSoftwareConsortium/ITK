/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationGrav_TriC02D.cxx
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

#include "itkFEMElementTriC02D.h"
#include "itkFEMLoadGrav.h"

namespace itk {
namespace fem {




typedef Element::Float Float;

/**
 * Computes the gravity load contribution to a QuadC02D element
 */
vnl_vector<Float>
GravityLoad(const vnl_vector<Float>& f, TriC02D::ConstPointer element)
{
  vnl_vector<Float> GL(6), shapeF(3);
  vnl_matrix<Float> J(3,3);
  Float gxcom, gycom, detJ;

  // Talk to Aljaz about that
  // Material properties !!!
  Float dense = 1;
  
  /* Defines gaussian integration points */
  Float GPoints[][3] = {{2./3.,1./6.,1./6.}, {1./6.,2./3.,1./6.}, {1./6.,1./6.,2./3.}};

  /* Declares auxiliary variables */
  Float x[3], detJ2;
  int p;

  /* Initialize load vector */
  GL.fill(0.0);

  /* 
   * Computes constants
   * theta should be in radians
   */
  gxcom = dense * f[0];
  gycom = dense * f[1];

  x[0] = x[1] = x[2] = 0.0;
  J = element->ComputeJacobianMatrixAt(x);
  detJ = element->JacobianMatrixDeterminant(J);
  detJ2 = detJ / 2.;
  
  /* Carry out numerical integration */
  for (int i=0; i<3; i++) {
    
    /* Gets integration point */
    x[0] = GPoints[i][0];
    x[1] = GPoints[i][1];
    x[2] = GPoints[i][2];

    /* Computes the shape function derivatives at integration point */
    shapeF = element->ComputeShapeFunctionsAt(x);

    /* Calculates loads and associate with element nodal points */
    for (int j=0; j<3; j++) {
      p = j<<1;

      GL[  p] += gxcom * shapeF[j] * detJ2;
      GL[p+1] += gycom * shapeF[j] * detJ2;
    }
  }

  return GL / 3.;
}




/**
 * Handle LoadGrav in TriC02D element
 */
Element::LoadVectorType LoadGravImplementation(TriC02D::ConstPointer element, LoadElement::Pointer load)
{

  typedef Element::Float Float;
  LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*load);
  if ( !l0 ) throw;

  vnl_vector<Float> pt(2), f(2);

  /* Computes point at which gravity load acts on */
  pt[0] = (element->m_node1->X + element->m_node2->X + element->m_node3->X)/3;
  pt[1] = (element->m_node1->Y + element->m_node2->Y + element->m_node3->Y)/3;

  /* Gets gravity load */
  return GravityLoad(f = l0->Fg(pt),element);

}




}} // end namespace itk::fem
