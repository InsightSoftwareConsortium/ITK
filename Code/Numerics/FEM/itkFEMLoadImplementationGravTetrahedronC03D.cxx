/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationGravTetrahedronC03D.cxx
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

#include "itkFEMElementTetrahedronC03D.h"
#include "itkFEMLoadGrav.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {



vnl_vector<Element::Float>
GravityLoad(const vnl_vector<Element::Float>& f, TetrahedronC03D::ConstPointer element)
{
  typedef Element::Float Float;

  vnl_vector<Float> GL(12), shapeF(4);
  vnl_matrix<Float> J(3,3);
  Float gxcom, gycom, gzcom, detJ;

  // Talk to Aljaz about that
  // Material properties !!!
  Float dense = 1;
  
  /**
   * Gaussian integration point - use the one-point integration
   * for tetrahedra (which is exact for linear elements).  Set
   * each shape function equal to 0.25 and evaluate from there.
   */
  Float x[3] = {0.25, 0.25, 0.25};

  /** Declares auxiliary variables */
  int p;

  /** Initialize load vector */
  GL.fill(0.0);

  /** Computes constants
    * theta should be in radians
    */
  gxcom = dense * f[0];
  gycom = dense * f[1];
  gzcom = dense * f[2];
  
  /** Carry out numerical integration */

  /** Computes the shape functions, derivatives and Jacobian */
  shapeF = element->ComputeShapeFunctionsAt(x);
  J = element->ComputeJacobianMatrixAt(x);
  detJ = element->JacobianMatrixDeterminant(J);

  /** Calculates loads and associate with element nodal points */
  for (int j=0; j<4; j++) {
    p = 3*j;

    GL[  p] += gxcom * shapeF[j] * detJ;
    GL[p+1] += gycom * shapeF[j] * detJ;
    GL[p+2] += gzcom * shapeF[j] * detJ;
  }

  return GL / 6;
}



/**
 * Handle LoadGrav in TetrahedronC03D element
 */
Element::LoadVectorType LoadGravImplementationTetrahedronC03D(TetrahedronC03D::ConstPointer element, Element::LoadElementPointer load)
{

  typedef Element::Float Float;
  LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*load);
  if ( !l0 ) throw FEMException(__FILE__, __LINE__, "FEM error");

  /**
   * Handle gravity loads
   */
  vnl_vector<Float> pt(2), f(2);

  pt.fill(0.0);

  /** Computes point at which gravity load acts on */
  for (int j=0; j < 8; j++)
  {
    pt[0] += element->m_node[j]->X;
    pt[1] += element->m_node[j]->Y;
    pt[2] += element->m_node[j]->Z;
  }
  pt = pt / 8;

  /** Gets gravity load */
  return GravityLoad(f = l0->Fg(pt),element);

}




}} // end namespace itk::fem
