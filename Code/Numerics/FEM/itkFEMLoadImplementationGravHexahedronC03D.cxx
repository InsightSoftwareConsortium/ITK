/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationGravHexahedronC03D.cxx
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

#include "itkFEMElementHexahedronC03D.h"
#include "itkFEMLoadGrav.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {



vnl_vector<Element::Float>
GravityLoad(const vnl_vector<Element::Float>& f, HexahedronC03D::ConstPointer element)
{
  // Added TS
  // cout << "GravityLoad()" << endl;
  // End

  typedef Element::Float Float;

  vnl_vector<Float> GL(24), shapeF(8);
  vnl_matrix<Float> J(3,3);
  Float gxcom, gycom, gzcom, detJ;

  // Talk to Aljaz about that
  // Material properties !!!
  Float dense = 1;
  
  /** Defines gaussian integration points */
  Float pt = 1.0 / sqrt(3.0);
  Float GPoints[][3] = {{-pt, -pt, pt}, {pt, -pt, pt}, {pt, pt, pt}, {-pt, pt, pt}, 
            {-pt, -pt, -pt}, {pt, -pt, -pt}, {pt, pt, -pt}, {-pt, pt, -pt}};

  /** Declares auxiliary variables */
  Float x[3];
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
  for (int i=0; i<8; i++) {
    
    /** Gets integration point */
    x[0] = GPoints[i][0];
    x[1] = GPoints[i][1];
    x[2] = GPoints[i][2];

    /** Computes the shape function derivatives at integration point */
    shapeF = element->ComputeShapeFunctionsAt(x);

    /** Computes the Jacobian matrix and its determinant
      * at the i-th integration point
      */
    J = element->ComputeJacobianMatrixAt(x);
    detJ = element->JacobianMatrixDeterminant(J);

    /** Calculates loads and associate with element nodal points */
    for (int j=0; j<8; j++) {
      p = 3*j;

      GL[  p] += gxcom * shapeF[j] * detJ;
      GL[p+1] += gycom * shapeF[j] * detJ;
      GL[p+2] += gzcom * shapeF[j] * detJ;
    }
  }

  // Added TS
  // cout << GL << endl;
  // cout << "end GravityLoad" << endl;
  // End

  return GL;
}



/**
 * Handle LoadGrav in HexahedronC03D element
 */
Element::LoadVectorType LoadGravImplementationHexahedronC03D(HexahedronC03D::ConstPointer element, Element::LoadElementPointer load)
{
  // Added TS
  // cout << "LoadGravImplementationHexahedronC03D()" << endl;
  // End

  typedef Element::Float Float;
  LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*load);
  if ( !l0 ) throw FEMException(__FILE__, __LINE__, "FEM error");

  /**
   * Handle gravity loads
   */
  vnl_vector<Float> pt(3), f(3);

  pt.fill(0.0);

  /** Computes point at which gravity load acts on */
  for (int j=0; j < 8; j++)
  {
    pt[0] += element->m_node[j]->X;
    pt[1] += element->m_node[j]->Y;
    pt[2] += element->m_node[j]->Z;
  }
  pt = pt / 8;

  // Added TS
  // cout << "end LoadGravImplementationHexahedronC03D" << endl;
  // End

  /** Gets gravity load */
  return GravityLoad(f = l0->Fg(pt),element);

}




}} // end namespace itk::fem
