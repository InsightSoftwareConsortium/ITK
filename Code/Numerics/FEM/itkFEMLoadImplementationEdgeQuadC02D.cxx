/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationEdgeQuadC02D.cxx
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
#include "itkFEMLoadEdge.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {



/**
 * Computes the normal and tangential contribution
 * acting on one edge of a QuadC02D element
 */
typedef Element::Float Float;
vnl_vector<Float>
EdgeLoad(Float Pn1, Float Pn2, Float Pt1, Float Pt2,
           Float  x1, Float  y1, Float  x2, Float  y2,
           int n1, int n2,
           QuadC02D::ConstPointer element)
{
  vnl_vector<Float> GL(8), shapeF(4);
  vnl_matrix<Float> J(2,2), shapeD(4,2);

  /** Defines gaussian integration points */
  Float pt = 1.0 / sqrt(3.0);
  Float GPoints[2] = {-pt, pt};

  /** Declares auxiliary variables */
  double x[2], pgash1, pgash2, dgash1, dgash2, pxcom, pycom;

  /** Initializes load vector */
  GL.fill(0.0);

  /** Gaussian numerical integration */
  for (int j=0; j<2; j++) {
    /** Gets Gaussian integration points */
    x[0] = GPoints[j];
    x[1] = 0.0;

    /** Computes the shape function derivatives at
      * integration point
      */
    shapeF = element->ComputeShapeFunctionsAt(x);

    /** Computes the shape function derivatives at
      * integration point
      */
    shapeD = element->ComputeShapeFunctionDerivativesAt(x);

    /** Computes components of the equivalent nodal loads */
    pgash1 = (Pn1 * shapeF(0)) + (Pn2 * shapeF(1));
    pgash2 = (Pt1 * shapeF(0)) + (Pt2 * shapeF(1));
    dgash1 = (x1 * shapeD(0,0)) + (x2 * shapeD(1,0));
    dgash2 = (y1 * shapeD(0,0)) + (y2 * shapeD(1,0));

    pxcom = (dgash1 * pgash2) - (dgash2 * pgash1);
    pycom = (dgash1 * pgash1) + (dgash2 * pgash2);

    GL[      n1 << 1] += shapeF(n1) * pxcom;
    GL[(n1 << 1) + 1] += shapeF(n1) * pycom;
    GL[      n2 << 1] += shapeF(n2) * pxcom;
    GL[(n2 << 1) + 1] += shapeF(n2) * pycom;
  }

  return GL;
}



/**
 * Handle LoadEdge in QuadC02D element
 */
Element::LoadVectorType LoadEdgeImplementationQuadC02D(QuadC02D::ConstPointer element, Element::LoadElementPointer load)
{

  typedef Element::Float Float;
  LoadEdge::Pointer l0=dynamic_cast<LoadEdge*>(&*load);
  if ( !l0 ) throw FEMException(__FILE__, __LINE__, "FEM error");

  /**
   * Handle edge loads
   */
  int n1, n2;
  Float Pn1, Pt1, Pn2, Pt2;
  Float x1,x2, y1,y2;
  
  /** Get node local numbers and coordinates */
  element->GetNode(l0->m_Edge, n1, n2);
  element->GetNodeCoordinates(n1, x1, y1);
  element->GetNodeCoordinates(n2, x2, y2);

  /** Gets normal and tangential force acting on the edge */
  /** Normal forces */ 
  Pn1 = l0->m_Force[0][0];
  Pn2 = l0->m_Force[1][0];

  /** Tangential forces */ 
  Pt1 = l0->m_Force[0][1];
  Pt2 = l0->m_Force[1][1];

  return EdgeLoad(Pn1, Pt1, Pn2, Pt2, x1, y1, x2, y2, n1, n2, element);

}




}} // end namespace itk::fem
